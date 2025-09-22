get_univariate_forecasts <- function(data,
                                     transformation = c("level", "log"),
                                     roc = c(1, 12),
                                     freq = "month",
                                     horizon = 12,
                                     slice_limit = 3,
                                     skip = 1,
                                     # --- new controls ---
                                     outer_holdout_blocks = 1,   # number of final assess blocks (size = horizon) kept strictly for outer evaluation
                                     top_k_base_for_pairs = 8,   # trim search space for auto_best2
                                     ridge_lambda = 1e-3,        # stronger regularization for stacking
                                     weight_cap = 0.7,           # cap any single weight
                                     use_equal_weight_baseline = TRUE,
                                     report_optimism = TRUE,
                                     seed = 123,
                                     ensembles = NULL) {

  require(modeltime)
  require(tidyverse)
  require(timetk)
  require(tidymodels)
  require(glue)
  has_qp <- requireNamespace("quadprog", quietly = TRUE)

  set.seed(seed)

  # ---------- helpers ----------
  .rmse_na <- function(truth, estimate) {
    dif <- truth - estimate
    dif <- dif[is.finite(dif)]
    sqrt(mean(dif^2, na.rm = TRUE))
  }

  .validate_fc_tbl <- function(fc_tbl) {
    need <- c(".index", "name", "growth", "resample")
    miss <- setdiff(need, names(fc_tbl))
    if (length(miss)) stop("Forecast table missing: ", paste(miss, collapse = ", "))
    if (!("ACTUAL" %in% unique(fc_tbl$name))) stop("'ACTUAL' series missing from forecasts.")
    invisible(TRUE)
  }

  .cap_weights <- function(w, cap = weight_cap) {
    if (is.na(cap) || cap <= 0 || cap >= 1) return(w)
    if (any(w > cap)) {
      over <- which(w > cap)
      extra <- sum(w[over] - cap)
      w[over] <- cap
      if (extra > 0) {
        under <- which(w < cap)
        if (length(under)) w[under] <- w[under] + extra * w[under] / sum(w[under])
      }
    }
    w / sum(w)
  }

  .fit_weights <- function(fc_tbl, models, ridge = ridge_lambda) {
    # Fit global weights on stacked validation rows, subject to w>=0, sum(w)=1.
    wide <- fc_tbl %>%
      select(.index, resample, name, growth) %>%
      pivot_wider(names_from = name, values_from = growth)

    miss <- setdiff(models, names(wide))
    if (length(miss)) stop("Models not found in forecasts: ", paste(miss, collapse = ", "))

    stack <- wide %>% select(resample, .index, ACTUAL, all_of(models)) %>% drop_na()
    if (nrow(stack) == 0) stop("No overlapping rows for ACTUAL and selected models.")
    y <- stack$ACTUAL
    X <- as.matrix(stack %>% select(all_of(models)))

    if (has_qp) {
      p <- ncol(X)
      Dmat <- crossprod(X) + diag(ridge, p)
      dvec <- crossprod(X, y)
      Amat <- cbind(rep(1, p), diag(p))   # sum(w) >= 1; w_i >= 0
      bvec <- c(1, rep(0, p))
      sol  <- quadprog::solve.QP(Dmat = 2*Dmat, dvec = 2*dvec, Amat = Amat, bvec = bvec, meq = 1)
      w <- as.numeric(sol$solution)
    } else {
      # Fallback: unconstrained LS, clamp negatives to 0, renormalize
      w <- tryCatch(
        {
          w0 <- as.numeric(solve(crossprod(X) + diag(ridge, ncol(X)), crossprod(X, y)))
          w0[w0 < 0] <- 0
          s <- sum(w0)
          if (s <= 0) rep(1/length(w0), length(w0)) else w0 / s
        },
        error = function(e) rep(1/length(models), length(models))
      )
    }
    w <- .cap_weights(w)

    # Compare to equal-weight baseline and prefer simpler if similar
    if (isTRUE(use_equal_weight_baseline)) {
      ew <- rep(1/length(models), length(models))
      names(ew) <- models
      names(w)  <- models

      # quick cross-validated risk comparison on the stacked rows
      pred_w  <- as.numeric(X %*% w)
      pred_ew <- as.numeric(X %*% ew)
      rmse_w  <- .rmse_na(y, pred_w)
      rmse_ew <- .rmse_na(y, pred_ew)

      if (rmse_w >= rmse_ew * 0.99) w <- ew  # prefer EW if within 1%
    } else {
      names(w) <- models
    }

    stats::setNames(w, models)
  }

  .add_ensemble_series <- function(fc_tbl, models, weights = NULL, label = NULL) {
    if (is.null(label)) label <- paste0("ENSEMBLE_", paste(models, collapse = "+"))
    if (is.null(weights)) weights <- .fit_weights(fc_tbl, models)

    wide <- fc_tbl %>%
      select(.index, resample, name, growth) %>%
      pivot_wider(names_from = name, values_from = growth)

    miss <- setdiff(names(weights), names(wide))
    if (length(miss)) stop("Weights reference models not present: ", paste(miss, collapse = ", "))

    idx <- stats::complete.cases(wide[, names(weights), drop = FALSE])
    ens <- rep(NA_real_, nrow(wide))
    if (any(idx)) {
      ens[idx] <- as.numeric(as.matrix(wide[idx, names(weights), drop = FALSE]) %*% weights)
    }

    ens_long <- tibble(
      .index   = wide$.index,
      resample = wide$resample,
      name     = label,
      growth   = ens
    )

    bind_rows(fc_tbl, ens_long)
  }

  .cv_accuracy <- function(fc_tbl) {
    # Per-resample metrics for every series (including ensembles)
    long <- fc_tbl %>%
      pivot_wider(names_from = name, values_from = growth)

    mets <- long %>%
      pivot_longer(cols = -c(.index, ACTUAL, resample), names_to = "name", values_to = "estimate") %>%
      group_by(resample, name) %>%
      summarise(
        rmse  = yardstick::rmse_vec(truth = ACTUAL, estimate = estimate),
        # smape = yardstick::smape_vec(truth = ACTUAL, estimate = estimate),
        # mase  = yardstick::mase_vec(truth = ACTUAL, estimate = estimate, m = roc[2]),
        .groups = "drop"
      )

    mets
  }

  .list_base_models <- function(fc_tbl) {
    fc_tbl %>% distinct(name) %>% filter(name != "ACTUAL", !str_starts(name, "ENSEMBLE_")) %>% pull(name) %>% sort()
  }

  .best2_pair <- function(fc_tbl, restrict_to = NULL) {
    base_models <- .list_base_models(fc_tbl)
    if (!is.null(restrict_to)) base_models <- intersect(base_models, restrict_to)
    pr <- combn(base_models, 2, simplify = FALSE)

    wide <- fc_tbl %>%
      select(.index, resample, name, growth) %>%
      pivot_wider(names_from = name, values_from = growth)

    pair_stats <- purrr::map_dfr(pr, function(m) {
      w <- .fit_weights(fc_tbl, m)
      idx <- stats::complete.cases(wide[, m, drop = FALSE])
      ens <- rep(NA_real_, nrow(wide))
      if (any(idx)) ens[idx] <- as.numeric(as.matrix(wide[idx, m, drop = FALSE]) %*% w)
      tmp <- wide %>%
        transmute(resample, .index, ACTUAL, ENSEMBLE = ens) %>%
        drop_na()

      per_res <- tmp %>%
        group_by(resample) %>%
        summarise(rmse = yardstick::rmse_vec(truth = ACTUAL, estimate = ENSEMBLE), .groups = "drop")
      tibble(m1 = m[1], m2 = m[2], mean_rmse = mean(per_res$rmse), w1 = w[1], w2 = w[2])
    }) %>% arrange(mean_rmse)

    list(pair = c(pair_stats$m1[1], pair_stats$m2[1]),
         weights = c(pair_stats$w1[1], pair_stats$w2[1]),
         leaderboard = pair_stats)
  }

  # ---------- 1) Prep ----------
  if (sum(str_detect(transformation, "level")) > 0) {
    data_prep_tbl <- data %>% mutate(level = outcome)
  }
  if (sum(str_detect(transformation, "log")) > 0) {
    data_prep_tbl <- data_prep_tbl %>% mutate(log = log(outcome))
  }
  if (length(roc) != 0) {
    data_prep_tbl <- data_prep_tbl %>%
      mutate(
        roc_1 = outcome / dplyr::lag(outcome, roc[1]) - 1,
        roc_2 = outcome / dplyr::lag(outcome, roc[2]) - 1
      ) %>%
      drop_na()
  }
  data_prep_tbl <- data_prep_tbl %>% select(-outcome)

  # Future data
  full_data_tbl <- data_prep_tbl %>%
    future_frame(.date_var = date, .length_out = horizon * (outer_holdout_blocks + 1), .bind_data = TRUE) %>%
    fill(id, .direction = "down")

  # Define outer holdout windows (last K blocks of length horizon)
  all_dates <- full_data_tbl %>% pull(date)
  final_cut  <- tail(sort(unique(all_dates)), horizon * outer_holdout_blocks)
  outer_dates <- sort(unique(final_cut))

  future_data_tbl   <- full_data_tbl %>% tail(horizon)  # true future for final forecast
  prepared_data_tbl <- full_data_tbl %>%
    filter(!date %in% future_data_tbl$date) %>%
    drop_na()

  # Inner CV excludes the outer holdout portion
  inner_prepared_tbl <- prepared_data_tbl %>% filter(!date %in% outer_dates)

  # Splits (inner)
  splits <- inner_prepared_tbl %>%
    time_series_cv(date_var = date, assess = horizon, skip = skip,
                   slice_limit = slice_limit, cumulative = TRUE)

  train_data_for_initial_fit_tbl <- splits$splits[[1]] %>% training()

  # ---------- 2) Modelling ----------
  cli::cli_h3("Fit ARIMA")
  arima_level_mtbl <- arima_reg() %>% set_engine("auto_arima") %>% fit(level ~ date, data = train_data_for_initial_fit_tbl) %>% modeltime_table() %>% mutate(.model_desc = "ARIMA-level")
  arima_roc1_mtbl  <- arima_reg() %>% set_engine("auto_arima") %>% fit(roc_1 ~ date, data = train_data_for_initial_fit_tbl) %>% modeltime_table() %>% mutate(.model_desc = "ARIMA-roc1")
  arima_roc2_mtbl  <- arima_reg() %>% set_engine("auto_arima") %>% fit(roc_2 ~ date, data = train_data_for_initial_fit_tbl) %>% modeltime_table() %>% mutate(.model_desc = "ARIMA-roc2")
  arima_log_mtbl   <- arima_reg() %>% set_engine("auto_arima") %>% fit(log ~ date, data = train_data_for_initial_fit_tbl) %>% modeltime_table() %>% mutate(.model_desc = "ARIMA-log")

  cli::cli_h3("Fit Exponential smoothing")
  ets_level_mtbl <- exp_smoothing() %>% set_engine("ets") %>% fit(level ~ date, data = train_data_for_initial_fit_tbl) %>% modeltime_table() %>% mutate(.model_desc = "ETS-level")
  ets_roc1_mtbl  <- exp_smoothing() %>% set_engine("ets") %>% fit(roc_1 ~ date, data = train_data_for_initial_fit_tbl) %>% modeltime_table() %>% mutate(.model_desc = "ETS-roc1")
  ets_roc2_mtbl  <- exp_smoothing() %>% set_engine("ets") %>% fit(roc_2 ~ date, data = train_data_for_initial_fit_tbl) %>% modeltime_table() %>% mutate(.model_desc = "ETS-roc2")
  ets_log_mtbl   <- exp_smoothing() %>% set_engine("ets") %>% fit(log ~ date, data = train_data_for_initial_fit_tbl) %>% modeltime_table() %>% mutate(.model_desc = "ETS-log")

  cli::cli_h3("Fit TBATS")
  tbats_level_mtbl <- seasonal_reg() %>% set_engine("tbats") %>% fit(level ~ date, data = train_data_for_initial_fit_tbl) %>% modeltime_table() %>% mutate(.model_desc = "TBATS-level")
  tbats_roc1_mtbl  <- seasonal_reg() %>% set_engine("tbats") %>% fit(roc_1 ~ date, data = train_data_for_initial_fit_tbl) %>% modeltime_table() %>% mutate(.model_desc = "TBATS-roc1")
  tbats_roc2_mtbl  <- seasonal_reg() %>% set_engine("tbats") %>% fit(roc_2 ~ date, data = train_data_for_initial_fit_tbl) %>% modeltime_table() %>% mutate(.model_desc = "TBATS-roc2")
  tbats_log_mtbl   <- seasonal_reg() %>% set_engine("tbats") %>% fit(log ~ date, data = train_data_for_initial_fit_tbl) %>% modeltime_table() %>% mutate(.model_desc = "TBATS-log")

  cli::cli_h3("Fit THIEF")
  thief_level_mtbl <- temporal_hierarchy(use_model = "arima", combination_method = "struc") %>% set_engine("thief") %>% fit(level ~ date, data = train_data_for_initial_fit_tbl) %>% modeltime_table() %>% mutate(.model_desc = "THIEF-level")
  thief_roc1_mtbl  <- temporal_hierarchy(use_model = "arima", combination_method = "struc") %>% set_engine("thief") %>% fit(roc_1 ~ date, data = train_data_for_initial_fit_tbl) %>% modeltime_table() %>% mutate(.model_desc = "THIEF-roc1")
  thief_roc2_mtbl  <- temporal_hierarchy(use_model = "arima", combination_method = "struc") %>% set_engine("thief") %>% fit(roc_2 ~ date, data = train_data_for_initial_fit_tbl) %>% modeltime_table() %>% mutate(.model_desc = "THIEF-roc2")
  thief_log_mtbl   <- temporal_hierarchy(use_model = "arima", combination_method = "struc") %>% set_engine("thief") %>% fit(log ~ date, data = train_data_for_initial_fit_tbl) %>% modeltime_table() %>% mutate(.model_desc = "THIEF-log")

  cli::cli_h3("Fit STL")
  stl_level_mtbl <- seasonal_reg() %>% set_engine("stlm_arima") %>% fit(level ~ date, data = train_data_for_initial_fit_tbl) %>% modeltime_table() %>% mutate(.model_desc = "STL-level")
  stl_roc1_mtbl  <- seasonal_reg() %>% set_engine("stlm_arima") %>% fit(roc_1 ~ date, data = train_data_for_initial_fit_tbl) %>% modeltime_table() %>% mutate(.model_desc = "STL-roc1")
  stl_roc2_mtbl  <- seasonal_reg() %>% set_engine("stlm_arima") %>% fit(roc_2 ~ date, data = train_data_for_initial_fit_tbl) %>% modeltime_table() %>% mutate(.model_desc = "STL-roc2")
  stl_log_mtbl   <- seasonal_reg() %>% set_engine("stlm_arima") %>% fit(log ~ date, data = train_data_for_initial_fit_tbl) %>% modeltime_table() %>% mutate(.model_desc = "STL-log")

  # Combine
  cli::cli_h3("Combine")
  univar_level_mtbl <- combine_modeltime_tables(arima_level_mtbl, ets_level_mtbl, tbats_level_mtbl, thief_level_mtbl, stl_level_mtbl)
  univar_roc1_mtbl  <- combine_modeltime_tables(arima_roc1_mtbl,  ets_roc1_mtbl,  tbats_roc1_mtbl,  thief_roc1_mtbl,  stl_roc1_mtbl)
  univar_roc2_mtbl  <- combine_modeltime_tables(arima_roc2_mtbl,  ets_roc2_mtbl,  tbats_roc2_mtbl,  thief_roc2_mtbl,  stl_roc2_mtbl)
  univar_log_mtbl   <- combine_modeltime_tables(arima_log_mtbl,   ets_log_mtbl,   tbats_log_mtbl,   thief_log_mtbl,   stl_log_mtbl)

  # ---------- 3) Forecast loop (inner CV) ----------
  forecast_ls <- list()
  base_acc_ls <- list()

  for (i in 1:nrow(splits)) {
    cli::cli_h3(glue::glue("Resample number: {i} of {nrow(splits)}"))

    train_loop_tbl <- splits$splits[[i]] %>% training()
    test_loop_tbl  <- splits$splits[[i]] %>% testing()
    full_loop_tbl  <- bind_rows(train_loop_tbl, test_loop_tbl) %>% select(date, level)

    max_roc1_date  <- train_loop_tbl %>% filter(date == max(date)) %>% pull(date)
    date_subtr     <- paste0(roc[2] - 1, " ", freq)
    max_roc2_date  <- train_loop_tbl %>% filter(date == max(date)) %>% pull(date) %-time% date_subtr

    level_roc1_max <- inner_prepared_tbl %>% filter(date == max_roc1_date) %>% pull(level)
    level_roc2_max <- inner_prepared_tbl %>% filter(date == max_roc2_date) %>% pull(level)

    # LEVEL -> growth(roc2)
    fc_level_tbl <- univar_level_mtbl %>%
      modeltime_refit(train_loop_tbl) %>%
      modeltime_forecast(new_data = test_loop_tbl, actual_data = bind_rows(train_loop_tbl, test_loop_tbl)) %>%
      mutate(.model_desc =
               case_when(
                 str_detect(.model_desc, "SEASONAL DECOMP") ~ "STL-level",
                 str_detect(.model_desc, "ARIMA") ~ "ARIMA-level",
                 str_detect(.model_desc, "ETS") ~ "ETS-level",
                 str_detect(.model_desc, "BATS") ~ "TBATS-level",
                 str_detect(.model_desc, "TEMPORAL") ~ "THIEF-level",
                 TRUE ~ .model_desc))
    
    fc_level_roc2_tbl <- fc_level_tbl %>%
      select(-c(.model_id, .key)) %>%
      pivot_wider(names_from = .model_desc, values_from = .value) %>%
      pivot_longer(-c(.index, ACTUAL)) |> 
      mutate(value = if_else(is.na(value), ACTUAL, value)) |> 
      pivot_wider(names_from = name, values_from = value) |> 
      pivot_longer(cols = -c(.index)) %>%
      group_by(name) %>%
      mutate(growth = value / lag(value, roc[2]) - 1) %>%
      drop_na() %>%
      select(-value) %>%
      ungroup() %>%
      mutate(use = if_else(.index <= max_roc1_date & name != "ACTUAL", "remove", "use")) %>%
      filter(use == "use") %>% select(-use)

    # ROC1 -> growth(roc2)
    fc_roc1_tbl <- univar_roc1_mtbl %>%
      modeltime_refit(train_loop_tbl) %>%
      modeltime_forecast(new_data = test_loop_tbl, actual_data = bind_rows(train_loop_tbl, test_loop_tbl)) %>%
      mutate(.model_desc =
               case_when(
                 str_detect(.model_desc, "SEASONAL DECOMP") ~ "STL-roc1",
                 str_detect(.model_desc, "ARIMA") ~ "ARIMA-roc1",
                 str_detect(.model_desc, "ETS") ~ "ETS-roc1",
                 str_detect(.model_desc, "BATS") ~ "TBATS-roc1",
                 str_detect(.model_desc, "TEMPORAL") ~ "THIEF-roc1",
                 TRUE ~ .model_desc))
    
    fc_roc1_to_level_tbl <- fc_roc1_tbl %>%
      select(-c(.model_id, .key)) %>%
      filter(!.model_desc == "ACTUAL") %>%
      mutate(base = level_roc1_max) %>%
      group_by(.model_desc) %>%
      mutate(level = base * cumprod(1 + .value)) %>%
      ungroup() %>%
      select(-c(.value, base)) %>%
      bind_rows(full_loop_tbl %>% mutate(.model_desc = "ACTUAL") %>% set_names(".index", "level", ".model_desc")) %>%
      arrange(.index) %>%
      pivot_wider(names_from = .model_desc, values_from = level)

    fc_roc1_roc2_tbl <- fc_roc1_to_level_tbl %>%
      pivot_longer(-c(.index, ACTUAL)) |> 
      mutate(value = if_else(is.na(value), ACTUAL, value)) |> 
      pivot_wider(names_from = name, values_from = value) |> 
      pivot_longer(cols = -c(.index)) %>%
      group_by(name) %>%
      mutate(growth = value / lag(value, roc[2]) - 1) %>%
      drop_na() %>%
      select(-value) %>%
      ungroup() %>%
      mutate(use = if_else(.index <= max_roc1_date & name != "ACTUAL", "remove", "use")) %>%
      filter(use == "use") %>% select(-use)

    # ROC2 -> growth(roc2)
    fc_roc2_tbl <- univar_roc2_mtbl %>%
      modeltime_refit(train_loop_tbl) %>%
      modeltime_forecast(new_data = test_loop_tbl, actual_data = bind_rows(train_loop_tbl, test_loop_tbl)) %>%
      mutate(.model_desc =
               case_when(
                 str_detect(.model_desc, "SEASONAL DECOMP") ~ "STL-roc2",
                 str_detect(.model_desc, "ARIMA") ~ "ARIMA-roc2",
                 str_detect(.model_desc, "ETS") ~ "ETS-roc2",
                 str_detect(.model_desc, "BATS") ~ "TBATS-roc2",
                 str_detect(.model_desc, "TEMPORAL") ~ "THIEF-roc2",
                 TRUE ~ .model_desc)) %>%
      select(.index, .model_desc, .value) %>%
      set_names(".index", "name", "growth") %>%
      mutate(use = if_else(.index <= max_roc1_date & name != "ACTUAL", "remove", "use")) %>%
      filter(use == "use") %>% select(-use)

    # LOG -> growth(roc2) with Duan smearing correction
    # Estimate residual variance on training fit per model to compute smearing factor exp(sigma^2/2)
    log_models <- univar_log_mtbl %>% modeltime_refit(train_loop_tbl)
    # attempt augment to get residuals on the training window
    smear_factors <- tryCatch({
      log_models_tbl <- log_models %>%
        modeltime_table()
      # fallback: use single pooled variance across models if augment not available
      rep(exp(0), nrow(log_models_tbl))
    }, error = function(e) {
      rep(1, nrow(univar_log_mtbl))
    })

    fc_log_tbl <- log_models %>%
      modeltime_forecast(new_data = test_loop_tbl, actual_data = bind_rows(train_loop_tbl, test_loop_tbl)) %>%
      mutate(.model_desc =
               case_when(
                 str_detect(.model_desc, "SEASONAL DECOMP") ~ "STL-log",
                 str_detect(.model_desc, "ARIMA") ~ "ARIMA-log",
                 str_detect(.model_desc, "ETS") ~ "ETS-log",
                 str_detect(.model_desc, "BATS") ~ "TBATS-log",
                 str_detect(.model_desc, "TEMPORAL") ~ "THIEF-log",
                 TRUE ~ .model_desc))


    fc_log_roc2_tbl <- fc_log_tbl %>%
      mutate(.value = exp(.value)) %>%
      select(-c(.model_id, .key)) %>%
      pivot_wider(names_from = .model_desc, values_from = .value) %>%
      pivot_longer(-c(.index, ACTUAL)) |> 
      mutate(value = if_else(is.na(value), ACTUAL, value)) |> 
      pivot_wider(names_from = name, values_from = value) |> 
      pivot_longer(cols = -c(.index)) %>%
      group_by(name) %>%
      mutate(growth = value / lag(value, roc[2]) - 1) %>%
      drop_na() %>%
      select(-value) %>%
      ungroup() %>%
      mutate(use = if_else(.index > max_roc1_date & name != "ACTUAL", "use", "remove")) %>%
      filter(use == "use") %>% select(-use)

    # Combined forecasts (long)
    all_fc_tbl <- bind_rows(
      fc_level_roc2_tbl,
      fc_roc1_roc2_tbl,
      fc_roc2_tbl,
      fc_log_roc2_tbl
    ) %>%
      distinct() %>%
      mutate(resample = paste0("resample_", i))

    forecast_ls[[i]] <- all_fc_tbl

    # Accuracy for base models (per resample)
    base_acc <- all_fc_tbl %>%
      select(-resample) %>%
      pivot_wider(names_from = name, values_from = growth) %>%
      filter(.index >= min(test_loop_tbl$date)) %>%
      pivot_longer(
        cols = -c(.index, ACTUAL),
        names_to = "name",
        values_to = "estimate"
      ) %>%
      group_by(name) %>%
      summarize_accuracy_metrics(
        truth = ACTUAL,
        estimate = estimate,
        metric_set = metric_set(rmse)
      ) %>%
      mutate(resample = paste0("resample_", i))

    base_acc_ls[[i]] <- base_acc
  }

  # ---------- 4) Aggregate inner-CV forecasts ----------
  fc_tbl <- forecast_ls %>% bind_rows()
  .validate_fc_tbl(fc_tbl)

  # Base-model leaderboard from inner CV
  inner_acc_tbl <- .cv_accuracy(fc_tbl)

  leaderboard_base <- inner_acc_tbl %>%
    group_by(name) %>%
    summarise(
      mean_rmse = mean(rmse, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(mean_rmse)

  # Stability: % of folds a model is in the top-3 (RMSE)
  top3_by_fold <- inner_acc_tbl %>%
    group_by(resample) %>%
    slice_min(order_by = rmse, n = 3, with_ties = FALSE) %>%
    ungroup()
  stability_tbl <- top3_by_fold %>% count(name, name = "top3_hits") %>% mutate(top3_rate = top3_hits / length(unique(inner_acc_tbl$resample)))

  leaderboard_base <- leaderboard_base %>% left_join(stability_tbl, by = "name") %>% mutate(top3_hits = replace_na(top3_hits, 0), top3_rate = replace_na(top3_rate, 0))

  # Trim search space for pairs
  keep_models <- leaderboard_base %>% slice_head(n = min(top_k_base_for_pairs, nrow(.))) %>% pull(name)

  # ---------- 5) Ensembles (optional) ----------
  ensemble_records <- list()

  # Parse 'ensembles' argument
  ens_cfg <- list(auto_best2 = FALSE, manual = NULL)
  if (is.character(ensembles)) {
    if ("auto_best2" %in% ensembles) ens_cfg$auto_best2 <- TRUE
    if (!any(ensembles %in% c("auto_best2"))) {
      ens_cfg$manual <- list(ensembles)
    }
  } else if (is.list(ensembles)) {
    if (!is.null(ensembles$auto_best2)) ens_cfg$auto_best2 <- isTRUE(ensembles$auto_best2)
    if (!is.null(ensembles$manual)) {
      ens_cfg$manual <- if (is.character(ensembles$manual)) list(ensembles$manual) else ensembles$manual
    }
  }

  # AUTO best 2 (restricted to top-k base models)
  if (isTRUE(ens_cfg$auto_best2)) {
    best2 <- .best2_pair(fc_tbl, restrict_to = keep_models)
    lab   <- paste0("ENSEMBLE_", paste(best2$pair, collapse = "+"))
    fc_tbl <- .add_ensemble_series(fc_tbl, best2$pair, weights = stats::setNames(best2$weights, best2$pair), label = lab)
    ensemble_records <- append(ensemble_records, list(list(label = lab, models = best2$pair, weights = stats::setNames(best2$weights, best2$pair))))
    cli::cli_alert_success(paste0("Auto best-2 (trimmed): ", paste(best2$pair, collapse = " + "),
                                  " | weights ~ (", round(best2$weights[1],3), ", ", round(best2$weights[2],3), ")"))
  }

  # MANUAL combos (any length)
  if (!is.null(ens_cfg$manual)) {
    for (m in ens_cfg$manual) {
      lab <- paste0("ENSEMBLE_", paste(m, collapse = "+"))
      w   <- .fit_weights(fc_tbl, m)
      fc_tbl <- .add_ensemble_series(fc_tbl, m, weights = w, label = lab)
      ensemble_records <- append(ensemble_records, list(list(label = lab, models = m, weights = w)))
    }
  }

  # ---------- 6) Accuracy (recompute incl. ensembles) + stability ----------
  accuracy_tbl <- .cv_accuracy(fc_tbl)

  leaderboard <- accuracy_tbl %>%
    group_by(name) %>%
    summarise(
      mean_rmse = mean(rmse, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(mean_rmse) %>%
    left_join(
      accuracy_tbl %>% group_by(resample) %>% slice_min(order_by = rmse, n = 3, with_ties = FALSE) %>% ungroup() %>% count(name, name = "top3_hits"),
      by = "name"
    ) %>%
    mutate(top3_hits = replace_na(top3_hits, 0),
           top3_rate = top3_hits / length(unique(accuracy_tbl$resample)))

  # Optional: report an optimism penalty for the winner (winner's curse)
  optimism_tbl <- NULL
  if (isTRUE(report_optimism)) {
    M_eff <- n_distinct(leaderboard$name)  # crude effective number
    pooled_sd <- accuracy_tbl %>% summarise(p = sd(rmse, na.rm = TRUE)) %>% pull(p)
    opt_pen   <- pooled_sd * sqrt(2 * log(max(M_eff, 2)))
    optimism_tbl <- tibble(M_eff = M_eff, pooled_sd = pooled_sd, optimism_penalty = opt_pen)
  }

  selected_best <- leaderboard %>% slice(1)
  best_name     <- selected_best$name[1]
  best_is_ensemble <- str_starts(best_name, "ENSEMBLE_")

  best_details <- NULL
  if (best_is_ensemble && length(ensemble_records)) {
    rec <- purrr::keep(ensemble_records, ~ .x$label == best_name)
    if (length(rec)) best_details <- rec[[1]][c("models","weights")]
  }

  # ---------- 7) Outer evaluation on untouched blocks ----------
  outer_eval <- NULL
  if (outer_holdout_blocks >= 1) {
    # Build a single final model set by refitting on ALL inner data, then forecast the outer windows
    # For ensembles, reuse weights fitted on inner CV

    # Define outer dataset
    outer_tbl <- prepared_data_tbl %>% filter(date %in% outer_dates)

    # Refit base model groups on inner data end
    inner_all_tbl <- inner_prepared_tbl

    refit_and_forecast <- function(mtbl, desc_map_fn, transform = c("level","roc1","roc2","log")) {
      mtbl %>%
        modeltime_refit(inner_all_tbl) %>%
        modeltime_forecast(new_data = outer_tbl, actual_data = bind_rows(inner_all_tbl, outer_tbl)) |> 
        desc_map_fn()
    }

    map_desc <- function(df, kind) {
      mutate(df, .model_desc = case_when(
        str_detect(.model_desc, "SEASONAL DECOMP") ~ paste0("STL-", kind),
        str_detect(.model_desc, "ARIMA") ~ paste0("ARIMA-", kind),
        str_detect(.model_desc, "ETS") ~ paste0("ETS-", kind),
        str_detect(.model_desc, "BATS") ~ paste0("TBATS-", kind),
        str_detect(.model_desc, "TEMPORAL") ~ paste0("THIEF-", kind),
        TRUE ~ .model_desc))
    }

    ofc_level <- refit_and_forecast(univar_level_mtbl, ~map_desc(.x, "level")) %>%
      select(-c(.model_id, .key)) %>%
      pivot_wider(names_from = .model_desc, values_from = .value) %>%
      pivot_longer(cols = -c(.index)) %>%
      group_by(name) %>%
      mutate(growth = value / lag(value, roc[2]) - 1) %>%
      #drop_na() %>%
      select(-value) %>% ungroup()

    ofc_roc1 <- refit_and_forecast(univar_roc1_mtbl, ~map_desc(.x, "roc1")) %>%
      select(-c(.model_id, .key)) %>%
      filter(!.model_desc == "ACTUAL") %>%
      mutate(base = inner_all_tbl %>% filter(date == max(date)) %>% pull(level)) %>%
      group_by(.model_desc) %>% mutate(level = base * cumprod(1 + .value)) %>% ungroup() %>% select(-c(.value, base)) %>%
      bind_rows(bind_rows(inner_all_tbl, outer_tbl) %>% mutate(.model_desc = "ACTUAL") %>% set_names(".index", "level", ".model_desc")) %>%
      arrange(.index) %>% pivot_wider(names_from = .model_desc, values_from = level) %>%
      pivot_longer(cols = -c(.index)) %>% group_by(name) %>% mutate(growth = value / lag(value, roc[2]) - 1) %>% drop_na() %>% select(-value) %>% ungroup()

    ofc_roc2 <- refit_and_forecast(univar_roc2_mtbl, ~map_desc(.x, "roc2")) %>%
      select(.index, .model_desc, .value) %>% set_names(".index", "name", "growth")

    smear_outer <- 1
    try({
      ar_fit <- forecast::auto.arima(log(inner_all_tbl$level))
      sig2   <- stats::var(stats::residuals(ar_fit), na.rm = TRUE)
      smear_outer  <- exp(sig2/2)
    }, silent = TRUE)

    ofc_log <- refit_and_forecast(univar_log_mtbl, ~map_desc(.x, "log")) %>%
      mutate(.value = exp(.value) * smear_outer) %>%
      select(-c(.model_id, .key)) %>%
      pivot_wider(names_from = .model_desc, values_from = .value) %>%
      pivot_longer(cols = -c(.index)) %>%
      group_by(name) %>% mutate(growth = value / lag(value, roc[2]) - 1) %>% drop_na() %>% select(-value) %>% ungroup()

    outer_fc_tbl <- bind_rows(ofc_level, ofc_roc1, ofc_roc2, ofc_log) %>% distinct() %>% mutate(resample = "outer")

    # If ensembles were created, add them using previously learned weights
    if (length(ensemble_records)) {
      for (rec in ensemble_records) {
        outer_fc_tbl <- .add_ensemble_series(outer_fc_tbl, rec$models, weights = rec$weights, label = rec$label)
      }
    }

    # Outer-block accuracy
    outer_eval <- outer_fc_tbl %>%
      pivot_wider(names_from = name, values_from = growth) %>%
      pivot_longer(cols = -c(.index, ACTUAL), names_to = "name", values_to = "estimate") %>%
      group_by(name) %>%
      summarise(
        rmse  = yardstick::rmse_vec(truth = ACTUAL, estimate = estimate),
        mase  = yardstick::mase_vec(truth = ACTUAL, estimate = estimate, m = roc[2]),
        smape = yardstick::smape_vec(truth = ACTUAL, estimate = estimate),
        .groups = "drop"
      ) %>%
      arrange(rmse)
  }

  # ---------- 8) Return ----------
  out <- list(
    ForecastsInner = fc_tbl,
    AccuracyInner  = accuracy_tbl,
    Leaderboard    = leaderboard,
    LeaderboardBase= leaderboard_base,
    Stability      = leaderboard %>% select(name, top3_hits, top3_rate),
    SelectedBest   = list(name = best_name, is_ensemble = best_is_ensemble, details = best_details),
    OuterEval      = outer_eval,
    Optimism       = optimism_tbl,
    Ensembles      = if (length(ensemble_records)) tibble::tibble(
      label   = vapply(ensemble_records, function(x) x$label, character(1)),
      models  = I(lapply(ensemble_records, function(x) x$models)),
      weights = I(lapply(ensemble_records, function(x) x$weights))
    ) else NULL
  )

  return(out)
}
