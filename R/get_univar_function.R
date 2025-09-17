

# data = data_raw_tbl
# transformation = c("level", "log")
# roc = c(1, 12)
# freq = "month"
# horizon = 12
# slice_limit = 3
# skip = 1
# accuracy_at_level = "roc_2"
# ensembles = NULL
# 


get_univariate_forecasts <- function(data,
                                     transformation = c("level", "log"),
                                     roc = c(1, 12),
                                     freq = "month",
                                     horizon = 12,
                                     slice_limit = 3,
                                     skip = 1,
                                     accuracy_at_level = "roc_2",
                                     ensembles = NULL
) {
  
  require(modeltime)
  require(tidyverse)
  require(timetk)
  require(tidymodels)
  
  # SETUP ----
  if (sum(str_detect(transformation, "level")) > 0) {
    data_prep_tbl <- data %>%
      mutate(level = outcome)
  }

  if (sum(str_detect(transformation, "log")) > 0) {
    data_prep_tbl <- data_prep_tbl %>%
      mutate(log = log(outcome))
  }
  
  if (length(roc) != 0) {
    
    data_prep_tbl <- data_prep_tbl %>% 
      mutate(
        roc_1 = outcome / lag(outcome, roc[1]) - 1,
        roc_2 = outcome / lag(outcome, roc[2]) - 1
      ) %>% 
      drop_na()
    
  }
  
  data_prep_tbl <- data_prep_tbl %>% 
    select(-outcome)
  
  
  # Future data
  full_data_tbl <- data_prep_tbl %>% 
    future_frame(.date_var = date, .length_out = horizon, .bind_data = TRUE) %>% 
    fill(id, .direction = "down")
  
  
  future_data_tbl <- full_data_tbl %>% 
    tail(horizon)
  
  prepared_data_tbl <- full_data_tbl %>% 
    filter(!date %in% future_data_tbl$date) %>% 
    drop_na()
  
  
  # Splits
  splits <- prepared_data_tbl %>% time_series_cv(date_var = date, assess = horizon, skip = skip, slice_limit = slice_limit, cumulative = TRUE)
  
  train_data_for_initial_fit_tbl <- splits$splits[[1]] %>% training()
  test_data_for_initial_fit_tbl  <- splits$splits[[1]] %>% testing()
  
  
  # 2.0.0 MODELLING ----
  
  # 2.1.0 ARIMA ----
  # 2.1.1 Level
  cli::cli_h3("Fit ARIMA")
  
  arima_level_mtbl <- arima_reg() %>% 
    set_engine("auto_arima") %>% 
    fit(level ~ date, data = train_data_for_initial_fit_tbl) %>% 
    modeltime_table() %>% 
    mutate(.model_desc = "ARIMA-level")
  
  # 2.1.2 1m roc
  arima_roc1_mtbl <- arima_reg() %>% 
    set_engine("auto_arima") %>% 
    fit(roc_1 ~ date, data = train_data_for_initial_fit_tbl) %>% 
    modeltime_table() %>% 
    mutate(.model_desc = "ARIMA-roc1")
  
  # 2.1.3 12m roc
  arima_roc2_mtbl <- arima_reg() %>% 
    set_engine("auto_arima") %>% 
    fit(roc_2 ~ date, data = train_data_for_initial_fit_tbl) %>% 
    modeltime_table() %>% 
    mutate(.model_desc = "ARIMA-roc2")
  
  # 2.1.4 log
  arima_log_mtbl <- arima_reg() %>% 
    set_engine("auto_arima") %>% 
    fit(log ~ date, data = train_data_for_initial_fit_tbl) %>% 
    modeltime_table() %>% 
    mutate(.model_desc = "ARIMA-log")
  
  
  # 2.2.0 EXPONENTIAL SMOOTHING ----
  cli::cli_h3("Fit Exponential smoothing")
  
  # 2.2.1 Level
  ets_level_mtbl <- exp_smoothing() %>% 
    set_engine("ets") %>% 
    fit(level ~ date, data = train_data_for_initial_fit_tbl) %>% 
    modeltime_table() %>% 
    mutate(.model_desc = "ETS-level")
  
  # 2.2.2 1m roc
  ets_roc1_mtbl <- exp_smoothing() %>% 
    set_engine("ets") %>% 
    fit(roc_1 ~ date, data = train_data_for_initial_fit_tbl) %>% 
    modeltime_table() %>% 
    mutate(.model_desc = "ETS-roc1")
  
  # 2.2.3 12m roc
  ets_roc2_mtbl <- exp_smoothing() %>% 
    set_engine("ets") %>% 
    fit(roc_2 ~ date, data = train_data_for_initial_fit_tbl) %>% 
    modeltime_table() %>% 
    mutate(.model_desc = "ETS-roc2")
  
  # 2.2.4 log
  ets_log_mtbl <- exp_smoothing() %>% 
    set_engine("ets") %>% 
    fit(log ~ date, data = train_data_for_initial_fit_tbl) %>% 
    modeltime_table() %>% 
    mutate(.model_desc = "ETS-log")
  
  
  # 2.3.0 TBATS ----
  cli::cli_h3("Fit TBATS")
  
  # 2.3.1 Level
  tbats_level_mtbl <- seasonal_reg() %>% 
    set_engine("tbats") %>% 
    fit(level ~ date, data = train_data_for_initial_fit_tbl) %>% 
    modeltime_table() %>% 
    mutate(.model_desc = "TBATS-level")
  
  # 2.3.2 1m roc
  tbats_roc1_mtbl <- seasonal_reg() %>% 
    set_engine("tbats") %>% 
    fit(roc_1 ~ date, data = train_data_for_initial_fit_tbl) %>% 
    modeltime_table() %>% 
    mutate(.model_desc = "TBATS-roc1")
  
  # 2.3.3 12m roc
  tbats_roc2_mtbl <- seasonal_reg() %>% 
    set_engine("tbats") %>% 
    fit(roc_2 ~ date, data = train_data_for_initial_fit_tbl) %>% 
    modeltime_table() %>% 
    mutate(.model_desc = "TBATS-roc2")
  
  # 2.3.4 log
  tbats_log_mtbl <- seasonal_reg() %>% 
    set_engine("tbats") %>% 
    fit(log ~ date, data = train_data_for_initial_fit_tbl) %>% 
    modeltime_table() %>% 
    mutate(.model_desc = "TBATS-log")
  
  
  # 2.4.0 THIEF ----
  cli::cli_h3("Fit THIEF")
  
  # 2.4.1 Level
  thief_level_mtbl <- temporal_hierarchy(use_model = "arima", combination_method = "struc") %>% 
    set_engine("thief") %>% 
    fit(level ~ date, data = train_data_for_initial_fit_tbl) %>% 
    modeltime_table() %>% 
    mutate(.model_desc = "THIEF-level")
  
  # 2.4.2 1m roc
  thief_roc1_mtbl <- temporal_hierarchy(use_model = "arima", combination_method = "struc") %>% 
    set_engine("thief") %>% 
    fit(roc_1 ~ date, data = train_data_for_initial_fit_tbl) %>% 
    modeltime_table() %>% 
    mutate(.model_desc = "THIEF-roc1")
  
  # 2.4.3 12m roc
  thief_roc2_mtbl <- temporal_hierarchy(use_model = "arima", combination_method = "struc") %>% 
    set_engine("thief") %>% 
    fit(roc_2 ~ date, data = train_data_for_initial_fit_tbl) %>% 
    modeltime_table() %>% 
    mutate(.model_desc = "THIEF-roc2")
  
  # 2.4.4 log
  thief_log_mtbl <- temporal_hierarchy(use_model = "arima", combination_method = "struc") %>% 
    set_engine("thief") %>% 
    fit(log ~ date, data = train_data_for_initial_fit_tbl) %>% 
    modeltime_table() %>% 
    mutate(.model_desc = "THIEF-log")
  
  
  
  # 2.5.0 STL ----
  cli::cli_h3("Fit STL")
  
  # 2.5.1 Level
  stl_level_mtbl <- seasonal_reg() %>% 
    set_engine("stlm_arima") %>% 
    fit(level ~ date, data = train_data_for_initial_fit_tbl) %>% 
    modeltime_table() %>% 
    mutate(.model_desc = "STL-level")
  
  # 2.5.2 1m roc
  stl_roc1_mtbl <- seasonal_reg() %>% 
    set_engine("stlm_arima") %>% 
    fit(roc_1 ~ date, data = train_data_for_initial_fit_tbl) %>% 
    modeltime_table() %>% 
    mutate(.model_desc = "STL-roc1")
  
  # 2.5.3 12m roc
  stl_roc2_mtbl <- seasonal_reg() %>% 
    set_engine("stlm_arima") %>% 
    fit(roc_2 ~ date, data = train_data_for_initial_fit_tbl) %>% 
    modeltime_table() %>% 
    mutate(.model_desc = "STL-roc2")
  
  # 2.5.4 log
  stl_log_mtbl <- seasonal_reg() %>% 
    set_engine("stlm_arima") %>% 
    fit(log ~ date, data = train_data_for_initial_fit_tbl) %>% 
    modeltime_table() %>% 
    mutate(.model_desc = "STL-log")
  
  
  # 2.6.0 Combine ----
  cli::cli_h3("Combine")
  
  univar_level_mtbl <- combine_modeltime_tables(
    arima_level_mtbl,
    ets_level_mtbl,
    tbats_level_mtbl,
    thief_level_mtbl,
    stl_level_mtbl
  )
  
  univar_roc1_mtbl <- combine_modeltime_tables(
    arima_roc1_mtbl,
    ets_roc1_mtbl,
    tbats_roc1_mtbl,
    thief_roc1_mtbl,
    stl_roc1_mtbl
  )
  
  
  univar_roc2_mtbl <- combine_modeltime_tables(
    arima_roc2_mtbl,
    ets_roc2_mtbl,
    tbats_roc2_mtbl,
    thief_roc2_mtbl,
    stl_roc2_mtbl
    
  )
  
  univar_log_mtbl <- combine_modeltime_tables(
    arima_log_mtbl,
    ets_log_mtbl,
    tbats_log_mtbl,
    thief_log_mtbl,
    stl_log_mtbl
  )
  

  

  # 3.0.0 Forecast ----------------------------------------------------------

  forecast_ls    <- list()
  fc_ensemble_ls <- list()
  accuracy_ls    <- list()
  
  for(i in 1:nrow(splits)) {
    
    cli::cli_h3(glue::glue("Resample númer: {i} af {nrow(splits)}"))
    
    # Univariate models
    train_loop_univar_tbl <- splits$splits[[i]] %>% training()
    test_loop_univar_tbl  <- splits$splits[[i]] %>% testing()
    full_loop_univar_tbl  <- bind_rows(train_loop_univar_tbl, test_loop_univar_tbl) %>% select(date, level)
    
    # Sæki viðeigandi gildi til að geta reiknað út 1m verðbólgu og 12m verðbólgu
    max_roc1_date  <- train_loop_univar_tbl %>% filter(date == max(date)) %>% pull(date)
    
    date_subtr <- paste0(roc[2] - 1, " ", freq)
    max_roc2_date <- train_loop_univar_tbl %>% filter(date == max(date)) %>% pull(date) %-time% date_subtr
    
    level_roc1_max <- prepared_data_tbl %>% filter(date == max_roc1_date) %>% pull(level)
    level_roc2_max <- prepared_data_tbl %>% filter(date == max_roc2_date) %>% pull(level)
    
    

    # 1.0.0 FORECASTS ----
    
# 1.1.0 LEVEL -------------------------------------------------------------

    fc_level_tbl <- univar_level_mtbl %>% 
      modeltime_refit(train_loop_univar_tbl) %>% 
      modeltime_forecast(
        new_data    = test_loop_univar_tbl,
        actual_data = bind_rows(train_loop_univar_tbl, test_loop_univar_tbl)
      ) %>% 
      mutate(
        .model_desc =
          case_when(
            str_detect(.model_desc, "SEASONAL DECOMP") ~ "STL-level",
            str_detect(.model_desc, "ARIMA") ~ "ARIMA-Level",
            str_detect(.model_desc, "ETS") ~ "ETS-Level",
            str_detect(.model_desc, "BATS") ~ "TBATS-Level",
            str_detect(.model_desc, "TEMPORAL") ~ "THIEF-Level",
            TRUE ~ .model_desc
          )
      )
    
    # 1.1.1 Level to 12m roc
    fc_level_roc2_tbl <- fc_level_tbl %>%
      select(-c(.model_id, .key)) %>%
      pivot_wider(names_from = .model_desc, values_from = .value) %>%
      mutate(across(contains("level"), .fns = function(x) ifelse(is.na(x), ACTUAL, x))) %>% 
      pivot_longer(cols = -c(.index)) %>% 
      group_by(name) %>% 
      mutate(growth = value / lag(value, roc[2])  - 1) %>% 
      drop_na() %>% 
      select(-value) %>% 
      ungroup()
    
    # Remove historical values for the model
    fc_level_roc2_tbl <- fc_level_roc2_tbl %>% 
      mutate(
        use = 
          case_when(
            .index <= max_roc1_date & name != "ACTUAL" ~ "remove",
            TRUE ~ "use"
          )
      ) %>% 
      filter(use == "use") %>% 
      select(-use)
    

# 1.2.0 ROC 1 -------------------------------------------------------------

    fc_roc1_tbl <- univar_roc1_mtbl %>% 
      modeltime_refit(train_loop_univar_tbl) %>% 
      modeltime_forecast(
        new_data = test_loop_univar_tbl,
        actual_data = bind_rows(train_loop_univar_tbl, test_loop_univar_tbl)
      ) %>% 
      mutate(
        .model_desc =
          case_when(
            str_detect(.model_desc, "SEASONAL DECOMP") ~ "STL-roc1",
            str_detect(.model_desc, "ARIMA") ~ "ARIMA-roc1",
            str_detect(.model_desc, "ETS") ~ "ETS-roc1",
            str_detect(.model_desc, "BATS") ~ "TBATS-roc1",
            str_detect(.model_desc, "TEMPORAL") ~ "THIEF-roc1",
            TRUE ~ .model_desc
          )
      )
    
    # 1.2.1 1m roc to 12m
    fc_roc1_to_level_tbl <- fc_roc1_tbl %>% 
      select(-c(.model_id, .key)) %>% 
      filter(!.model_desc == "ACTUAL") %>% 
      mutate(base = level_roc1_max) %>%
      group_by(.model_desc) %>% 
      mutate(level = base * cumprod(1 + .value)) %>% 
      ungroup() %>% 
      select(-c(.value, base)) %>% 
      bind_rows(full_loop_univar_tbl %>%
                  mutate(.model_desc = "ACTUAL") %>% 
                  set_names(".index", "level", ".model_desc")
      ) %>% 
      arrange(.index) %>% 
      pivot_wider(names_from = .model_desc, values_from = level)
    
    # 1m roc forecast to 12m roc forecast
    fc_roc1_roc2_tbl <- fc_roc1_to_level_tbl %>% 
      mutate(across(contains("roc1") | contains("ENSEMBLE"), .fns = function(x) ifelse(is.na(x), ACTUAL, x))) %>% 
      pivot_longer(cols = -c(.index)) %>% 
      group_by(name) %>% 
      mutate(growth = value / lag(value, roc[2]) - 1) %>% 
      drop_na() %>% 
      select(-value) %>% 
      ungroup()
    
    
    # Remove historical values for the model
    fc_roc1_roc2_tbl <- fc_roc1_roc2_tbl %>% 
      mutate(
        use = 
          case_when(
            .index <= max_roc1_date & name != "ACTUAL" ~ "remove",
            TRUE ~ "use"
          )
      ) %>% 
      filter(use == "use") %>% 
      select(-use)
    
    

# 1.3.0 ROC 2 -------------------------------------------------------------

    fc_roc2_tbl <- univar_roc2_mtbl %>% 
      modeltime_refit(train_loop_univar_tbl) %>% 
      modeltime_forecast(
        new_data    = test_loop_univar_tbl,
        actual_data = bind_rows(train_loop_univar_tbl, test_loop_univar_tbl)
      ) %>% 
      mutate(
        .model_desc =
          case_when(
            str_detect(.model_desc, "SEASONAL DECOMP") ~ "STL-roc2",
            str_detect(.model_desc, "ARIMA") ~ "ARIMA-roc2",
            str_detect(.model_desc, "ETS") ~ "ETS-roc2",
            str_detect(.model_desc, "BATS") ~ "TBATS-roc2",
            str_detect(.model_desc, "TEMPORAL") ~ "THIEF-roc2",
            TRUE ~ .model_desc
          )
      )
    
    fc_roc2_tbl <- fc_roc2_tbl %>% 
      select(.index, .model_desc, .value) %>% 
      set_names(".index", "name", "growth")
    
    
    # Remove historical values for the model
    fc_roc2_tbl <- fc_roc2_tbl %>% 
      mutate(
        use = 
          case_when(
            .index <= max_roc1_date & name != "ACTUAL" ~ "remove",
            TRUE ~ "use"
          )
      ) %>% 
      filter(use == "use") %>% 
      select(-use)
    
    

# 1.4.0 log ---------------------------------------------------------------

    fc_log_tbl <- univar_log_mtbl %>% 
      modeltime_refit(train_loop_univar_tbl) %>% 
      modeltime_forecast(
        new_data = test_loop_univar_tbl,
        actual_data = bind_rows(train_loop_univar_tbl, test_loop_univar_tbl)
      ) %>% 
      mutate(
        .model_desc =
          case_when(
            str_detect(.model_desc, "SEASONAL DECOMP") ~ "STL-log",
            str_detect(.model_desc, "ARIMA") ~ "ARIMA-log",
            str_detect(.model_desc, "ETS") ~ "ETS-log",
            str_detect(.model_desc, "BATS") ~ "TBATS-log",
            str_detect(.model_desc, "TEMPORAL") ~ "THIEF-log",
            TRUE ~ .model_desc
          )
      )
    
    
    # 1.4.1 Level to 12m roc
    fc_log_roc2_tbl <- fc_log_tbl %>%
      mutate(.value = exp(.value)) %>% 
      select(-c(.model_id, .key)) %>%
      pivot_wider(names_from = .model_desc, values_from = .value) %>%
      mutate(across(contains("log"), .fns = function(x) ifelse(is.na(x), ACTUAL, x))) %>% 
      pivot_longer(cols = -c(.index)) %>% 
      group_by(name) %>% 
      mutate(growth = value / lag(value, roc[2]) - 1) %>% 
      drop_na() %>% 
      select(-value) %>% 
      ungroup()
    
    
    # Remove historical values for the model
    fc_log_roc2_tbl <- fc_log_roc2_tbl %>% 
      mutate(
        use = 
          case_when(
            .index > max_roc1_date & name != "ACTUAL" ~ "use",
            TRUE ~ "remove"
          )
      ) %>% 
      filter(use == "use") %>% 
      select(-use)
    
    

# 1.5.0 Combined forecasts ------------------------------------------------

    all_fc_tbl <- bind_rows(
      fc_level_roc2_tbl,
      fc_roc1_roc2_tbl,
      fc_roc2_tbl,
      fc_log_roc2_tbl
    ) %>% 
      distinct() %>% 
      mutate(resample = paste0("resample_", i))
    
    forecast_ls[[i]] <- all_fc_tbl
    # fc_ensemble_ls[[i]] <- all_fc_tbl %>%
    #   pivot_wider(names_from = name, values_from = growth) %>% 
    #   add_ensemble_columns
    

# 1.6.0 Accuracy ----------------------------------------------------------

    accuracy_tbl <- all_fc_tbl %>% 
      select(-resample) %>% 
      pivot_wider(names_from = name, values_from = growth) %>% 
      filter(.index >= min(test_loop_univar_tbl$date)) %>% 
      pivot_longer(cols = -c(.index, ACTUAL)) %>% 
      group_by(name) %>% 
      summarize_accuracy_metrics(truth = ACTUAL, estimate = value, metric_set = metric_set(rmse)) %>% 
      mutate(resample = paste0("resample_", i))
    
    accuracy_ls[[i]] <- accuracy_tbl    
    
  }

  fc_tbl <- forecast_ls %>% bind_rows()
  accuracy_tbl <- accuracy_ls %>% bind_rows()



  # ---- 4.0.0 Ensembles (optional) ----------------------------------------
ensemble_results <- NULL

if (!is.null(ensembles) && "auto_best2" %in% ensembles) {

  # Pick best pair & weights using cross-validated test slices
  pick <- pick_best_pair_and_weights(fc_tbl, step = 0.05)
  best_pair    <- pick$pair
  best_weights <- pick$weights
  pair_board   <- pick$leaderboard

  cli::cli_alert_success(
    "Best pair: {best_pair[1]} + {best_pair[2]} | weights ~ ({round(best_weights[1],3)}, {round(best_weights[2],3)})"
  )

  # Build ensemble forecasts (same long format as fc_tbl)
  ens_fc_tbl <- build_pair_ensemble_fc(
    fc_tbl,
    pair    = best_pair,
    weights = best_weights
  )

  # Bind into forecast table so downstream code can see it
  fc_tbl <- bind_rows(fc_tbl, ens_fc_tbl)

  # Compute accuracy table for the ensemble (and compare)
  ensemble_accuracy <- accuracy_from_long(fc_tbl, include_models = unique(ens_fc_tbl$name))

  ensemble_results <- list(
    "BestPair"         = best_pair,
    "Weights"          = best_weights,
    "PairLeaderboard"  = pair_board,
    "EnsembleAccuracy" = ensemble_accuracy
  )
}

 return_list <- list(
  "Forecasts" = fc_tbl,
  "Accuracy"  = accuracy_tbl
)

if (!is.null(ensemble_results)) {
  return_list$Ensembles <- ensemble_results
}

return(return_list)
  
  
}



# ---- Helpers for picking & building 2-model ensembles ----

# Grid of convex weights (you can set step = 0.01 if you want it finer)
.weight_grid <- function(step = 0.05) {
  tibble(w1 = seq(0, 1, by = step)) %>% mutate(w2 = 1 - w1)
}

# Compute CV metrics for all pairs and pick the best pair + weights
pick_best_pair_and_weights <- function(fc_tbl, # long: resample, .index, name, growth
                                       step = 0.05) {
  stopifnot("ACTUAL" %in% unique(fc_tbl$name))

  wide <- fc_tbl %>%
    select(resample, .index, name, growth) %>%
    pivot_wider(names_from = name, values_from = growth)

  model_names <- setdiff(names(wide), c("resample", ".index", "ACTUAL"))
  if (length(model_names) < 2) {
    cli::cli_abort("Need at least two candidate models to form an ensemble.")
  }

  pairs <- t(combn(model_names, 2)) %>%
    as_tibble(.name_repair = "minimal") %>%
    set_names(c("m1", "m2"))

  # Average error correlation across resamples (for tie-breaking/info)
  cor_tbl <- pairs %>%
    rowwise() %>%
    mutate(cor_mean = {
      cors <- wide %>%
        group_by(resample) %>%
        summarize(
          cor = suppressWarnings(
            cor(
              (.data[[m1]] - ACTUAL),
              (.data[[m2]] - ACTUAL),
              use = "complete.obs"
            )
          ),
          .groups = "drop_last"
        ) %>%
        pull(cor)
      mean(cors, na.rm = TRUE)
    }) %>%
    ungroup()

  wg <- tibble(w1 = seq(0, 1, by = step)) %>% mutate(w2 = 1 - w1)

  # For each pair & resample, pick the best convex weight on that resample
  best_per_resample <- pairs %>%
    rowwise() %>%
    mutate(metrics = list({
      wide %>%
        group_by(resample) %>%
        reframe(
          wg,
          ens  = w1 * .data[[m1]] + w2 * .data[[m2]],
          rmse = yardstick::rmse_vec(truth = ACTUAL, estimate = ens)
        ) %>%
        group_by(resample) %>%
        slice_min(rmse, with_ties = FALSE) %>%
        ungroup()
    })) %>%
    ungroup()

  summary_pairs <- best_per_resample %>%
    mutate(
      mean_rmse = purrr::map_dbl(metrics, ~ mean(.x$rmse, na.rm = TRUE)),
      mean_w1   = purrr::map_dbl(metrics, ~ mean(.x$w1,  na.rm = TRUE)),
      mean_w2   = purrr::map_dbl(metrics, ~ mean(.x$w2,  na.rm = TRUE))
    ) %>%
    select(m1, m2, mean_rmse, mean_w1, mean_w2) %>%
    left_join(cor_tbl, by = c("m1","m2")) %>%
    arrange(mean_rmse, abs(cor_mean))

  best <- summary_pairs %>% slice(1)

  list(
    pair        = c(best$m1, best$m2),
    weights     = c(best$mean_w1, best$mean_w2),
    leaderboard = summary_pairs
  )
}


# Build an ensemble series (long) from chosen pair + weights
build_pair_ensemble_fc <- function(fc_tbl, pair, weights, label = NULL) {
  label <- label %||% paste0("ENSEMBLE_", pair[1], "_", pair[2])

  # Join the two model columns and ACTUAL for completeness (not required)
  fc_tbl %>%
    select(resample, .index, name, growth) %>%
    filter(name %in% c(pair, "ACTUAL")) %>%
    pivot_wider(names_from = name, values_from = growth) %>%
    mutate("{label}" := weights[1] * .data[[ pair[1] ]] + weights[2] * .data[[ pair[2] ]]) %>%
    select(resample, .index, "{label}") %>%
    pivot_longer(-c(resample, .index), names_to = "name", values_to = "growth")
}

# Accuracy helper for any long-format forecast table (expects ACTUAL inside fc_tbl)
accuracy_from_long <- function(fc_tbl, include_models = NULL) {
  wide <- fc_tbl %>%
    select(resample, .index, name, growth) %>%
    pivot_wider(names_from = name, values_from = growth)

  cols <- setdiff(names(wide), c("resample", ".index", "ACTUAL"))
  if (!is.null(include_models)) cols <- intersect(cols, include_models)

  wide %>%
    pivot_longer(cols = all_of(cols), names_to = "name", values_to = "estimate") %>%
    group_by(resample, name) %>%
    summarize(rmse = yardstick::rmse_vec(truth = ACTUAL, estimate = estimate),
              .groups = "drop") %>%
    group_by(name) %>%
    summarize(mean_rmse = mean(rmse, na.rm = TRUE), .groups = "drop") %>%
    arrange(mean_rmse)
}
