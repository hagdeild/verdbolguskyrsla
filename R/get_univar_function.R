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

get_univariate_forecasts <- function(
  data,
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
  splits <- prepared_data_tbl %>%
    time_series_cv(
      date_var = date,
      assess = horizon,
      skip = skip,
      slice_limit = slice_limit + 1, # +1 er test set áður en við setjum saman ensemble
      cumulative = TRUE
    )

  train_data_for_initial_fit_tbl <- splits$splits[[nrow(splits)]] %>% training()
  test_data_for_initial_fit_tbl <- splits$splits[[nrow(splits)]] %>% testing()
  full_data_for_initial_fit_tbl <- bind_rows(
    train_data_for_initial_fit_tbl,
    test_data_for_initial_fit_tbl
  )

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
  thief_level_mtbl <- temporal_hierarchy(
    use_model = "arima",
    combination_method = "struc"
  ) %>%
    set_engine("thief") %>%
    fit(level ~ date, data = train_data_for_initial_fit_tbl) %>%
    modeltime_table() %>%
    mutate(.model_desc = "THIEF-level")

  # 2.4.2 1m roc
  thief_roc1_mtbl <- temporal_hierarchy(
    use_model = "arima",
    combination_method = "struc"
  ) %>%
    set_engine("thief") %>%
    fit(roc_1 ~ date, data = train_data_for_initial_fit_tbl) %>%
    modeltime_table() %>%
    mutate(.model_desc = "THIEF-roc1")

  # 2.4.3 12m roc
  thief_roc2_mtbl <- temporal_hierarchy(
    use_model = "arima",
    combination_method = "struc"
  ) %>%
    set_engine("thief") %>%
    fit(roc_2 ~ date, data = train_data_for_initial_fit_tbl) %>%
    modeltime_table() %>%
    mutate(.model_desc = "THIEF-roc2")

  # 2.4.4 log
  thief_log_mtbl <- temporal_hierarchy(
    use_model = "arima",
    combination_method = "struc"
  ) %>%
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

  # Accuracy for test set = 1. Use this information to create extra ensembles
  max_roc1_date <- train_data_for_initial_fit_tbl %>%
    filter(date == max(date)) %>%
    pull(date)

  date_subtr <- paste0(roc[2] - 1, " ", freq)
  max_roc2_date <- train_data_for_initial_fit_tbl %>%
    filter(date == max(date)) %>%
    pull(date) %-time%
    date_subtr

  level_roc1_max <- prepared_data_tbl %>%
    filter(date == max_roc1_date) %>%
    pull(level)
  level_roc2_max <- prepared_data_tbl %>%
    filter(date == max_roc2_date) %>%
    pull(level)

  # 1.0.0 FORECASTS ----

  # 1.1.0 LEVEL -------------------------------------------------------------

  fc_level_tbl <- univar_level_mtbl %>%
    modeltime_forecast(
      new_data = test_data_for_initial_fit_tbl,
      actual_data = bind_rows(
        train_data_for_initial_fit_tbl,
        test_data_for_initial_fit_tbl
      )
    ) %>%
    mutate(
      .model_desc = case_when(
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
    mutate(across(contains("level"), .fns = function(x) {
      ifelse(is.na(x), ACTUAL, x)
    })) %>%
    pivot_longer(cols = -c(.index)) %>%
    group_by(name) %>%
    mutate(growth = value / lag(value, roc[2]) - 1) %>%
    drop_na() %>%
    select(-value) %>%
    ungroup()

  # Remove historical values for the model
  fc_level_roc2_tbl <- fc_level_roc2_tbl %>%
    mutate(
      use = case_when(
        .index <= max_roc1_date & name != "ACTUAL" ~ "remove",
        TRUE ~ "use"
      )
    ) %>%
    filter(use == "use") %>%
    select(-use)

  # 1.2.0 ROC 1 -------------------------------------------------------------

  fc_roc1_tbl <- univar_roc1_mtbl %>%
    modeltime_forecast(
      new_data = test_data_for_initial_fit_tbl,
      actual_data = bind_rows(
        train_data_for_initial_fit_tbl,
        test_data_for_initial_fit_tbl
      )
    ) %>%
    mutate(
      .model_desc = case_when(
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
    bind_rows(
      full_data_for_initial_fit_tbl |>
        select(date, level) %>%
        mutate(.model_desc = "ACTUAL") %>%
        set_names(".index", "level", ".model_desc")
    ) %>%
    arrange(.index) %>%
    pivot_wider(names_from = .model_desc, values_from = level)

  # 1m roc forecast to 12m roc forecast
  fc_roc1_roc2_tbl <- fc_roc1_to_level_tbl %>%
    mutate(across(
      contains("roc1") | contains("ENSEMBLE"),
      .fns = function(x) ifelse(is.na(x), ACTUAL, x)
    )) %>%
    pivot_longer(cols = -c(.index)) %>%
    group_by(name) %>%
    mutate(growth = value / lag(value, roc[2]) - 1) %>%
    drop_na() %>%
    select(-value) %>%
    ungroup()

  # Remove historical values for the model
  fc_roc1_roc2_tbl <- fc_roc1_roc2_tbl %>%
    mutate(
      use = case_when(
        .index <= max_roc1_date & name != "ACTUAL" ~ "remove",
        TRUE ~ "use"
      )
    ) %>%
    filter(use == "use") %>%
    select(-use)

  # 1.3.0 ROC 2 -------------------------------------------------------------

  fc_roc2_tbl <- univar_roc2_mtbl %>%
    modeltime_forecast(
      new_data = test_data_for_initial_fit_tbl,
      actual_data = bind_rows(
        train_data_for_initial_fit_tbl,
        test_data_for_initial_fit_tbl
      )
    ) %>%
    mutate(
      .model_desc = case_when(
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
      use = case_when(
        .index <= max_roc1_date & name != "ACTUAL" ~ "remove",
        TRUE ~ "use"
      )
    ) %>%
    filter(use == "use") %>%
    select(-use)

  # 1.4.0 log ---------------------------------------------------------------

  fc_log_tbl <- univar_log_mtbl %>%
    modeltime_forecast(
      new_data = test_data_for_initial_fit_tbl,
      actual_data = bind_rows(
        train_data_for_initial_fit_tbl,
        test_data_for_initial_fit_tbl
      )
    ) %>%
    mutate(
      .model_desc = case_when(
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
    mutate(across(contains("log"), .fns = function(x) {
      ifelse(is.na(x), ACTUAL, x)
    })) %>%
    pivot_longer(cols = -c(.index)) %>%
    group_by(name) %>%
    mutate(growth = value / lag(value, roc[2]) - 1) %>%
    drop_na() %>%
    select(-value) %>%
    ungroup()

  # Remove historical values for the model
  fc_log_roc2_tbl <- fc_log_roc2_tbl %>%
    mutate(
      use = case_when(
        .index > max_roc1_date & name != "ACTUAL" ~ "use",
        TRUE ~ "remove"
      )
    ) %>%
    filter(use == "use") %>%
    select(-use)

  # Combined forecasts ----

  all_initial_fc_tbl <- bind_rows(
    fc_level_roc2_tbl,
    fc_roc1_roc2_tbl,
    fc_roc2_tbl,
    fc_log_roc2_tbl
  ) %>%
    distinct()

  # Accuracy ----

  all_initial_accuracy_tbl <- all_initial_fc_tbl |>
    pivot_wider(names_from = name, values_from = growth) %>%
    filter(.index >= min(test_data_for_initial_fit_tbl$date)) %>%
    pivot_longer(
      cols = -c(.index, ACTUAL),
      names_to = "name",
      values_to = "estimate"
    ) |>
    group_by(name) %>%
    summarize_accuracy_metrics(
      truth = ACTUAL,
      estimate = estimate,
      metric_set = metric_set(rmse)
    ) |>
    arrange((rmse))

  # Create ensembles before test set loop ----
  top_2 <- all_initial_accuracy_tbl |>
    slice_head(n = 2) |>
    pull(name)

  top_3 <- all_initial_accuracy_tbl |>
    slice_head(n = 3) |>
    pull(name)

  all_models <- unique(all_initial_accuracy_tbl$name)

  ensembles <- list(ensembles, top_2, top_3, all_models) |> compact()

  # Create ensemb ----
  forecast_ls <- list()
  accuracy_ls <- list()

  for (i in nrow(splits):2) {
    cli::cli_h3(glue::glue("Resample númer: {i} af {nrow(splits)}"))

    train_loop_univar_tbl <- splits$splits[[i]] %>% training()
    test_loop_univar_tbl <- splits$splits[[i]] %>% testing()

    all_fc_tbl <- create_forecast(
      fit_data = train_loop_univar_tbl,
      forecast_data = test_loop_univar_tbl
    )

    # Create ensemble
    ensemble_fc_tbl <- create_ensembles(
      data = all_fc_tbl,
      ensembles = ensembles
    )

    forecast_ls[[i]] <- ensemble_fc_tbl |>
      mutate(resample = paste0("resample_", i))

    # 1.6.0 Accuracy ----------------------------------------------------------

    all_accuracy_tbl <- ensemble_fc_tbl |>
      pivot_wider(names_from = name, values_from = growth) %>%
      filter(.index >= min(test_data_for_initial_fit_tbl$date)) %>%
      pivot_longer(
        cols = -c(.index, ACTUAL),
        names_to = "name",
        values_to = "estimate"
      ) |>
      group_by(name) %>%
      summarize_accuracy_metrics(
        truth = ACTUAL,
        estimate = estimate,
        metric_set = metric_set(rmse)
      ) |>
      arrange((rmse)) |>
      mutate(resample = paste0("resample_", i))

    accuracy_ls[[i]] <- all_accuracy_tbl
  }

  final_accuracy_tbl <- accuracy_ls |>
    bind_rows() |>
    group_by(name) |>
    summarise(
      mean = mean(rmse),
      sd = sd(rmse)
    ) |>
    arrange(mean)

  # Create future forecast ----

  fc_tbl <- create_forecast(
    fit_data = data_prep_tbl,
    forecast_data = future_data_tbl
  )

  future_ensemble_tbl <- create_ensembles(
    data = fc_tbl,
    ensembles = ensembles
  )

  # Get the most accurate one
  best_model <- final_accuracy_tbl |>
    slice_head(n = 1) |>
    pull(name)

  best_future_fc_tbl <- future_ensemble_tbl |>
    mutate(
      keep = case_when(
        name == "ACTUAL" ~ "keep",
        name %in% best_model ~ "keep",
        TRUE ~ "remove"
      )
    ) |>
    filter(keep == "keep") |>
    select(-keep)

  return_list <- list(
    "future_fc" = best_future_fc_tbl,
    "Accuracy" = final_accuracy_tbl,
    "tscv" = forecast_ls |> bind_rows()
  )

  return(return_list)
}


# Ensemble
create_ensembles <- function(
  data,
  ensembles,
  na.rm = TRUE,
  prefix = "ensemble_"
) {
  max_date <- data |>
    filter(name != "ACTUAL") |>
    filter(.index == min(.index)) |>
    pull(.index) |>
    unique()

  data_ensemble <- data |>
    arrange(.index) |>
    pivot_wider(names_from = name, values_from = growth) |>
    filter(.index >= max_date)

  out <- data_ensemble
  long_idx <- 0L

  for (mods in ensembles) {
    # Name logic
    if (length(mods) > 3L) {
      long_idx <- long_idx + 1L
      new_name <- paste0("ensemble_long_", long_idx)
    } else {
      new_name <- paste0(prefix, paste(mods, collapse = "_"))
    }

    # Compute row-wise mean over selected model columns
    out[[new_name]] <- rowMeans(
      dplyr::select(out, dplyr::all_of(mods)),
      na.rm = na.rm
    )
  }

  out |>
    pivot_longer(cols = -c(.index)) |>
    rename("growth" = "value") |>
    bind_rows(data |> filter(name == "ACTUAL")) |>
    arrange(.index) |>
    distinct()
}


create_forecast <- function(fit_data, forecast_data) {
  cli::cli_h3(glue::glue("Loka spáin"))

  max_fit_data_level <- fit_data |>
    filter(date == max(date)) |>
    pull(level)

  max_fin_date <- max(fit_data$date)

  fc_level_tbl <- univar_level_mtbl %>%
    modeltime_refit(fit_data) |>
    modeltime_forecast(
      new_data = forecast_data,
      actual_data = bind_rows(
        fit_data,
        forecast_data
      )
    ) |>
    mutate(
      .model_desc = case_when(
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
    mutate(across(contains("level"), .fns = function(x) {
      ifelse(is.na(x), ACTUAL, x)
    })) %>%
    pivot_longer(cols = -c(.index)) %>%
    group_by(name) %>%
    mutate(growth = value / lag(value, roc[2]) - 1) %>%
    drop_na() %>%
    select(-value) %>%
    ungroup()

  # Remove historical values for the model
  fc_level_roc2_tbl <- fc_level_roc2_tbl %>%
    mutate(
      use = case_when(
        name == "ACTUAL" ~ "remove",
        .index <= max_fin_date ~ "remove",
        TRUE ~ "use"
      )
    ) %>%
    filter(use == "use") %>%
    select(-use)

  # 1.2.0 ROC 1 -------------------------------------------------------------

  fc_roc1_tbl <- univar_roc1_mtbl %>%
    modeltime_refit(fit_data) |>
    modeltime_forecast(
      new_data = forecast_data,
      actual_data = bind_rows(
        fit_data,
        forecast_data
      )
    ) |>
    mutate(
      .model_desc = case_when(
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
    mutate(base = max_fit_data_level) %>%
    group_by(.model_desc) %>%
    mutate(level = base * cumprod(1 + .value)) %>%
    ungroup() %>%
    select(-c(.value, base)) %>%
    bind_rows(
      full_data_for_initial_fit_tbl |>
        select(date, level) %>%
        mutate(.model_desc = "ACTUAL") %>%
        set_names(".index", "level", ".model_desc")
    ) %>%
    arrange(.index) %>%
    pivot_wider(names_from = .model_desc, values_from = level)

  # 1m roc forecast to 12m roc forecast
  fc_roc1_roc2_tbl <- fc_roc1_to_level_tbl %>%
    mutate(across(
      contains("roc1") | contains("ENSEMBLE"),
      .fns = function(x) ifelse(is.na(x), ACTUAL, x)
    )) %>%
    pivot_longer(cols = -c(.index)) %>%
    group_by(name) %>%
    mutate(growth = value / lag(value, roc[2]) - 1) %>%
    drop_na() %>%
    select(-value) %>%
    ungroup()

  # Remove historical values for the model
  fc_roc1_roc2_tbl <- fc_roc1_roc2_tbl %>%
    mutate(
      use = case_when(
        name == "ACTUAL" ~ "remove",
        .index <= max_fin_date ~ "remove",
        TRUE ~ "use"
      )
    ) %>%
    filter(use == "use") %>%
    select(-use)

  # 1.3.0 ROC 2 -------------------------------------------------------------

  fc_roc2_tbl <- univar_roc2_mtbl %>%
    modeltime_refit(fit_data) |>
    modeltime_forecast(
      new_data = forecast_data,
      actual_data = bind_rows(
        fit_data,
        forecast_data
      )
    ) |>
    mutate(
      .model_desc = case_when(
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

  # # Remove historical values for the model
  # fc_roc2_tbl <- fc_roc2_tbl %>%
  #   mutate(
  #     use = case_when(
  #       .index <= max_fin_date & name == "ACTUAL" ~ "remove",
  #       TRUE ~ "use"
  #     )
  #   ) %>%
  #   drop_na() |>
  #   filter(use == "use") %>%
  #   select(-use)

  # 1.4.0 log ---------------------------------------------------------------

  fc_log_tbl <- univar_log_mtbl %>%
    modeltime_refit(fit_data) |>
    modeltime_forecast(
      new_data = forecast_data,
      actual_data = bind_rows(
        fit_data,
        forecast_data
      )
    ) |>
    mutate(
      .model_desc = case_when(
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
    mutate(across(contains("log"), .fns = function(x) {
      ifelse(is.na(x), ACTUAL, x)
    })) %>%
    pivot_longer(cols = -c(.index)) %>%
    group_by(name) %>%
    mutate(growth = value / lag(value, roc[2]) - 1) %>%
    drop_na() %>%
    select(-value) %>%
    ungroup()

  # Remove history
  fc_log_roc2_tbl <- fc_log_roc2_tbl %>%
    mutate(
      use = case_when(
        name == "ACTUAL" ~ "remove",
        .index <= max_fin_date ~ "remove",
        TRUE ~ "use"
      )
    ) %>%
    filter(use == "use") %>%
    select(-use)

  all_future_fc_tbl <- bind_rows(
    fc_level_roc2_tbl,
    fc_roc1_roc2_tbl,
    fc_roc2_tbl,
    fc_log_roc2_tbl
  ) %>%
    distinct()
}
