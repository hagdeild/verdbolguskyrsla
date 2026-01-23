# ==============================================================================
# CONFIGURATION
# ==============================================================================

MODEL_SPECS <- list(
  arima = list(
    spec_fn = function() modeltime::arima_reg(),
    engine = "auto_arima"
  ),
  ets = list(
    spec_fn = function() modeltime::exp_smoothing(),
    engine = "ets"
  ),
  tbats = list(
    spec_fn = function() modeltime::seasonal_reg(),
    engine = "tbats"
  ),
  thief = list(
    spec_fn = function() {
      modeltime::temporal_hierarchy(
        use_model = "arima",
        combination_method = "struc"
      )
    },
    engine = "thief"
  ),
  stl = list(
    spec_fn = function() modeltime::seasonal_reg(),
    engine = "stlm_arima"
  )
)

MODEL_DESC_LOOKUP <- c(
  "SEASONAL DECOMP" = "STL",
  "ARIMA" = "ARIMA",
  "ETS" = "ETS",
  "BATS" = "TBATS",
  "TEMPORAL" = "THIEF"
)

TRANSFORMATIONS <- c("level", "log", "roc_1", "roc_2")

# ==============================================================================
# HELPER FUNCTIONS
# ==============================================================================

#' Standardize model description from modeltime output
#' @param desc Character vector of raw model descriptions
#' @param transformation Character: transformation suffix to add
standardize_model_desc <- function(desc, transformation) {
  sapply(
    desc,
    function(d) {
      if (d == "ACTUAL") {
        return("ACTUAL")
      }
      for (pattern in names(MODEL_DESC_LOOKUP)) {
        if (grepl(pattern, d, ignore.case = TRUE)) {
          return(paste0(MODEL_DESC_LOOKUP[pattern], "-", transformation))
        }
      }
      d
    },
    USE.NAMES = FALSE
  )
}

#' Fit a single model on a single transformation
#' @param model_name Character: "arima", "ets", etc.
#' @param transformation Character: "level", "log", "roc_1", "roc_2"
#' @param train_data Tibble with date and transformation columns
#' @return A modeltime_table with 1 row
fit_single_model <- function(model_name, transformation, train_data) {
  spec <- MODEL_SPECS[[model_name]]
  model_desc <- paste0(toupper(model_name), "-", transformation)
  formula <- stats::as.formula(paste(transformation, "~ date"))

  spec$spec_fn() |>
    parsnip::set_engine(spec$engine) |>
    parsnip::fit(formula, data = train_data) |>
    modeltime::modeltime_table() |>
    dplyr::mutate(.model_desc = model_desc)
}

#' Fit all model-transformation combinations
#' @param train_data Tibble with date and all transformation columns
#' @param model_names Character vector of model names to fit
#' @param transformations Character vector of transformations
#' @param parallel Logical: use parallel processing with furrr
#' @return Named list of modeltime_tables, grouped by transformation
fit_all_models <- function(
  train_data,
  model_names = names(MODEL_SPECS),
  transformations = TRANSFORMATIONS,
  parallel = TRUE
) {
  combos <- tidyr::expand_grid(
    model = model_names,
    transformation = transformations
  )

  if (parallel && requireNamespace("furrr", quietly = TRUE)) {
    cli::cli_alert_info("Fitting models in parallel...")
    future::plan(
      future::multisession,
      workers = max(1, parallel::detectCores() - 1)
    )
    on.exit(future::plan(future::sequential), add = TRUE)

    results <- furrr::future_pmap(
      combos,
      function(model, transformation) {
        fit_single_model(model, transformation, train_data)
      },
      .options = furrr::furrr_options(
        seed = TRUE,
        packages = c("modeltime", "parsnip", "forecast", "thief", "dplyr")
      ),
      .progress = TRUE
    )
  } else {
    cli::cli_alert_info("Fitting models sequentially...")
    results <- purrr::pmap(
      combos,
      function(model, transformation) {
        cli::cli_text("  {model}-{transformation}")
        fit_single_model(model, transformation, train_data)
      }
    )
  }

  combos$result <- results

  # Group by transformation and combine
  split(combos, combos$transformation) |>
    lapply(function(df) {
      do.call(modeltime::combine_modeltime_tables, df$result)
    })
}

#' Compute shrinkage-adjusted inverse-RMSE weights for ensemble
#' @param accuracy_tbl Tibble with columns: name, rmse (from CV loop)
#' @param models Character vector of model names to weight
#' @param shrinkage Numeric 0-1, how much to shrink toward equal weights
#' @return Named numeric vector of weights summing to 1
compute_ensemble_weights <- function(accuracy_tbl, models, shrinkage = 0.5) {
  model_rmse <- accuracy_tbl |>
    dplyr::filter(name %in% models) |>
    dplyr::group_by(name) |>
    dplyr::summarise(mean_rmse = mean(rmse, na.rm = TRUE), .groups = "drop")

  # Inverse-RMSE weights (raw)
  raw_weights <- 1 / model_rmse$mean_rmse
  raw_weights <- raw_weights / sum(raw_weights)

  # Equal weights
  equal_weights <- rep(1 / length(models), length(models))

  # Shrinkage blend
  final_weights <- shrinkage * equal_weights + (1 - shrinkage) * raw_weights

  names(final_weights) <- model_rmse$name
  final_weights[models] # Ensure correct order
}

#' Generate forecast and transform to target metric (12m roc)
#' @param model_tbl Modeltime table
#' @param fit_data Training data
#' @param forecast_data Data to forecast on
#' @param transformation Which transformation this model uses
#' @param roc ROC periods vector
#' @param full_history_tbl Full historical data for level reference
#' @param max_fit_date Last date of fit data
#' @param refit Logical: whether to refit models
forecast_and_transform <- function(
  model_tbl,
  fit_data,
  forecast_data,
  transformation,
  roc,
  full_history_tbl,
  max_fit_date,
  refit = TRUE
) {
  # Get level at max fit date for roc_1 conversion

  max_fit_data_level <- fit_data |>
    dplyr::filter(date == max(date)) |>
    dplyr::pull(level)

  # Refit if needed and forecast
  if (refit) {
    fc_raw <- model_tbl |>
      modeltime::modeltime_refit(fit_data) |>
      modeltime::modeltime_forecast(
        new_data = forecast_data,
        actual_data = dplyr::bind_rows(fit_data, forecast_data)
      )
  } else {
    fc_raw <- model_tbl |>
      modeltime::modeltime_forecast(
        new_data = forecast_data,
        actual_data = dplyr::bind_rows(fit_data, forecast_data)
      )
  }

  # Standardize model descriptions

  fc_raw <- fc_raw |>
    dplyr::mutate(
      .model_desc = standardize_model_desc(.model_desc, transformation)
    )

  # Transform based on type
  if (transformation == "level") {
    result <- transform_level_to_roc2(fc_raw, roc)
  } else if (transformation == "log") {
    result <- transform_log_to_roc2(fc_raw, roc)
  } else if (transformation == "roc_1") {
    result <- transform_roc1_to_roc2(
      fc_raw,
      full_history_tbl,
      roc,
      max_fit_data_level
    )
  } else if (transformation == "roc_2") {
    result <- fc_raw |>
      dplyr::select(.index, .model_desc, .value) |>
      purrr::set_names(".index", "name", "growth")
  }

  # Filter to forecast period only (remove historical values for models, keep ACTUAL)
  result |>
    dplyr::mutate(
      use = dplyr::case_when(
        name == "ACTUAL" ~ "use",
        .index <= max_fit_date ~ "remove",
        TRUE ~ "use"
      )
    ) |>
    dplyr::filter(use == "use") |>
    dplyr::select(-use)
}

#' Transform level forecast to 12m rate of change
transform_level_to_roc2 <- function(fc_tbl, roc) {
  fc_tbl |>
    dplyr::select(-c(.model_id, .key)) |>
    tidyr::pivot_wider(names_from = .model_desc, values_from = .value) |>
    dplyr::mutate(dplyr::across(
      dplyr::contains("level"),
      .fns = function(x) ifelse(is.na(x), ACTUAL, x)
    )) |>
    tidyr::pivot_longer(cols = -c(.index)) |>
    dplyr::group_by(name) |>
    dplyr::mutate(growth = value / dplyr::lag(value, roc[2]) - 1) |>
    tidyr::drop_na() |>
    dplyr::select(-value) |>
    dplyr::ungroup()
}

#' Transform log forecast to 12m rate of change
transform_log_to_roc2 <- function(fc_tbl, roc) {
  fc_tbl |>
    dplyr::mutate(.value = exp(.value)) |>
    dplyr::select(-c(.model_id, .key)) |>
    tidyr::pivot_wider(names_from = .model_desc, values_from = .value) |>
    dplyr::mutate(dplyr::across(
      dplyr::contains("log"),
      .fns = function(x) ifelse(is.na(x), ACTUAL, x)
    )) |>
    tidyr::pivot_longer(cols = -c(.index)) |>
    dplyr::group_by(name) |>
    dplyr::mutate(growth = value / dplyr::lag(value, roc[2]) - 1) |>
    tidyr::drop_na() |>
    dplyr::select(-value) |>
    dplyr::ungroup()
}

#' Transform 1m roc forecast to 12m rate of change
transform_roc1_to_roc2 <- function(fc_tbl, full_history_tbl, roc, base_level) {
  # Convert roc_1 to level
  fc_to_level <- fc_tbl |>
    dplyr::select(-c(.model_id, .key)) |>
    dplyr::filter(!.model_desc == "ACTUAL") |>
    dplyr::mutate(base = base_level) |>
    dplyr::group_by(.model_desc) |>
    dplyr::mutate(level = base * cumprod(1 + .value)) |>
    dplyr::ungroup() |>
    dplyr::select(-c(.value, base)) |>
    dplyr::bind_rows(
      full_history_tbl |>
        dplyr::select(date, level) |>
        dplyr::mutate(.model_desc = "ACTUAL") |>
        purrr::set_names(".index", "level", ".model_desc")
    ) |>
    dplyr::arrange(.index) |>
    tidyr::pivot_wider(names_from = .model_desc, values_from = level)

  # Convert level to 12m roc
  fc_to_level |>
    dplyr::mutate(dplyr::across(
      dplyr::contains("roc1") | dplyr::contains("ENSEMBLE"),
      .fns = function(x) ifelse(is.na(x), ACTUAL, x)
    )) |>
    tidyr::pivot_longer(cols = -c(.index)) |>
    dplyr::group_by(name) |>
    dplyr::mutate(growth = value / dplyr::lag(value, roc[2]) - 1) |>
    tidyr::drop_na() |>
    dplyr::select(-value) |>
    dplyr::ungroup()
}

# ==============================================================================
# ENSEMBLE FUNCTIONS
# ==============================================================================

#' Create ensemble forecasts
#' @param data Forecast tibble with columns: .index, name, growth
#' @param ensembles List of character vectors, each specifying models to combine
#' @param weights_list Optional list of named weight vectors (same length as ensembles)
#' @param na.rm Remove NA when computing means
#' @param prefix Prefix for ensemble names
create_ensembles <- function(
  data,
  ensembles,
  weights_list = NULL,
  na.rm = TRUE,
  prefix = "ensemble_"
) {
  max_date <- data |>
    dplyr::filter(name != "ACTUAL") |>
    dplyr::filter(.index == min(.index)) |>
    dplyr::pull(.index) |>
    unique()

  data_ensemble <- data |>
    dplyr::arrange(.index) |>
    tidyr::pivot_wider(names_from = name, values_from = growth) |>
    dplyr::filter(.index >= max_date)

  out <- data_ensemble
  long_idx <- 0L

  for (i in seq_along(ensembles)) {
    mods <- ensembles[[i]]

    # Name logic
    if (length(mods) > 3L) {
      long_idx <- long_idx + 1L
      new_name <- paste0("ensemble_long_", long_idx)
    } else {
      new_name <- paste0(prefix, paste(mods, collapse = "_"))
    }

    # Use weights if provided, otherwise equal weights
    if (!is.null(weights_list) && !is.null(weights_list[[i]])) {
      weights <- weights_list[[i]][mods]
      mat <- as.matrix(dplyr::select(out, dplyr::all_of(mods)))
      out[[new_name]] <- drop(mat %*% weights)
    } else {
      out[[new_name]] <- rowMeans(
        dplyr::select(out, dplyr::all_of(mods)),
        na.rm = na.rm
      )
    }
  }

  out |>
    tidyr::pivot_longer(cols = -c(.index)) |>
    dplyr::rename("growth" = "value") |>
    dplyr::bind_rows(data |> dplyr::filter(name == "ACTUAL")) |>
    dplyr::arrange(.index) |>
    dplyr::distinct()
}

# ==============================================================================
# CREATE FORECAST (for CV loop and final forecast)
# ==============================================================================

#' Create forecasts for all transformations
#' @param fit_data Training data
#' @param forecast_data Data to forecast on
#' @param model_tables Named list of modeltime tables (level, roc_1, roc_2, log)
#' @param roc ROC periods
#' @param full_data_for_initial_fit_tbl Full historical data
create_forecast <- function(
  fit_data,
  forecast_data,
  model_tables,
  roc,
  full_data_for_initial_fit_tbl
) {
  cli::cli_h3("Creating forecasts")

  max_fit_date <- max(fit_data$date)

  all_fc <- lapply(names(model_tables), function(trans) {
    forecast_and_transform(
      model_tbl = model_tables[[trans]],
      fit_data = fit_data,
      forecast_data = forecast_data,
      transformation = trans,
      roc = roc,
      full_history_tbl = full_data_for_initial_fit_tbl,
      max_fit_date = max_fit_date,
      refit = TRUE
    )
  })

  dplyr::bind_rows(all_fc) |> dplyr::distinct(.index, name, .keep_all = TRUE)
}

# ==============================================================================
# MAIN FUNCTION
# ==============================================================================

get_univariate_forecasts <- function(
  data,
  transformation = c("level", "log"),
  roc = c(1, 12),
  freq = "month",
  horizon = 12,
  slice_limit = 3,
  skip = 1,
  accuracy_at_level = "roc_2",
  parallel = TRUE,
  ensemble = TRUE
) {
  # SETUP ----
  if (sum(stringr::str_detect(transformation, "level")) > 0) {
    data_prep_tbl <- data |>
      dplyr::mutate(level = outcome)
  }

  if (sum(stringr::str_detect(transformation, "log")) > 0) {
    data_prep_tbl <- data_prep_tbl |>
      dplyr::mutate(log = log(outcome))
  }

  if (length(roc) != 0) {
    data_prep_tbl <- data_prep_tbl |>
      dplyr::mutate(
        roc_1 = outcome / dplyr::lag(outcome, roc[1]) - 1,
        roc_2 = outcome / dplyr::lag(outcome, roc[2]) - 1
      ) |>
      tidyr::drop_na()
  }

  data_prep_tbl <- data_prep_tbl |>
    dplyr::select(-outcome)

  # Future data
  full_data_tbl <- data_prep_tbl |>
    timetk::future_frame(
      .date_var = date,
      .length_out = horizon,
      .bind_data = TRUE
    ) |>
    tidyr::fill(id, .direction = "down")

  future_data_tbl <- full_data_tbl |>
    tail(horizon)

  prepared_data_tbl <- full_data_tbl |>
    dplyr::filter(!date %in% future_data_tbl$date) |>
    tidyr::drop_na()

  # Splits
  splits <- prepared_data_tbl |>
    timetk::time_series_cv(
      date_var = date,
      assess = horizon,
      skip = skip,
      slice_limit = slice_limit + 1,
      cumulative = TRUE
    )

  train_data_for_initial_fit_tbl <- splits$splits[[nrow(splits)]] |>
    rsample::training()
  test_data_for_initial_fit_tbl <- splits$splits[[nrow(splits)]] |>
    rsample::testing()
  full_data_for_initial_fit_tbl <- dplyr::bind_rows(
    train_data_for_initial_fit_tbl,
    test_data_for_initial_fit_tbl
  )

  # FIT ALL MODELS ----
  cli::cli_h2("Fitting models")

  model_tables <- fit_all_models(
    train_data = train_data_for_initial_fit_tbl,
    model_names = names(MODEL_SPECS),
    transformations = TRANSFORMATIONS,
    parallel = parallel
  )

  # INITIAL FORECASTS ----
  cli::cli_h2("Initial forecasts")

  max_roc1_date <- train_data_for_initial_fit_tbl |>
    dplyr::filter(date == max(date)) |>
    dplyr::pull(date)

  # Generate initial forecasts for test set 1
  all_initial_fc_tbl <- lapply(names(model_tables), function(trans) {
    forecast_and_transform(
      model_tbl = model_tables[[trans]],
      fit_data = train_data_for_initial_fit_tbl,
      forecast_data = test_data_for_initial_fit_tbl,
      transformation = trans,
      roc = roc,
      full_history_tbl = full_data_for_initial_fit_tbl,
      max_fit_date = max_roc1_date,
      refit = FALSE
    )
  }) |>
    dplyr::bind_rows() |>
    dplyr::distinct(.index, name, .keep_all = TRUE)

  # Accuracy ----
  all_initial_accuracy_tbl <- all_initial_fc_tbl |>
    tidyr::pivot_wider(names_from = name, values_from = growth) |>
    dplyr::filter(.index >= min(test_data_for_initial_fit_tbl$date)) |>
    tidyr::pivot_longer(
      cols = -c(.index, ACTUAL),
      names_to = "name",
      values_to = "estimate"
    ) |>
    dplyr::group_by(name) |>
    modeltime::summarize_accuracy_metrics(
      truth = ACTUAL,
      estimate = estimate,
      metric_set = yardstick::metric_set(yardstick::rmse)
    ) |>
    dplyr::arrange(rmse)

  # Create ensembles before test set loop ----
  if (ensemble) {
    top_2 <- all_initial_accuracy_tbl |>
      dplyr::slice_head(n = 2) |>
      dplyr::pull(name)

    top_3 <- all_initial_accuracy_tbl |>
      dplyr::slice_head(n = 3) |>
      dplyr::pull(name)

    all_models <- unique(all_initial_accuracy_tbl$name)

    ensembles <- list(top_2, top_3, all_models)
  } else {
    ensembles <- list()
  }

  # CV LOOP ----
  cli::cli_h2("Cross-validation")

  forecast_ls <- list()
  accuracy_ls <- list()

  for (i in nrow(splits):2) {
    cli::cli_h3(glue::glue("Resample: {i} of {nrow(splits)}"))

    train_loop_tbl <- splits$splits[[i]] |> rsample::training()
    test_loop_tbl <- splits$splits[[i]] |> rsample::testing()

    all_fc_tbl <- create_forecast(
      fit_data = train_loop_tbl,
      forecast_data = test_loop_tbl,
      model_tables = model_tables,
      roc = roc,
      full_data_for_initial_fit_tbl = full_data_for_initial_fit_tbl
    )

    # Create ensemble (simple averaging during CV)
    if (ensemble && length(ensembles) > 0) {
      ensemble_fc_tbl <- create_ensembles(
        data = all_fc_tbl,
        ensembles = ensembles,
        weights_list = NULL
      )
    } else {
      ensemble_fc_tbl <- all_fc_tbl
    }

    forecast_ls[[i]] <- ensemble_fc_tbl |>
      dplyr::mutate(resample = paste0("resample_", i))

    # Accuracy
    all_accuracy_tbl <- ensemble_fc_tbl |>
      tidyr::pivot_wider(names_from = name, values_from = growth) |>
      dplyr::filter(.index >= min(test_data_for_initial_fit_tbl$date)) |>
      tidyr::pivot_longer(
        cols = -c(.index, ACTUAL),
        names_to = "name",
        values_to = "estimate"
      ) |>
      dplyr::group_by(name) |>
      modeltime::summarize_accuracy_metrics(
        truth = ACTUAL,
        estimate = estimate,
        metric_set = yardstick::metric_set(yardstick::rmse)
      ) |>
      dplyr::arrange(rmse) |>
      dplyr::mutate(resample = paste0("resample_", i))

    accuracy_ls[[i]] <- all_accuracy_tbl
  }

  final_accuracy_tbl <- accuracy_ls |>
    dplyr::bind_rows() |>
    dplyr::group_by(name) |>
    dplyr::summarise(
      mean = mean(rmse),
      sd = sd(rmse),
      .groups = "drop"
    ) |>
    dplyr::arrange(mean)

  # COMPUTE ENSEMBLE WEIGHTS ----
  weights_list <- NULL
  if (ensemble && length(ensembles) > 0) {
    cli::cli_alert_info("Computing ensemble weights from CV accuracy...")
    all_cv_accuracy <- dplyr::bind_rows(accuracy_ls)
    weights_list <- lapply(ensembles, function(mods) {
      compute_ensemble_weights(
        accuracy_tbl = all_cv_accuracy,
        models = mods,
        shrinkage = 0.5
      )
    })
  }

  # FINAL FORECAST ----
  cli::cli_h2("Final forecast")

  fc_tbl <- create_forecast(
    fit_data = data_prep_tbl,
    forecast_data = future_data_tbl,
    model_tables = model_tables,
    roc = roc,
    full_data_for_initial_fit_tbl = full_data_for_initial_fit_tbl
  )

  if (ensemble && length(ensembles) > 0) {
    future_ensemble_tbl <- create_ensembles(
      data = fc_tbl,
      ensembles = ensembles,
      weights_list = weights_list
    )
  } else {
    future_ensemble_tbl <- fc_tbl
  }

  # Get the most accurate one
  best_model <- final_accuracy_tbl |>
    dplyr::slice_head(n = 1) |>
    dplyr::pull(name)

  best_future_fc_tbl <- future_ensemble_tbl |>
    dplyr::mutate(
      keep = dplyr::case_when(
        name == "ACTUAL" ~ "keep",
        name %in% best_model ~ "keep",
        TRUE ~ "remove"
      )
    ) |>
    dplyr::filter(keep == "keep") |>
    dplyr::select(-keep)

  return_list <- list(
    "future_fc" = best_future_fc_tbl,
    "Accuracy" = final_accuracy_tbl,
    "tscv" = forecast_ls |> dplyr::bind_rows(),
    "ensemble_weights" = if (ensemble) weights_list else NULL,
    "best_model" = best_model
  )

  return(return_list)
}
