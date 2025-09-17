# NEURALFORECAST ----

#' Inference for the NEURALFORECAST Time Series Models
#' @param train_data Tibble with train data
#' @param test_data Tibble with test data
#' @param future_data Tibble with future data
#' @param horizon Numeric. The forecast horizon. No default.
#' @param input_multiple Numeric. Relative size of the input_size. Defaults to 2
#' @param max_steps Numeric. Maximum number of training steps. Defaults to 200.
#' @param hist_exog_data Tibble with hist_exog_features
#' @param static_df Tibble with static features
#' @param nixtla_dl_models Neuralforecast models to use
#' @param frequency Character month.
#'
#' @export

get_neuralforecast <- function(train_data = NULL,
                               test_data = NULL,
                               future_data = NULL,
                               horizon = NULL,
                               input_multiple = 2,
                               max_steps = 1000,
                               transformation = NULL,
                               hist_exog_data = NULL,
                               static_df = NULL,
                               nixtla_dl_models,
                               frequency,
                               negative_to_zero
                               ) {
  if (is.null(train_data)) {
    stop("train_data missing")
  }

  if (is.null(test_data) & is.null(future_data)) {
    stop("Need to supply either test data or future_data")
  }

  if (!is.null(test_data) & !is.null(future_data)) {
    stop("Please supply either test data or future_data, not  both")
  }

  if (!is.null(test_data)) {
    date_to_int_tbl <- bind_rows(train_data, test_data) %>%
      dplyr::select(date) %>%
      dplyr::distinct() %>%
      dplyr::arrange(date) %>%
      dplyr::mutate(ds = row_number())
  } else {
    date_to_int_tbl <- train_data %>%
      dplyr::select(date) %>%
      dplyr::distinct() %>%
      timetk::future_frame(.date_var = date, .length_out = horizon, .bind_data = TRUE) %>%
      dplyr::arrange(date) %>%
      dplyr::mutate(ds = row_number())
  }


  # Prepare the train data
  train_data <- train_data %>%
    dplyr::select(id, date, outcome) %>%
    dplyr::mutate(
      outcome =
        dplyr::case_when(
          transformation == "log" ~ exp(outcome),
          transformation == "log1p" ~ expm1(outcome),
          TRUE ~ outcome
        )
    ) %>%
    dplyr::left_join(date_to_int_tbl) %>%
    dplyr::select(id, ds, outcome) %>%
    purrr::set_names("unique_id", "ds", "y") %>%
    tidyr::drop_na()


  if (!is.null(hist_exog_data)) {
    hist_exog_data <- hist_exog_data %>%
      dplyr::left_join(date_to_int_tbl) %>%
      select(-date) %>%
      dplyr::rename("unique_id" = "id")

    hist_exog_ls <- names(hist_exog_data)
    hist_exog_ls <- hist_exog_ls[!hist_exog_ls %in% c("unique_id", "ds")]
    hist_exog_ls <- as.list(hist_exog_ls)


    train_data <- train_data %>%
      dplyr::left_join(hist_exog_data)
  } else {
    hist_exog_ls <- NULL
  }

  if (!is.null(static_df)) {
    static_df <- static_df %>%
      dplyr::rename("unique_id" = "id")

    stat_exog_ls <- names(static_df)
    stat_exog_ls <- stat_exog_ls[!stat_exog_ls %in% c("unique_id", "ds")]
    stat_exog_ls <- as.list(stat_exog_ls)
  } else {
    stat_exog_ls <- NULL
  }



  # Prepare the test data
  if (!is.null(test_data)) {
    test_data <- test_data %>%
      dplyr::select(id, date, outcome) %>%
      dplyr::mutate(
        outcome =
          dplyr::case_when(
            transformation == "log" ~ exp(outcome),
            transformation == "log1p" ~ expm1(outcome),
            TRUE ~ outcome
          )
      ) %>%
      dplyr::left_join(date_to_int_tbl) %>%
      dplyr::select(id, ds, outcome) %>%
      purrr::set_names("unique_id", "ds", "y") %>%
      dplyr::filter(unique_id %in% train_data$unique_id)
  }


  # Model setup
  cli::cli_h1("Set up nixtla models")

  nf <- pkg.env$nf

  horizon <- as.integer(horizon)
  input_size <- as.integer(horizon * input_multiple)

  models <- create_neuralforecast_models(
    nf            = nf,
    models_to_run = nixtla_dl_models,
    input_size    = input_size,
    horizon       = horizon,
    max_steps     = max_steps,
    hist_exog_ls  = hist_exog_ls,
    stat_exog_ls  = stat_exog_ls
  )

  # stage models
  cli::cli_h3("Set up nixtla model")
  nf_set <- neural_model_setup(nf = nf, models = models, frequency = as.integer(1))

  # fit models
  cli::cli_h3("Fit nixtla model")

  if (is.null(static_df)) {
    nf_fit <- nf_set$fit(df = train_data)
  } else {
    nf_fit <- nf_set$fit(df = train_data, static_df = static_df)
  }

  # make predictions
  cli::cli_h3("Make prediction with nixtla model")

  nf_preds <- nf_set$predict(nf_fit)

  nf_preds_tbl <- reticulate::py_to_r(nf_preds$reset_index()) %>%
    tibble::as_tibble()


  nf_preds_tbl <- nf_preds_tbl %>%
    tidyr::pivot_longer(cols = -c(ds, unique_id)) %>%
    dplyr::rename(".model_desc" = "name") %>%
    dplyr::rename(".value" = "value") %>%
    dplyr::mutate(
      .key        = "prediction",
      .model_id   = 1
    ) %>%
    dplyr::rename(".index" = "ds") %>%
    dplyr::rename("id" = "unique_id")


  # modify train data
  if (!is.null(test_data)) {
    full_data <- train_data %>%
      bind_rows(test_data) %>%
      dplyr::mutate(
        .key = "actual",
        .model_desc = "ACTUAL",
        .model_id = NA_real_
      ) %>%
      dplyr::rename(".value" = "y") %>%
      dplyr::rename("id" = "unique_id") %>%
      dplyr::rename(".index" = "ds") %>%
      dplyr::select(.model_id, .model_desc, .key, id, .index, .value)
  } else {
    full_data <- train_data %>%
      dplyr::mutate(
        .key = "actual",
        .model_desc = "ACTUAL",
        .model_id = NA_real_
      ) %>%
      dplyr::rename(".value" = "y") %>%
      dplyr::rename("id" = "unique_id") %>%
      dplyr::rename(".index" = "ds") %>%
      dplyr::select(.model_id, .model_desc, .key, id, .index, .value)
  }


  fc_tbl <- nf_preds_tbl %>%
    dplyr::bind_rows(full_data) %>%
    dplyr::arrange(id, .index)

  # Int to date
  fc_tbl <- fc_tbl %>%
    dplyr::rename("ds" = ".index") %>%
    dplyr::left_join(date_to_int_tbl) %>%
    dplyr::select(-ds) %>%
    dplyr::rename(".index" = "date")

  return(fc_tbl)
}



# HELPER FUNCITONS ----


# 1.0 Import library ----
import_neuralforecast <- function() {
  nf <- reticulate::import("neuralforecast")

  return(nf)
}

neural_model_setup <- function(nf, models = models, frequency) {
  nf$NeuralForecast(models = models, freq = frequency)
}

# 2.0 Fitting functions ----
neural_model_fit <- function(model_setup = neural_model_setup, df = train_data) {
  model_setup$fit(df = df)
}

# 3.0 Forecsting function ----
neural_model_predict <- function(model_setup = neural_model_setup, model_fit = neural_model_fit) {
  model_setup$predict(
    model_fit
  )
}


# 4.0 Time series cross validation -----

#' Time series cross validation for nhits
#' @param data Tibble.
tscv_neuralforecast <- function(data,
                                horizon,
                                slice_limit,
                                skip,
                                transformation,
                                input_multiple,
                                max_steps,
                                hist_exog_data,
                                static_df,
                                nixtla_dl_models,
                                negative_to_zero
                                ) {
  data <- data %>%
    dplyr::filter(!is.na(outcome))

  # Skip
  if (skip == "auto") {
    skip <- round(horizon / 4)
  }

  # Create splits
  splits <- split_data(
    data        = data,
    horizon     = horizon,
    slice_limit = slice_limit,
    skip        = skip
  )


  # TSCV
  nhits_tscv_ls <- list()

  for (i in 1:nrow(splits)) {
    split_use <- splits$splits[[i]]

    train_data_tbl <- split_use %>%
      rsample::training() %>%
      arrange(id, date)
    test_data_tbl <- split_use %>%
      rsample::testing() %>%
      arrange(id, date)

    # Rename for NHITS
    train_data_tbl <- train_data_tbl %>%
      dplyr::select(date, id, outcome) %>%
      dplyr::mutate(id = as.character(id))
    # tidyr::complete(id, date) %>%
    # dplyr::mutate(outcome = if_else(is.na(outcome), 0, outcome))

    test_data_tbl <- test_data_tbl %>%
      dplyr::select(date, id, outcome) %>%
      dplyr::mutate(id = as.character(id)) %>%
      tidyr::complete(id, date) %>%
      dplyr::mutate(outcome = if_else(is.na(outcome), 0, outcome))


    tscv_nhits <- get_neuralforecast(
      train_data       = train_data_tbl,
      test_data        = test_data_tbl,
      horizon          = horizon,
      input_multiple   = input_multiple,
      max_steps        = max_steps,
      transformation   = transformation,
      hist_exog_data   = hist_exog_data,
      static_df        = static_df,
      nixtla_dl_models = nixtla_dl_models,
      frequency        = frequency,
      negative_to_zero = negative_to_zero
    )


    nhits_tscv_ls[[i]] <- tscv_nhits %>% dplyr::mutate(resample = paste0("resample_", i))
  }

  dplyr::bind_rows(nhits_tscv_ls)
}


get_dl_frequency <- function(frequency) {
  dl_frequency <- switch(frequency,
    "year"    = 1,
    "quarter" = 4,
    "month"   = 12,
    "week"    = 52,
    "day"     = 7,
    "hour"    = 24
  )

  return(as.integer(dl_frequency))
}


#' Check and Activate Conda Environment
#'
#' This function checks if the specified Conda environment exists and activates it.
#' It's designed to ensure that the necessary Python environment is ready before
#' any Python interaction.
#'
#' @param env_name The name of the Conda environment to be activated.
#' @return Invisible NULL, printing messages to the console.
check_and_activate_env <- function(env_name = "r-neuralforecast") {
  if (!reticulate::condaenv_exists(env_name)) {
    stop("The specified Conda environment does not exist. Please run 'install_neuralforecast()' or check the environment name.")
  } else {
    message("Activating Conda environment: ", env_name)
    reticulate::use_condaenv(env_name, required = TRUE)
  }
}


#' Create hist_exog_list

create_hist_exog <- function(data, var_name) {
  # Convert var_name string to a symbol for tidy evaluation
  var_name_sym <- rlang::sym(var_name)

  data %>%
    tidyr::complete(id, date) %>%
    tidyr::fill(-outcome, .direction = "downup") %>%
    dplyr::mutate(outcome = ifelse(is.na(outcome), 0, outcome)) %>%
    dplyr::arrange(id, date) %>%
    dplyr::group_by(id) %>%
    sumo_future_frame(date_col = "date", .n_future = 12, freq = "month") %>%
    dplyr::group_by(date, !!var_name_sym) %>%
    dplyr::mutate(
      "{var_name}_mean" := mean(outcome, na.rm = TRUE)
    ) %>%
    dplyr::ungroup()
}


#' Create Neural Forecast Models
#'
#' This function dynamically constructs a list of neural forecast models based on the specified input.
#' Currently, it supports `NHITS` and `NBEATSX` models from the neural forecasting framework.
#' Additional models can be incorporated by extending the conditional statements within the function.
#'
#' @param models_to_run A character vector specifying the models to be created.
#'  Supported values include "nhits" and "nbeatsx". This allows for flexibility
#'  in model creation based on user needs.
#' @param input_size An integer specifying the input size for the model. This parameter defines
#'  the number of time steps to be used as input for forecasting.
#' @param horizon An integer specifying the forecast horizon. This determines how many time steps
#'  into the future the model should predict.
#' @param max_steps An optional integer specifying the maximum steps for the NHITS model training process.
#'  Defaults to 1000. This parameter controls the training effort and computational cost.
#' @param hist_exog_ls A list of external regressors to be used by the models. This parameter allows
#'  for the inclusion of additional historical exogenous variables that can help
#'  improve forecast accuracy.
#'
#' @return A list containing the specified forecast models. If only one model is specified,
#'  the function returns the model directly rather than wrapping it in a list. This
#'  ensures compatibility with functions expecting a model object.
#'
#' @examples
#' # Create both NHITS and NBEATSX models
#' models <- create_neuralforecast_models(c("nhits", "nbeatsx"), 24, 12, 1000, list("example_exog"))
#'
#' # Create only the NHITS model
#' nhits_model <- create_neuralforecast_models(c("nhits"), 24, 12, 1000, list("example_exog"))
#'
#' @export
create_neuralforecast_models <- function(nf,
                                         models_to_run,
                                         input_size,
                                         horizon,
                                         max_steps = 1000,
                                         hist_exog_ls,
                                         stat_exog_ls
                                         ) {
  models_list <- list()

  if ("nhits" %in% models_to_run) {
    NHITS <- nf$models$NHITS

    nhits_model <- NHITS(
      h              = as.integer(horizon),
      input_size     = as.integer(input_size),
      max_steps      = max_steps,
      hist_exog_list = hist_exog_ls,
      stat_exog_list = stat_exog_ls,
      batch_size     = as.integer(1024),
      logger         = FALSE
    )
    models_list <- c(models_list, list(nhits_model))
  }

  if ("nbeatsx" %in% models_to_run) {
    NBEATSx <- nf$models$NBEATSx

    nbeatsx_model <- NBEATSx(
      h              = as.integer(horizon),
      input_size     = as.integer(input_size),
      max_steps      = max_steps,
      hist_exog_list = hist_exog_ls,
      stat_exog_list = stat_exog_ls,
      logger         = FALSE
    )
    models_list <- c(models_list, list(nbeatsx_model))
  }

  if ("patchtst" %in% models_to_run) {
    PATCHTST <- nf$models$PatchTST

    patchtst_model <- PATCHTST(
      h              = as.integer(horizon),
      input_size     = as.integer(input_size),
      max_steps      = max_steps,
      hist_exog_list = hist_exog_ls,
      stat_exog_list = stat_exog_ls,
      logger         = FALSE
    )
    models_list <- c(models_list, list(patchtst_model))
  }

  if ("kan" %in% models_to_run) {
    kan <- nf$models$KAN

    kan_model <- kan(
      h              = as.integer(horizon),
      input_size     = as.integer(input_size),
      max_steps      = max_steps,
      hist_exog_list = hist_exog_ls,
      stat_exog_list = stat_exog_ls,
      logger         = FALSE
    )

    models_list <- c(models_list, list(kan_model))
  }

  # if ("timemixer" %in% models_to_run) {
  #   timemixer <- nf$models$TimeMixer
  #
  #   timemixer_model <- timemixer(
  #     h              = as.integer(horizon),
  #     input_size     = as.integer(input_size),
  #     max_steps      = max_steps,
  #     n_series       = n_series,
  #     hist_exog_list = hist_exog_ls,
  #     stat_exog_list = stat_exog_ls,
  #     logger         = FALSE
  #   )
  #
  #   models_list <- c(models_list, list(timemixer_model))
  # }


  # Flatten the list if it contains only one model to match your specific requirement
  # if (length(models_list) == 1) {
  #   return(models_list[[1]])
  # } else {
  #   return(models_list)
  # }

  return(models_list)
}



#' Install neuralforecast Environment
#'
#' This function sets up a conda environment named `r-neuralforecast` and installs the `neuralforecast` Python package along with its dependencies. Optionally, PyTorch can also be installed in this environment.
#'
#' @param fresh_install Logical, if `TRUE`, any existing `r-neuralforecast` conda environment will be removed and a new one will be created for a fresh install. Defaults to `FALSE`.
#' @return Invisible `FALSE` if Conda is not found and the installation cannot proceed, otherwise, the function proceeds with the installation without a specific return value.
#' @details Before proceeding with the installation, this function checks for the presence of Conda on the system using `check_conda()`. If Conda is not found, the function provides instructions for installing Conda and then halts further execution. If a fresh installation is requested, any existing `r-neuralforecast` environment is first removed. The function then proceeds to create a new environment and install the specified packages.
#' @examples
#' \dontrun{
#' install_neuralforecast(fresh_install = TRUE)
#' }
#' @export
# #' @importFrom reticulate py_install conda_remove py_discover_config
## ' @importFrom cli cli_alert_info cli_alert_success cli_alert_danger


install_neuralforecast <- function(fresh_install = FALSE, specific_version = "1.7.5", env = "main") {


  envname <- if (env == "main") "r-neuralforecast" else "r-neuralforecast-dev"


  # Check for Conda installation
  if (!sumo_check_conda()) {
    return(invisible(FALSE))
  }

  # Check Python availability
  if (!reticulate::py_available(initialize = TRUE)) {
    cli::cli_alert_danger("Python is not available in your R environment. Please install Python or Anaconda and try again.")
    return(invisible(FALSE))
  }

  # Handle fresh install
  if (fresh_install) {
    cli::cli_alert_info("Removing conda env `r-neuralforecast` for a fresh install...")
    reticulate::conda_remove(envname = envname)
  }

  # Handle package version specification
  neuralforecast_pkg <- "neuralforecast"
  if (!is.null(specific_version)) {
    neuralforecast_pkg <- paste0(neuralforecast_pkg, "==", specific_version)
  }

  # Install NeuralForecast
  cli::cli_alert_info("Installing neuralforecast and dependencies...")
  reticulate::py_install(packages = neuralforecast_pkg,
                         envname  = envname,
                         method   = "conda",
                         conda    = "auto",
                         python_version = "3.11",
                         pip = TRUE
  )


  # Check for the existence of the conda environment
  envs <- reticulate::conda_list()
  if (envname %in% envs$name) {
    cli::cli_alert_success(paste0("The `", envname, "` conda environment has been successfully created."))
    cli::cli_alert_info(paste0("Please restart your R session and use reticulate::use_condaenv('", envname, "', required = TRUE) to activate the environment."))
  } else {
    cli::cli_alert_danger(paste0("Failed to create the `", envname, "` conda environment. Please check your configuration and try again."))
  }
}



# Check conda
sumo_check_conda <- function() {

  conda_list_nrow <- nrow(reticulate::conda_list())

  if (is.null(conda_list_nrow) || conda_list_nrow == 0L) {
    # No conda
    message("Could not detect Conda or Miniconda Package Managers, one of which is required for 'install_neuralforecast()'. \nAvailable options:\n",
            " - [Preferred] You can install Miniconda (light-weight) using 'reticulate::install_miniconda()'. \n",
            " - Or, you can install the full Aniconda distribution (1000+ packages) using 'reticulate::conda_install()'. \n\n",
            "Then use 'install_gluonts()' to set up the GluonTS python environment.")
    conda_found <- FALSE
  } else {
    conda_found <- TRUE
  }

  return(conda_found)
}


