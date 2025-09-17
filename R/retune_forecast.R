# Finn besta líkanið fyrir verðbólguspá. Bæði langtíma og skammtíma


# SETUP ----
library(tidyverse)

source("R/get_univar_function.R")
source("R/neuralforecast.R")


# Data ----

vnv_tbl <- read_csv2("https://px.hagstofa.is:443/pxis/sq/afdd1b6c-63e6-46fa-8dbd-257ffa5d0cc9") %>% 
  janitor::clean_names()

vnv_tbl <- vnv_tbl |> 
  filter(visitala == "Vísitala neysluverðs") |> 
  select(manudur, visitala_neysluverds) |> 
  set_names("date", "outcome") |> 
  mutate(
    id = "id",
    date = make_date(str_sub(date, 1, 4), str_sub(date, 6, 7)),
    outcome = outcome / 10
  )

full_vnv_tbl <- vnv_tbl |> 
  timetk::future_frame(.date_var = date, .length_out = 12, .bind_data = TRUE) |> 
  fill(id, .direction = "down")


gengi_tbl <- read_csv2("data/timeseries_byday_20000104_20250915.csv") |> 
  select(1, 2) |> 
  set_names("date", "visitala") |> 
  mutate(
    date = dmy(date),
    date = floor_date(date, "month")
  ) |> 
  group_by(date) |> 
  summarise(visitala = mean(visitala, na.rm = TRUE))


laun_tbl <- read_csv2("https://px.hagstofa.is:443/pxis/sq/c8daf660-8c92-4c37-8fe9-4a39e54691c2") |> 
  set_names("date", "laun") |> 
  mutate(
    date = make_date(str_sub(date, 1, 4), str_sub(date, 6, 7)),
    laun = as.numeric(laun)
  ) |> 
  drop_na()

full_vnv_tbl <- full_vnv_tbl |> 
  left_join(gengi_tbl) |> 
   left_join(laun_tbl) |> 
  mutate(
    outcome = log(outcome),
    visitala = log(visitala),
    visitala = visitala - lag(visitala)
  ) |> 
   drop_na()



# Best univariate ----
fc_best_ls <- get_univariate_forecasts(
  data = vnv_tbl,
  horizon = 12
  #ensembles = "auto_best2"
)

fc_best_ls$Accuracy |> 
  group_by(name) |> 
  summarise(
    mean = mean(rmse),
    sd = sd(rmse)
  ) |> 
  arrange(mean)

(
  fc_best_ls$Forecasts |> 
  filter(resample == "resample_1", .index >= "2020-01-01") |> 
  ggplot(aes(.index, growth, col = name)) + 
  geom_line()
) |> 
  plotly::ggplotly()



# Prófa stl-log arimax ----

train_vnv_tbl <-  vnv_tbl |> 
  head(-12)

test_vnv_tbl <- vnv_tbl |> 
  tail(12)

stl_log_arimax_mtbl <- seasonal_reg() %>% 
    set_engine("stlm_arima") %>% 
    fit(outcome ~ date + visitala, data = train_vnv_tbl) %>% 
    modeltime_table()

stl_log_arimax_mtbl |> 
  modeltime_forecast(
    new_data = test_vnv_tbl,
    actual_data = bind_rows(train_vnv_tbl,  test_vnv_tbl)
  ) |> 
  plot_modeltime_forecast()




# Neuralforecast ----
# nf <- import_neuralforecast()

 
# date_to_int_tbl <- full_vnv_tbl %>%
#       dplyr::select(date) %>%
#       dplyr::distinct() %>%
#       dplyr::arrange(date) %>%
#       dplyr::mutate(ds = row_number())

# train_vnv_tbl <-  full_vnv_tbl |> 
#   select(date, outcome, id) |> 
#   filter(!is.na(outcome)) |> 
#   head(-12) |> 
#   left_join(date_to_int_tbl) |> 
#   select(-date) |> 
#   rename(
#     "unique_id" = "id",
#     "y" = "outcome"
#   )

# test_vnv_tbl <- full_vnv_tbl |> 
#   select(date, outcome, id) |> 
#   filter(!is.na(outcome)) |> 
#   tail(12) |> 
#   left_join(date_to_int_tbl) |> 
#   select(-date) |> 
#   rename(
#     "unique_id" = "id",
#     "y" = "outcome"
#   )


# hist_exog_reg <- full_vnv_tbl |> 
#   left_join(date_to_int_tbl) |> 
#     select(-c(outcome, date)) |> 
#   rename("unique_id" = "id")

# hist_exog_ls <- names(hist_exog_reg)
# hist_exog_ls <- hist_exog_ls[!hist_exog_ls %in% c("unique_id", "ds")]
# hist_exog_ls <- as.list(hist_exog_ls)

# train_vnv_tbl <- train_vnv_tbl |> 
#   left_join(hist_exog_reg)


# horizon <- as.integer(10)
# input_size <- as.integer(24)
# nixtla_dl_models <- c("nhits", "nbeatsx", "kan")


# models <- create_neuralforecast_models(
#     nf            = nf,
#     models_to_run = nixtla_dl_models,
#     input_size    = input_size,
#     horizon       = horizon,
#     max_steps     = 200,
#     hist_exog_ls  = hist_exog_ls,
#     stat_exog_ls  = NULL
#   )


# nf_set <- neural_model_setup(nf = nf, models = models, frequency = as.integer(1))

# nf_fit <- nf_set$fit(df = train_vnv_tbl)

# nf_preds <- nf_set$predict(nf_fit)

# nf_preds_tbl <- nf_preds |> as_tibble()


# nf_preds_tbl <- nf_preds_tbl %>%
#     tidyr::pivot_longer(cols = -c(ds)) %>%
#     dplyr::rename(".model_desc" = "name") %>%
#     dplyr::rename(".value" = "value") %>%
#     dplyr::mutate(
#       .key        = "prediction",
#       .model_id   = 1
#     ) %>%
#     dplyr::rename(".index" = "ds") |> 
#   mutate(id = "id")


#  full_data <- train_vnv_tbl %>%
#       bind_rows(test_vnv_tbl) %>%
#       dplyr::mutate(
#         .key = "actual",
#         .model_desc = "ACTUAL",
#         .model_id = NA_real_
#       ) %>%
#       dplyr::rename(".value" = "y") %>%
#       dplyr::rename("id" = "unique_id") %>%
#       dplyr::rename(".index" = "ds") %>%
#       dplyr::select(.model_id, .model_desc, .key, id, .index, .value)

# fc_tbl <- nf_preds_tbl %>%
#     dplyr::bind_rows(full_data) %>%
#     dplyr::arrange(id, .index)

# fc_tbl |> 
#   modeltime::plot_modeltime_forecast()


# fc_tbl |> 
#   select(-c(.model_id, .key)) |> 
#   mutate(.value = exp(.value)) |> 
#   pivot_wider(names_from = .model_desc, values_from = .value) |> 
#   select(-KAN) |> 
#   mutate(
#     NHITS = if_else(is.na(NHITS), ACTUAL, NHITS),
#     NBEATSx = if_else(is.na(NBEATSx), ACTUAL, NBEATSx)
#   ) |> 
#   mutate(
#     NHITS = NHITS / lag(NHITS, 12) - 1,
#     NBEATSx = NBEATSx / lag(NBEATSx, 12) - 1,
#     ACTUAL = ACTUAL / lag(ACTUAL, 12) - 1
#   ) |> 
#   pivot_longer(cols = c(ACTUAL, NHITS, NBEATSx)) |> 
#   mutate(
#     .key = if_else(.index > 295, "prediction", "actual"),
#     .model_id = 1
#   ) |> 
#   rename(
#     ".model_desc" = "name",
#     ".value" = "value"
#   ) |> 
#   modeltime::plot_modeltime_forecast()
