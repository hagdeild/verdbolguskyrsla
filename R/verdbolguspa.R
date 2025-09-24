# 1.0.0 SETUP ----
library(tidyverse)
library(modeltime)
library(tidymodels)


date_from <- floor_date(today() - years(5), "month")

# 1.1.0 Data ----
data_raw_tbl <- read_csv2(
  "https://px.hagstofa.is:443/pxis/sq/cc5a0596-bf1a-4a88-872a-485896d53ed6"
) %>%
  set_names("date", "vnv", "vnvh") %>%
  mutate(
    date = make_date(str_sub(date, 1, 4), str_sub(date, 6, 7)),
    vnv = vnv / 10,
    vnvh = vnvh / 10
  ) %>%
  drop_na() %>%
  mutate(id = "id") %>%
  filter(date >= "2003-01-01") %>%
  arrange(date)


level_roc1_max <- data_raw_tbl %>%
  filter(date == max(date)) %>%
  pull(vnv)

data_path <- here("data/verdbolguskyrsla_data.xlsx")
spar_annarra_tbl <- readxl::read_excel(data_path, "spar") |>
  mutate(date = date(date))

spar_bankar_tbl <- spar_annarra_tbl |>
  select(-Seðlabankinn) |>
  pivot_longer(cols = -c(date)) |>
  drop_na()

spar_si_tbl <- spar_annarra_tbl |>
  select(date, Seðlabankinn)


# 2.0.0 Models ------------------------------------------------------------

# 2.1.0 Short term forecast -----------------------------------------------
short_data_tbl <- data_raw_tbl %>%
  select(date, vnv) %>%
  mutate(vnv = log(vnv))

stl_log_mtbl <- seasonal_reg() %>%
  set_engine("stlm_arima") %>%
  fit(vnv ~ date, data = short_data_tbl) %>%
  modeltime_table()

fc_short_tbl <- stl_log_mtbl %>%
  modeltime_forecast(
    h = 3,
    actual_data = short_data_tbl
  )

short_forecast_12m_tbl <- fc_short_tbl %>%
  mutate(
    .value = exp(.value),
    .value = .value / lag(.value, 12) - 1
  ) %>%
  filter(.key != "actual") %>%
  select(.index, .value) %>%
  set_names("date", "value") %>%
  mutate(name = "Skammtíma")


# 2.2.0 Long term forecast ------------------------------------------------

long_term_data_tbl <- data_raw_tbl %>%
  mutate(vnv = vnv / lag(vnv, 12) - 1) %>%
  drop_na()

fc_long_tbl <- arima_reg() %>%
  set_engine("auto_arima") %>%
  fit(vnv ~ date, data = long_term_data_tbl) %>%
  modeltime_table() %>%
  modeltime_forecast(
    h = 12,
    actual_data = long_term_data_tbl,
    keep_data = TRUE
  )

fc_long_tbl <- fc_long_tbl %>%
  select(date, .key, .value) %>%
  left_join(data_raw_tbl %>% select(date, vnv)) %>%
  mutate(
    fc_vnv = lag(vnv, 12) * (1 + .value),
    fc_1m = fc_vnv / lag(fc_vnv) - 1
  )


# 3.0.0 Valuebox ----------------------------------------------------------

# 3.1.0 12 mánaða spá -----------------------------------------------------
# spa_12m <- fc_long_tbl %>%
#   filter(.key == "prediction") %>%
#   filter(date == min(date)) %>%
#   pull(.value)

# short_12m <- short_forecast_12m_tbl %>%
#   filter(date == min(date)) %>%
#   pull(value)

# spa_naesta_manadar <- if_else(
#   short_12m < spa_12m,
#   paste0(percent(short_12m, 0.1), " - ", percent(spa_12m, 0.1)),
#   paste0(percent(spa_12m, 0.1), " - ", percent(short_12m, 0.1))
# )

# 3.2.0 Spá næsta mánaðar -------------------------------------------------
# spa_1m <- fc_tbl %>%
#   filter(.key == "prediction") %>%
#   filter(date == min(date)) %>%
#   pull(fc_1m)

short_1m <- fc_short_tbl %>%
  mutate(.value = exp(.value)) %>%
  mutate(diff = .value / lag(.value) - 1) %>%
  filter(.key != "actual") %>%
  slice_head(n = 1) %>%
  pull(diff)

# spa_naesta_manadar_1m <- if_else(
#   short_1m < spa_1m,
#   paste0(percent(short_1m, 0.01), " - ", percent(spa_1m, 0.01)),
#   paste0(percent(spa_1m, 0.01), " - ", percent(short_1m, 0.01))
# )

# 3.5.0 Valuebox --------------.--------------------------------------------
fc_valuebox_tbl <- tibble(
  spa_12m = percent(short_forecast_12m_tbl$value[1], accuracy = 0.01),
  spa_1m = percent(short_1m, accuracy = 0.01)
)


# 4.0.0 SAVE --------------------------------------------------------------

# Bæti skammtímaspá við
fc_tbl <- fc_long_tbl %>%
  mutate(name = if_else(.key == "actual", "Söguleg", "Langtíma")) %>%
  select(date, .value, name) %>%
  rename("value" = ".value") %>%
  bind_rows(short_forecast_12m_tbl) %>%
  as_tibble() |>
  filter(date >= date_from)


# Bæti við spám annarra greiningaraðila
fc_tbl <- fc_tbl |>
  bind_rows(spar_bankar_tbl) |>
  left_join(spar_si_tbl)


list(
  "verdbolguspa" = fc_tbl,
  "verdbolguspa_valuebox" = fc_valuebox_tbl
) |>
  write_rds("data/spar.rds")
