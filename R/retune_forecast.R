# Finn besta líkanið fyrir verðbólguspá. Bæði langtíma og skammtíma

# SETUP ----
library(tidyverse)

source("R/get_univar_function.R")
#source("R/neuralforecast.R")

# Data ----

vnv_tbl <- read_csv2(
  "https://px.hagstofa.is:443/pxis/sq/afdd1b6c-63e6-46fa-8dbd-257ffa5d0cc9"
) %>%
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


laun_tbl <- read_csv2(
  "https://px.hagstofa.is:443/pxis/sq/c8daf660-8c92-4c37-8fe9-4a39e54691c2"
) |>
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
  horizon = 3,
  slice_limit = 4,
  ensemble = TRUE
)

fc_best_ls$Accuracy

# (fc_best_ls$tscv |>
#   filter(resample == "resample_5", .index >= "2020-01-01") |>
#   ggplot(aes(.index, growth, col = name)) +
#   geom_line()) |>
#   plotly::ggplotly()

fc_best_ls$future_fc |>
  ggplot(aes(.index, growth, col = name)) +
  geom_line()
