# Finn besta líkanið fyrir verðbólguspá. Bæði langtíma og skammtíma

# SETUP ----
library(tidyverse)
library(vrmodel)

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
  ) |>
  filter(date >= "2003-01-01")


# Best univariate ----
fc_best_ls <- get_univariate_forecasts(
  data = vnv_tbl,
  horizon = 6,
  slice_limit = 6,
  ensemble = TRUE
)

fc_best_ls$Accuracy

# (fc_best_ls$tscv |>
#   filter(resample == "resample_5", .index >= "2020-01-01") |>
#   ggplot(aes(.index, growth, col = name)) +
#   geom_line()) |>
#   plotly::ggplotly()

fc_best_ls$best_fc |>
  ggplot(aes(.index, roc_2, col = name)) +
  geom_line()
