# Sviðsmyndagreining út frá olíuverðhækkunum

# 1.0.0 SETUP ----
library(tidyverse)


# 1.1.0 Data ----

# spár
spar_tbl <- read_rds("data/spar.rds") |>
  pluck("verdbolguspa")


spar_tbl <- spar_tbl |>
  select(-Seðlabankinn) |>
  filter(name %in% c("Söguleg", "Skammtíma"))


# sviðsmyndir
svidsmyndir_tbl <- read_csv("../hagrannsoknir/output/oliuverd/scenarios.csv") |>
  select(month, cumulative_pct, shock_pct, duration)

svidsmyndir_tbl <- svidsmyndir_tbl |>
  mutate(cumulative_pct_converted = (exp(cumulative_pct / 100) - 1))

# 2.0.0 GREINING ----

soguleg_tbl <- spar_tbl |>
  filter(name == "Söguleg") |>
  cross_join(distinct(svidsmyndir_tbl, shock_pct, duration)) |>
  mutate(svidsmyndir = value)

baseline_tbl <- spar_tbl |>
  filter(name == "Skammtíma") |>
  cross_join(distinct(svidsmyndir_tbl, duration)) |>
  mutate(shock_pct = "Grunnspá", svidsmyndir = value)

skammtima_tbl <- spar_tbl |>
  filter(name == "Skammtíma") |>
  mutate(month = row_number() - 1L) |>
  left_join(svidsmyndir_tbl, by = "month") |>
  mutate(svidsmyndir = value + cumulative_pct_converted)

spar_udpate_tbl <- bind_rows(soguleg_tbl, baseline_tbl, skammtima_tbl)


(spar_udpate_tbl |>
  filter(date >= "2024-01-01") |>
  ggplot(aes(date, svidsmyndir, col = shock_pct)) +
  geom_line() +
  facet_wrap(~duration)) |>
  plotly::ggplotly()


spar_udpate_tbl |>
  filter(duration == "2 month") |>
  view()
