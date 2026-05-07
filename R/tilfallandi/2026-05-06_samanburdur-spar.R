# Samanburður á spám VR og annarra greiningaraðila - Hvernig hefur gengið?

# 1.0.0 SETUP ----
library(tidyverse)
library(modeltime)
library(tidymodels)

# 1.1.0 Data ----

# 1.1.1 vnv ----
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


# 1.1.2 spar greiningaradila ----
spar_annarra_tbl <- readxl::read_excel(
  "data/greiningar/greiningaradilar_sogulegar_spar.xlsx"
) |>
  janitor::clean_names() |>
  mutate(
    date = date(date),
    creation_date = date(creation_date)
  )

# 1.1.3 raun verdbolga ----
actuals_tbl <- data_raw_tbl |>
  mutate(infl = vnv / lag(vnv, 12) - 1) |>
  select(date, infl_actual = infl) |>
  drop_na()

# 2.0.0 VR model ----
medium_data_tbl <- data_raw_tbl %>%
  mutate(infl = vnv / lag(vnv, 12) - 1) %>%
  drop_na()

cutoff_dates <- c(
  "2026-01-01",
  "2026-02-01",
  "2026-03-01",
  "2026-04-01",
  "2026-05-01"
) |> as.Date()

fc_vr_ls <- list()

for (i in seq_along(cutoff_dates)) {
  cutoff <- cutoff_dates[i]

  temp_data_tbl <- medium_data_tbl |>
    filter(date < cutoff)

  medium_mtbl <- temporal_hierarchy(
    use_model = "arima",
    combination_method = "struc"
  ) %>%
    set_engine("thief") %>%
    fit(infl ~ date, data = temp_data_tbl) %>%
    modeltime_table()

  fc_medium_tbl <- medium_mtbl %>%
    modeltime_forecast(
      h = 6,
      actual_data = temp_data_tbl,
      keep_data = TRUE
    )

  fc_vr_ls[[i]] <- fc_medium_tbl |>
    filter(.key != "actual") |>
    slice_head(n = 3) |>
    transmute(
      date = floor_date(.index, "month"),
      horizon = row_number(),
      spa_vr = .value
    )
}

# VR forecast keyed by (target date, horizon). One number per (date, horizon).
fc_vr_tbl <- bind_rows(fc_vr_ls)

# 3.0.0 Samanburdur ----

# 3.1.0 Pairs - analyst row joined with VR forecast at same (date, horizon) ----
# Horizon = row_number() within (analyst, fyrri_seinni, creation_date).
pairs_tbl <- spar_annarra_tbl |>
  group_by(greiningaradili, fyrri_seinni, creation_date) |>
  arrange(date, .by_group = TRUE) |>
  mutate(horizon = row_number()) |>
  ungroup() |>
  filter(horizon <= 3) |>
  rename(spa_analyst = spa) |>
  inner_join(fc_vr_tbl, by = c("date", "horizon")) |>
  left_join(actuals_tbl, by = "date") |>
  mutate(
    err_analyst = spa_analyst - infl_actual,
    err_vr = spa_vr - infl_actual
  )

# 3.2.0 RMSE - allur horizon (1:3) ----
rmse_full_tbl <- pairs_tbl |>
  filter(!is.na(infl_actual)) |>
  group_by(greiningaradili, fyrri_seinni) |>
  summarise(
    rmse_analyst = sqrt(mean(err_analyst^2)),
    rmse_vr = sqrt(mean(err_vr^2)),
    n = n(),
    .groups = "drop"
  ) |>
  arrange(greiningaradili, fyrri_seinni)

rmse_full_tbl

# 3.3.0 RMSE per horizon ----
rmse_per_horizon_tbl <- pairs_tbl |>
  filter(!is.na(infl_actual)) |>
  group_by(greiningaradili, fyrri_seinni, horizon) |>
  summarise(
    rmse_analyst = sqrt(mean(err_analyst^2)),
    rmse_vr = sqrt(mean(err_vr^2)),
    n = n(),
    .groups = "drop"
  ) |>
  arrange(greiningaradili, fyrri_seinni, horizon)

rmse_per_horizon_tbl
