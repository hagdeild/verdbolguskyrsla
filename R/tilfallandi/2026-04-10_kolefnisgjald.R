# Lækkun kolefnisgjalds eftir hækkun olíuverðs
# AHT! SAMA GREINING VIRKAR FYRIR 24% -> 11% VSK BREYTINGU

# 1.0.0 SETUP ----
library(tidyverse)


# 1.1.0 Data ----

fc_ls <- read_rds("data/spar.rds")

verdbolguspar_tbl <- fc_ls |>
  pluck("verdbolguspa") |>
  select(-c(fc_vnv, Seðlabankinn)) |>
  filter(name %in% c("Söguleg", "VR"))


# sviðsmyndir — IRF was estimated with data through Feb, month 0 = March
svidsmyndir_tbl <- read_csv("data/greiningar/oliuverd_scenarios_varx.csv") |>
  select(month, cum_cpi, shock_label, duration) |>
  rename("shock_pct" = "shock_label") |>
  mutate(cum_cpi = cum_cpi / 100) |>

  filter(shock_pct == "+30%")

# How many months of the IRF are already in the THIEF baseline?
# IRF month 0 = March 2026 (when the oil shock hit).
# n_absorbed adjusts dynamically based on how far THIEF data extends.
irf_start <- as.Date("2026-03-01")
thief_start <- verdbolguspar_tbl |> filter(name == "VR") |> pull(date) |> min()
n_absorbed <- interval(irf_start, thief_start) %/% months(1)

# Subtract the already-absorbed cumulative effect and drop absorbed months
# Also subtract kolefnisgjald effect (0.3 pp) from forecast months
absorbed_effect <- svidsmyndir_tbl |>
  filter(month == n_absorbed - 1) |>
  select(shock_pct, duration, absorbed_cpi = cum_cpi)

svidsmyndir_tbl <- svidsmyndir_tbl |>
  filter(month >= n_absorbed) |>
  left_join(absorbed_effect, by = c("shock_pct", "duration")) |>
  mutate(
    addon = cum_cpi - absorbed_cpi,
    month = month - n_absorbed
  ) |>
  select(-absorbed_cpi)


# 2.0.0 GREINING ----

# 2.1.0 Olíuverð ----
soguleg_tbl <- verdbolguspar_tbl |>
  filter(name == "Söguleg") |>
  cross_join(distinct(svidsmyndir_tbl, duration)) |>
  mutate(shock_pct = "Söguleg", svidsmyndir = value)

baseline_tbl <- verdbolguspar_tbl |>
  filter(name == "VR") |>
  cross_join(distinct(svidsmyndir_tbl, duration)) |>
  mutate(shock_pct = "Grunnspá", svidsmyndir = value)

skammtima_tbl <- verdbolguspar_tbl |>
  filter(name == "VR") |>
  mutate(month = row_number() - 1L) |>
  left_join(svidsmyndir_tbl, by = "month") |>
  mutate(
    svidsmyndir = value + addon,
  )


# 2.2.0 Kolefnisgjald ----
kolefnisgjald_bensin <- 24.25
kolefnisgjald_diesel <- 28.3

bensin_verd <- 246
diesel_verd <- 279.6

bensin_vog <- 249 / 10000
diesel_vog <- 108 / 10000
total_vog <- bensin_vog + diesel_vog

bensin_laekkun <- kolefnisgjald_bensin / bensin_verd
diesel_laekkun <- kolefnisgjald_diesel / diesel_verd

avg_laekkun <- mean(bensin_laekkun, diesel_laekkun)

bein_ahrif <- total_vog * avg_laekkun

# Olíuverðsáhrif + lækkun kolefnisgjalds
skammtima_kolefni_tbl <- skammtima_tbl |>
  mutate(
    svidsmyndir = svidsmyndir - bein_ahrif,
    shock_pct %in% c("+30% og kolefnisgjald")
  )

spar_udpate_tbl <- bind_rows(
  soguleg_tbl,
  baseline_tbl,
  skammtima_tbl,
  skammtima_kolefni_tbl
)


# 3.0.0 PLOT ----
p_svidsmyndir <- spar_udpate_tbl |>
  mutate(
    duration = case_when(
      duration == "1 month" ~ "Tímabundin hækkun í 1 mánuð",
      duration == "2 months" ~ "Tímabundin hækkun í 2 mánuði",
      duration == "3 months" ~ "Tímabundin hækkun í 3 mánuði",
      TRUE ~ "Hækkun varir næstu 6 mánuði"
    ),
    duration = factor(
      duration,
      levels = c(
        "Tímabundin hækkun í 1 mánuð",
        "Tímabundin hækkun í 2 mánuði",
        "Tímabundin hækkun í 3 mánuði",
        "Hækkun varir næstu 6 mánuði"
      )
    )
  ) |>
  #filter(date >= "2024-01-01") |>
  ggplot(aes(date, svidsmyndir, col = shock_pct, linetype = shock_pct)) +
  geom_line() +
  scale_linetype_manual(
    values = c(
      "Grunnspá" = "dashed",
      "Söguleg" = "solid",
      "+10%" = "solid",
      "+30%" = "solid",
      "+50%" = "solid",
      "+70%" = "solid",
      "+30% og kolefnisgjald" = "dashed"
    )
  ) +
  scale_color_manual(
    values = c(
      "Söguleg" = "black",
      "Grunnspá" = "black",
      setNames(blue_palette[2:5], c("+10%", "+30%", "+50%", "+70%")),
      "+30% og kolefnisgjald" = blue_palette[3]
    )
  ) +
  facet_wrap(~duration) +
  #theme_minimal() +
  theme(
    legend.title = element_blank(),
    text = element_text(size = text_size)
  ) +
  labs(x = NULL, y = NULL) +
  scale_y_continuous(labels = percent)


plotly::ggplotly(p_svidsmyndir)
