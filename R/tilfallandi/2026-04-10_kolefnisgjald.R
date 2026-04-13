# Lækkun VSK eftir hækkun olíuverðs
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
# Also subtract VSK effect from forecast months
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
    shock_pct = "+30%"
  )


# 2.2.0 VSK ----
# kolefnisgjald_bensin <- 24.25
# kolefnisgjald_diesel <- 28.3

# bensin_verd <- 246
# diesel_verd <- 279.6

bensin_vog <- 249 / 10000
diesel_vog <- 108 / 10000
total_vog <- bensin_vog + diesel_vog

# bensin_laekkun <- kolefnisgjald_bensin / bensin_verd
# diesel_laekkun <- kolefnisgjald_diesel / diesel_verd

# avg_laekkun <- mean(bensin_laekkun, diesel_laekkun)

avg_laekkun <- abs(1.11 / 1.24 - 1)

bein_ahrif <- total_vog * avg_laekkun

# Olíuverðsáhrif + lækkun VSK
skammtima_vsk_tbl <- skammtima_tbl |>
  mutate(
    svidsmyndir = svidsmyndir - bein_ahrif,
    shock_pct = "+30% + VSK lækkun"
  )

spar_udpate_tbl <- bind_rows(
  soguleg_tbl,
  baseline_tbl,
  skammtima_tbl,
  skammtima_vsk_tbl
)


# 3.0.0 PLOT ----

blue_palette <- c(
  "#0072b1",
  "#54b6e9",
  "#00a073",
  "#f0e142",
  "#e59e23",
  "#d65e18",
  "#D64550",
  "#6a3d9a", # deep purple
  "#333333" # dark grey (almost black)
)

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
      "+30%" = "solid",
      "+30% + VSK lækkun" = "dashed"
    )
  ) +
  scale_color_manual(
    values = c(
      "Söguleg" = "black",
      "Grunnspá" = "black",
      "+30%" = blue_palette[2],
      "+30% + VSK lækkun" = blue_palette[3]
    )
  ) +
  facet_wrap(~duration) +
  #theme_minimal() +
  theme(
    legend.title = element_blank(),
    text = element_text(size = 16)
  ) +
  labs(x = NULL, y = NULL) +
  scale_y_continuous(labels = scales::percent)

p_svidsmyndir

# plotly::ggplotly(p_svidsmyndir)

# 3.1.0 Single plot ----

title_text <- c(
  "Grunnspáin er univariate líkan á vísitölu neysluverðs. 30% hækkun brent olíu er metin með IRF út frá VARX líkani.
Fyrstu gráðu áhrifin eru að einhverju leyti komin fram í mars tölunum og sýnir því grafið hér annarrar gráðu áhrifin.
Síðan er bætt við 0,3% lækkun vegna útspils ríkisstjórnarinnar."
)

spar_udpate_tbl |>
  filter(
    duration == "2 months",
    shock_pct %in% c("Söguleg", "Grunnspá", "+30%", "+30% + VSK lækkun")
  ) |>
  ggplot(aes(date, svidsmyndir, col = shock_pct, linetype = shock_pct)) +
  geom_line() +
  annotate(
    "point",
    x = as.Date("2026-08-01"),
    y = 0.047,
    shape = 15,
    size = 3,
    color = "#dc1e35"
  ) +
  annotate(
    "text",
    x = as.Date("2026-08-01"),
    y = 0.041,
    label = "Kjarasamningar 4,7%",
    vjust = -1,
    size = 4,
    color = "#dc1e35"
  ) +
  scale_linetype_manual(
    values = c(
      "Grunnspá" = "dashed",
      "Söguleg" = "solid",
      "+30%" = "solid",
      "+30% + VSK lækkun" = "dashed"
    )
  ) +
  scale_color_manual(
    values = c(
      "Söguleg" = "black",
      "Grunnspá" = "black",
      "+30%" = blue_palette[2],
      "+30% + VSK lækkun" = blue_palette[3]
    )
  ) +
  labs(
    x = NULL,
    y = NULL,
    title = "Verðbólguspá með sviðsmyndagreiningu",
    subtitle = str_wrap(title_text, width = 120)
  ) +
  theme_minimal() +
  theme(
    legend.title = element_blank(),
    text = element_text(size = 16),
    plot.subtitle = element_text(color = "grey30", size = 14),
    legend.position = "bottom",
    legend.direction = "vertical"
  ) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 0.11))
