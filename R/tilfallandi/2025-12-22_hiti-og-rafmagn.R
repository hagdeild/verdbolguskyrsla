# Greining á hita og rafmagni

# 1.0.0 SETUP ----
library(tidyverse)
library(vr)


# 1.1.0 Data ----

manudir_tbl <- tibble(
  manudur = c(
    "Janúar",
    "Febrúar",
    "Mars",
    "Apríl",
    "Maí",
    "Júní",
    "Júlí",
    "Ágúst",
    "September",
    "Október",
    "Nóvember",
    "Desember"
  ),
  man_no = 1:12
)

# 1997-2002
data_1_tbl <- read_csv2(
  "https://px.hagstofa.is:443/pxis/sq/dea6f676-1493-45d7-9706-099aa9053bdc",
  locale = locale(encoding = "latin1"),
  na = c(".", "..")
) |>
  janitor::clean_names() |>
  drop_na()

data_1_tbl <- data_1_tbl |>
  left_join(manudir_tbl) |>
  mutate(
    date = make_date(ar, man_no)
  ) |>
  select(date, undirvisitala, visitala_neysluverds) |>
  rename("value" = "visitala_neysluverds")


# 2002-2019
data_2_tbl <- read_csv2(
  "https://px.hagstofa.is:443/pxis/sq/b837e7b7-a824-43d8-8fae-e46a66e913e8",
  locale = locale(encoding = "latin1"),
  na = c(".", "..")
) |>
  janitor::clean_names() |>
  drop_na()

data_2_tbl <- data_2_tbl |>
  left_join(manudir_tbl) |>
  mutate(
    date = make_date(ar, man_no)
  ) |>
  select(date, undirvisitala, visitala_neysluverds) |>
  rename("value" = "visitala_neysluverds")


# 2019-present
data_3_tbl <- read_csv2(
  "https://px.hagstofa.is:443/pxis/sq/e21ea443-3a17-41c6-9281-be5d6ee69a23"
) |>
  janitor::clean_names() |>
  mutate(date = make_date(str_sub(manudur, 1, 4), str_sub(manudur, 6, 7))) |>
  select(-manudur)


# Skeyti saman data 2 og data 3 ----

# Step 1: Standardize column names
data_2_clean <- bind_rows(
  data_1_tbl |> filter(year(date) < 2002),
  data_2_tbl
) |>
  rename(visitala_neysluverds = value)

# Step 2: Calculate linking factors for each undirvisitala
# Using the average ratio in 2019 as the linking factor
linking_factors <- data_2_clean |>
  filter(year(date) == 2019) |>
  inner_join(
    data_3_tbl |> filter(year(date) == 2019),
    by = c("undirvisitala", "date"),
    suffix = c("_old", "_new")
  ) |>
  group_by(undirvisitala) |>
  summarise(
    linking_factor = mean(visitala_neysluverds_new / visitala_neysluverds_old),
    .groups = "drop"
  )

# Step 3: Rebase the old data (pre-2019) using the linking factors
data_2_rebased <- data_2_clean |>
  filter(date < min(data_3_tbl$date)) |>
  left_join(linking_factors, by = "undirvisitala") |>
  mutate(visitala_neysluverds = visitala_neysluverds * linking_factor) |>
  select(-linking_factor)

# Step 4: Combine the rebased old data with the new data
combined_data <- bind_rows(
  data_2_rebased,
  data_3_tbl
) |>
  arrange(undirvisitala, date)


# 1.2.0 Vísitala neysluverðs ----
vnv_tbl <- read_csv2(
  "https://px.hagstofa.is:443/pxis/sq/adc71e68-042c-4d0e-a63e-ece7b4dfe75f"
) |>
  set_names("date", "vnv") |>
  mutate(date = make_date(str_sub(date, 1, 4), str_sub(date, 6, 7)))


# 2.0.0 GREINING ----
data_use_tbl <- combined_data |>
  left_join(vnv_tbl) |>
  mutate(value_fix = visitala_neysluverds / vnv) |>
  arrange(date) |>
  group_by(undirvisitala) |>
  mutate(index = value_fix / value_fix[1] * 100) |>
  ungroup()

data_use_tbl <- data_use_tbl |>
  mutate(undirvisitala = sub("^\\d+\\s*", "", undirvisitala))


icelandic_months <- c(
  "janúar",
  "febrúar",
  "mars",
  "apríl",
  "maí",
  "júní",
  "júlí",
  "ágúst",
  "september",
  "október",
  "nóvember",
  "desember"
)

date_from <- "2000-01-01"

# Create the title
plot_title <- glue::glue(
  "Raunverðshækkun á hita og rafmagni frá {icelandic_months[month(date_from)]} {year(date_from)}"
)


p <- data_use_tbl |>
  filter(date >= date_from) |>
  group_by(undirvisitala) |>
  mutate(index = index / index[1] * 100) |>
  ungroup() |>
  ggplot(aes(date, index, col = undirvisitala)) +
  geom_line() +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    text = element_text(size = 32)
  ) +
  labs(
    x = NULL,
    y = NULL,
    title = plot_title
  ) +
  scale_color_manual(values = my_cols)

ggsave(
  plot = p,
  paste0(
    "R/tilfallandi/2025-2025-12-22_hiti-og-rafmagn",
    paste0("_fra-", date_from),
    ".png"
  ),
  width = 10,
  height = 7,
  dpi = 200
)
