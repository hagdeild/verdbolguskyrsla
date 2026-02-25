# 1.0.0 SETUP ----

library(tidyverse)
library(eurostat)
library(XML)
library(httr)
library(openxlsx)
library(zoo)
library(rvest)
library(readxl)
library(here)
library(fredr)

lond_tbl <- read_csv("data/lond.csv") |>
  select(iso_3, land)

# 1.1.0 Functions ----
fix_date <- function(data) {
  data %>%
    mutate(date = make_date(str_sub(manudur, 1, 4), str_sub(manudur, 6, 7)))
}

# 1.2.0 Other ----

date_from <- floor_date(today() - years(5), "month")
data_path <- here("data/verdbolguskyrsla_data.xlsx")
exp_path <- here("data/Verdbolguvaentingar-a-mismunandi-maelikvarda.xlsx")

new_coicop_date_from <- "2026-01-01"

# 2.0.0 Verðbólga ----

# 2.1.0 Verðbólga með og án húsnæðis ----
print("Verðbólga með og án húsænðis")

vnv_tbl <- read_csv2(
  "https://px.hagstofa.is:443/pxis/sq/6dd6d9a9-ff2f-4379-a78f-7fc800e09ea9"
) %>%
  janitor::clean_names()

vnv_tbl <- vnv_tbl %>%
  fix_date() %>%
  select(date, visitala, visitala_neysluverds) %>%
  set_names("date", "flokkur", "visitala") %>%
  mutate(
    flokkur = ifelse(
      flokkur == "Vísitala neysluverðs",
      "Verðbólga",
      "Verðbólga án húsnæðis"
    )
  ) %>%
  group_by(flokkur) %>%
  mutate(verdbolga = visitala / lag(visitala, 12) - 1) %>%
  drop_na() %>%
  ungroup()


vnv_tbl <- vnv_tbl %>%
  arrange(date) %>%
  group_by(flokkur) %>%
  mutate(milli_manada = visitala / lag(visitala) - 1) %>%
  ungroup() %>%
  drop_na() |>
  filter(date >= date_from)

valuebox_verdbolga <- vnv_tbl %>%
  filter(flokkur == "Verðbólga") %>%
  arrange(date) %>%
  mutate(
    visital_ma = TTR::SMA(visitala, 12),
    hradi = ((visital_ma / lag(visital_ma, 6))^(1 / 6))^12 - 1,
    hradi_diff = hradi - lag(hradi)
  ) %>%
  filter(date == max(date)) %>%
  select(verdbolga, milli_manada, hradi, hradi_diff)

verdbolga_latest <- vnv_tbl |>
  filter(date == max(date), flokkur == "Verðbólga") |>
  pull(verdbolga)

# 2.2.0 Innlend og erlend verðbólga ----

# innlend_innflutt_old_tbl <- read_csv2(
#   "https://px.hagstofa.is:443/pxis/sq/efeec1b8-11a5-402c-b747-99fe8ab61bec",
#   na = "."
# ) %>%
#   set_names("date", "flokkur", "visitala", "weight") |>
#   drop_na()

# innlend_innflutt_tbl <- innlend_innflutt_tbl %>%
#   mutate(
#     # Laga gildi
#     visitala = visitala / 10,
#     weight = weight / 1000,

#     # Bý til dagsetningu
#     date = make_date(year = str_sub(date, 1, 4), month = str_sub(date, 6, 7)),

#     # Bý til nýja flokka
#     flokkur_2 = case_when(
#       str_detect(flokkur, "5|6|7|8|9") ~ "Innflutt verðlag",
#       str_detect(flokkur, "Húsnæði") ~ "Húsnæði",
#       str_detect(flokkur, "1|2|3|4|11|12") ~ "Innlent verðlag utan húsnæðis"
#     )
#   )

# innlend_innflutt_tbl <- innlend_innflutt_tbl %>%
#   drop_na() |>
#   group_by(date, flokkur_2) %>%
#   mutate(
#     new_weight = weight / sum(weight),
#     new_index = new_weight * visitala
#   ) %>%
#   summarise(index = sum(new_index)) %>%
#   group_by(flokkur_2) %>%
#   mutate(delta = index / lag(index, 12) - 1) %>%
#   drop_na(delta) %>%
#   ungroup()

# Frá nóvember 2022
# innlend_innflutt_nov_tbl <- innlend_innflutt_tbl %>%
#   filter(date >= date_from) %>%
#   group_by(flokkur_2) %>%
#   mutate(voxtur = index / index[1] - 1)

# 2.3.0 Verðbólga eftir eðli og uppruna ----

# 2.3.1 eldri ----
print("Verðbólga eftir eðli og uppruna - eldri")

edli_og_uppruna_old_raw_tbl <- read_csv2(
  "https://px.hagstofa.is:443/pxis/sq/efeec1b8-11a5-402c-b747-99fe8ab61bec",
  na = "."
) %>%
  drop_na() |>
  set_names("date", "flokkur", "value", "weight") %>%
  mutate(
    value = value / 10,
    weight = weight / 1000
  ) %>%
  mutate(
    date = make_date(str_sub(date, 1, 4), str_sub(date, 6, 7))
  ) %>%
  drop_na(value)

edli_og_uppruna_old_tbl <- edli_og_uppruna_old_raw_tbl %>%
  mutate(
    flokkur = case_when(
      str_detect(flokkur, "Innlendar vörur og grænmeti") ~ "Innlendar vörur",
      str_detect(flokkur, "Innfluttar vörur alls") ~ "Innfluttar vörur",
      str_detect(flokkur, "Opinber þjónusta") ~ "Opinber þjónusta",
      str_detect(flokkur, "Önnur þjónusta") ~ "Önnur þjónusta",
      str_detect(flokkur, "10  Húsnæði") ~ "Húsnæði",
      TRUE ~ "annad"
    )
  ) %>%
  arrange(date, flokkur) %>%
  group_by(flokkur) %>%
  mutate(
    delta = value / lag(value, 12) - 1,
    weight = zoo::rollapply(
      weight,
      width = 12,
      FUN = mean,
      fill = NA,
      partial = FALSE,
      align = "right"
    )
  ) %>%
  ungroup() %>%
  drop_na(delta) %>%
  mutate(
    hlutdeild = delta * weight
  ) |>
  filter(!flokkur == "annad")

# 2.3.2 ný útgáfa ----
print("Verðbólga eftir eðli og uppruna - ný útgáfa")

edli_og_uppruna_new_raw_tbl <- read_csv2(
  "https://px.hagstofa.is:443/pxis/sq/8f78a07e-2d8c-4a77-950a-d89527370778",
  na = "."
) |>
  drop_na() |>
  set_names("date", "flokkur", "value", "weight") %>%
  mutate(
    value = value / 10,
    weight = weight / 1000
  ) %>%
  mutate(
    date = make_date(str_sub(date, 1, 4), str_sub(date, 6, 7))
  ) %>%
  drop_na(value)

edli_og_uppruna_new_tbl <- edli_og_uppruna_new_raw_tbl |>
  mutate(
    flokkur = case_when(
      str_detect(flokkur, "2a Innlendar vörur") ~ "Innlendar vörur",
      str_detect(flokkur, "2b Innfluttar vörur") ~ "Innfluttar vörur",
      str_detect(
        flokkur,
        "2c Vörur í blönduðum flokkum"
      ) ~ "Innlendar og innfluttar",
      str_detect(flokkur, "5c Opinber þjónusta") ~ "Opinber þjónusta",
      str_detect(
        flokkur,
        "5a Ferðaþjónusta|5b Húsnæði|5d Önnur þjónusta"
      ) ~ "Önnur þjónusta",
      str_detect(flokkur, "3 Búvörur") ~ "Búvörur",
      str_detect(
        flokkur,
        "4 Vörur án orku og matvöru"
      ) ~ "Vörur án orku og matvöru",
      TRUE ~ "annað"
    )
  ) |>
  arrange(date, flokkur) %>%
  group_by(flokkur) %>%
  mutate(
    delta = value / lag(value, 12) - 1,
    weight = zoo::rollapply(
      weight,
      width = 12,
      FUN = mean,
      fill = NA,
      partial = TRUE,
      align = "right"
    )
  ) %>%
  ungroup() %>%
  drop_na(delta) %>%
  mutate(
    hlutdeild = delta * weight
  ) |>
  filter(!flokkur == "annað")

# 2.3.3 sameina gögn ----
print("Verðbólga eftir eðli og uppruna - sameina")

edli_og_uppruna_tbl <-
  bind_rows(
    edli_og_uppruna_old_tbl,
    edli_og_uppruna_new_tbl |> filter(date >= new_coicop_date_from)
  )

# Bæti við verðbólgunni
edli_og_uppruna_tbl <- edli_og_uppruna_tbl %>%
  left_join(
    vnv_tbl %>% filter(flokkur == "Verðbólga") %>% select(-c(visitala, flokkur))
  ) %>%
  drop_na() |>
  filter(date >= date_from)


# 2.4.0 Undirliggjandi verðbólga ----

# 2.4.1 eldri ----
print("Undirliggjandi verðbólga - eldri")

undirliggjandi_old_tbl <- read_csv2(
  "https://px.hagstofa.is:443/pxis/sq/8643c932-380e-4c83-889a-14be1ad99354",
  na = "."
) |>
  drop_na() |>
  set_names(
    "date",
    "Kjarnavísitala 1",
    "Kjarnavísitala 2",
    "Kjarnavísitala 4"
  ) |>
  mutate(date = make_date(str_sub(date, 1, 4), str_sub(date, 6, 7))) %>%
  pivot_longer(cols = -date) %>%
  arrange(name, date) %>%
  group_by(name) %>%
  mutate(value = value / lag(value, 12) - 1) %>%
  drop_na() |>
  filter(date >= date_from) |>
  ungroup()


# 2.4.2 ný útgáfa ----
print("Undirliggjandi verðbólga - ný útgáfa")

undirliggjandi_new_tbl <- read_csv2(
  "https://px.hagstofa.is:443/pxis/sq/f039c5d1-fd5b-4a39-b7d8-d9766d6c4333"
) |>
  drop_na() |>
  set_names(
    "date",
    "Kjarnavísitala 1",
    "Kjarnavísitala 2",
    "Kjarnavísitala 4",
    "Kjarnavísitala 5"
  ) |>
  mutate(date = make_date(str_sub(date, 1, 4), str_sub(date, 6, 7))) %>%
  pivot_longer(cols = -date) %>%
  arrange(name, date) %>%
  group_by(name) %>%
  mutate(value = value / lag(value, 12) - 1) %>%
  drop_na() |>
  filter(date >= date_from) |>
  ungroup()


# 2.4.3 sameina ----
print("Undirliggjandi verðbólga - sameina")

undirliggjandi_tbl <-
  bind_rows(
    undirliggjandi_old_tbl |> filter(date < max(undirliggjandi_new_tbl$date)),
    undirliggjandi_new_tbl
  )


# 3.0.0 - WATERFALL - ----

# Based on this: https://www.r-bloggers.com/2019/05/basic-waterfall-graphs-in-r/
# https://rpubs.com/techanswers88/waterfall-chart-ggplot

# 3.1.0 Vísitölugildi ----

print("waterfall - vísitölugildi")


undirflokkar_raw_new_tbl <- read_csv2(
  "https://px.hagstofa.is:443/pxis/sq/70a2529f-327f-485e-9264-12977837bbae",
  # "https://px.hagstofa.is:443/pxis/sq/810ad986-60c1-471e-8f52-18f467e25ea2",
  na = "."
) |>
  janitor::clean_names() %>%
  fix_date() %>%
  select(-c(manudur, lidur)) %>%
  set_names("undirflokkur", "visitala", "date")


# 3.1.3 sameina raw undirflokka ----

# Find the overlap date (first month in new data that exists in old data)
# overlap_date <- min(undirflokkar_raw_new_tbl$date)

# # Calculate rebasing factors for each undirflokkur
# # Factor = old_visitala / new_visitala at overlap date
# rebase_factors <- undirflokkar_raw_old_tbl %>%
#   filter(date == overlap_date) %>%
#   select(undirflokkur, visitala_old = visitala) %>%
#   inner_join(
#     undirflokkar_raw_new_tbl %>%
#       filter(date == overlap_date) %>%
#       select(undirflokkur, visitala_new = visitala),
#     by = "undirflokkur"
#   ) %>%
#   mutate(factor = visitala_old / visitala_new) %>%
#   select(undirflokkur, factor)

# # Rebase new data and combine
# undirflokkar_raw_tbl <- bind_rows(
#   # Old data up to (but not including) the overlap date
#   undirflokkar_raw_old_tbl %>% filter(date < overlap_date),
#   # New data from overlap date onwards, rebased to old index
#   undirflokkar_raw_new_tbl %>%
#     left_join(rebase_factors, by = "undirflokkur") %>%
#     mutate(
#       factor = replace_na(factor, 1),
#       visitala = visitala * factor
#     ) %>%
#     select(-factor)
# )

# Bý til undirflokka án númers og vel þá undirflokka sem ég vil

print("waterfall - undirflokkar án númers")


undirflokkar_tbl <- undirflokkar_raw_new_tbl %>%
  mutate(
    visitala = as.numeric(visitala),
    numer_flokks = parse_number(undirflokkur),
    undirflokkur = str_trim(str_remove_all(undirflokkur, "[:digit:]")),
    nchar_undirfl = nchar(numer_flokks),
    to_select = case_when(
      nchar_undirfl == 2 | is.na(nchar_undirfl) ~ "select",
      str_detect(undirflokkur, "Póstur og sími") ~ "select",
      str_detect(undirflokkur, "Menntun") ~ "select",
      TRUE ~ "remove"
    )
  ) %>%
  filter(to_select == "select") %>%
  select(-c(to_select, numer_flokks, nchar_undirfl))


# 3.2.0 Vogir ----

print("waterfall - vogir")

manudir_tbl <- tibble(
  manudur = c("Mars", "Desember"),
  man_no = c(3, 12)
)

# Nýjar vogir
# desember 2025 á við 2026

undirflokkar_vogir_new_raw_tbl <- read_csv2(
  "https://px.hagstofa.is:443/pxis/sq/95692056-2ee8-43c6-82fa-9479ac9d6534"
) |>
  janitor::clean_names() |>
  separate(timi, c("manudur", "ar")) |>
  left_join(manudir_tbl) |>
  mutate(date = make_date(year = ar, month = man_no)) %>%
  select(date, undirvisitala, visitala_neysluverds) %>%
  set_names("date", "undirflokkur", "vog") %>%
  mutate(vog = as.numeric(vog), vog = vog / 10000) |>
  arrange(date, undirflokkur)

undirflokkar_vogir_new_raw_tbl <- undirflokkar_vogir_new_raw_tbl |>
  mutate(date = date %m+% months(1)) |>
  right_join(
    crossing(
      date = seq.Date(
        from = as.Date("2026-01-01"),
        to = max(undirflokkar_raw_new_tbl$date),
        by = "month"
      ),
      undirflokkur = unique(undirflokkar_vogir_new_raw_tbl$undirflokkur)
    ),
    by = c("date", "undirflokkur")
  ) |>
  arrange(undirflokkur, date) |>
  group_by(undirflokkur) |>
  fill(vog, .direction = "down") |>
  ungroup() |>
  drop_na(vog)

# 3.2.3 sameina vogir ----
# undirflokkar_vogir_tbl <- bind_rows(
#   undirflokkar_vogir_old_raw_tbl |>
#     filter(date < min(undirflokkar_vogir_new_raw_tbl$date)),
#   undirflokkar_vogir_new_raw_tbl
# )

print("waterfall - sameina vogir")

undirflokkar_vogir_tbl <- undirflokkar_vogir_new_raw_tbl %>%
  mutate(
    numer_flokks = parse_number(undirflokkur),
    undirflokkur = str_trim(str_remove_all(undirflokkur, "[:digit:]")),
    nchar_undirfl = nchar(numer_flokks),
    to_select = case_when(
      nchar_undirfl == 2 | is.na(nchar_undirfl) ~ "select",
      str_detect(undirflokkur, "Póstur og sími") ~ "select",
      str_detect(undirflokkur, "Menntun") ~ "select",
      TRUE ~ "remove"
    )
  ) %>%
  filter(to_select == "select") %>%
  select(-c(numer_flokks:to_select))

# 3.3.0 Sameina gögn ----

print("waterfall - sameina öll gögn")

undirflokkar_og_vogir_tbl <- undirflokkar_tbl %>%
  drop_na() |>
  arrange(date, undirflokkur) |>
  group_by(undirflokkur) |>
  mutate(
    verdbolga = visitala / lag(visitala, 12) - 1,
    verdbolga_1m = visitala / lag(visitala, 1) - 1
  ) |>
  drop_na() |>
  left_join(undirflokkar_vogir_tbl) %>%
  group_by(undirflokkur) %>%
  fill(vog, .direction = "down") %>%
  drop_na(vog) %>%
  ungroup() |>
  drop_na(visitala) |>
  mutate(ahrif = verdbolga * vog, ahrif_1m = verdbolga_1m * vog)

# 3.4.0 Úreikningar ----

print("waterfall - final útreikningar")

# Verð að splitta þeim upp og sameina aftur til að fá final verðbólgu neðst
undirflokkar_latest_tbl <- undirflokkar_og_vogir_tbl %>%
  filter(date == max(date)) %>%
  select(undirflokkur, ahrif, ahrif_1m) %>%
  filter(!undirflokkur == "Vísitala neysluverðs") %>%
  arrange(desc(ahrif))


# 3.5.0 Milli mánaða - tilbúin gögn frá Hagstofu ----
ahrif_a_visitolu_1m_tbl <-
  read_csv2(
    "https://px.hagstofa.is:443/pxis/sq/825f985f-8623-42f2-be9d-0d60e5ec1bbd"
  ) |>
  select(-2) |>
  set_names("date", "name", "value") |>
  mutate(
    value = as.numeric(value),
    date = make_date(str_sub(date, 1, 4), str_sub(date, 6, 7))
  ) |>
  filter(date == max(date)) |>
  filter(str_detect(name, "^\\d{3}\\s")) |>
  mutate(name = str_remove(name, "^\\d+\\s+"))


# 4.0.0 STÖPLARIT UNDIRFLOKKA ----

print("stöplarit undirflokka")


undirflokkar_12m_tbl <- undirflokkar_tbl %>%
  arrange(undirflokkur, date) %>%
  group_by(undirflokkur) %>%
  mutate(
    verdbolga = visitala / lag(visitala, 12) - 1
  ) %>%
  filter(date == max(date)) %>%
  ungroup() %>%
  select(undirflokkur, verdbolga) %>%
  pivot_wider(names_from = undirflokkur, values_from = verdbolga) %>%
  pivot_longer(cols = -"Vísitala neysluverðs") |>
  set_names("verdbolga", "name", "value")

if (all(is.na(undirflokkar_12m_tbl$verdbolga))) {
  undirflokkar_12m_tbl$verdbolga <- verdbolga_latest
}

undirflokkar_1m_tbl <- undirflokkar_tbl %>%
  arrange(undirflokkur, date) %>%
  group_by(undirflokkur) %>%
  mutate(
    verdbolga = visitala / lag(visitala, 1) - 1
  ) %>%
  filter(date == max(date)) %>%
  ungroup() %>%
  select(undirflokkur, verdbolga) %>%
  pivot_wider(names_from = undirflokkur, values_from = verdbolga) %>%
  pivot_longer(cols = -"Vísitala neysluverðs") |>
  set_names("verdbolga", "name", "value")


# 5.0.0 FROOP ----
print("Froop")

froop_flokkar_tbl <- read_excel(data_path, sheet = "froop") %>%
  janitor::clean_names()

# 5.1.0 Eldri flokkun ----

print("Froop - eldri")

# visitala
undirflokkar_raw_old_tbl <- read_csv2(
  "https://px.hagstofa.is:443/pxis/sq/ea481531-344b-45ba-b5bf-90f45bb08022",
  na = ".."
) %>%
  janitor::clean_names() %>%
  fix_date() %>%
  select(-manudur) %>%
  set_names("undirflokkur", "visitala", "date")

# vogir
undirflokkar_vogir_old_raw_tbl <- read_csv2(
  "https://px.hagstofa.is:443/pxis/sq/dac051cc-1094-449d-b638-b3a95ddf2b86"
) %>%
  janitor::clean_names() %>%
  separate(timi, c("manudur", "ar")) |>
  left_join(manudir_tbl) |>
  drop_na(man_no) |>
  mutate(date = make_date(year = ar, month = man_no)) %>%
  drop_na(date) %>%
  select(date, undirvisitala, visitala_neysluverds) %>%
  set_names("date", "undirflokkur", "vog") %>%
  mutate(vog = as.numeric(vog), vog = vog / 10000)


vnv_vogir_old_tbl <- undirflokkar_raw_old_tbl %>%
  drop_na(visitala) %>%
  left_join(undirflokkar_vogir_old_raw_tbl) %>%
  group_by(undirflokkur) %>%
  fill(vog, .direction = "down") %>%
  drop_na(vog) %>%
  ungroup()


# Bý til númer flokks
froop_old_tbl <- vnv_vogir_old_tbl %>%
  # Númer flokka
  mutate(
    numer_flokks = parse_number(undirflokkur),
    numer_flokks = case_when(
      is.na(numer_flokks) ~ "0000",
      nchar(numer_flokks) == 3 ~ paste0("0", numer_flokks),
      nchar(numer_flokks) == 2 ~ paste0("00", numer_flokks),
      nchar(numer_flokks) == 1 ~ paste0("000", numer_flokks),
      TRUE ~ as.character(numer_flokks)
    )
  ) %>%
  filter(numer_flokks %in% froop_flokkar_tbl$froop) %>%
  # Vogir
  group_by(date) %>%
  mutate(vog = vog / sum(vog)) %>%
  ungroup() %>%

  # Froop vísitala
  mutate(
    visitala = as.numeric(visitala),
    impact = visitala * vog
  ) %>%
  group_by(date) %>%
  summarise(froop = sum(impact)) %>%
  mutate(infl = froop / lag(froop, 12) - 1) %>%
  drop_na() %>%
  select(date, infl, froop) %>%
  set_names("date", "froop", "froop_index")


non_froop_old_tbl <- vnv_vogir_old_tbl %>%
  # Númer flokka
  mutate(
    numer_flokks = parse_number(undirflokkur),
    numer_flokks = case_when(
      is.na(numer_flokks) ~ "0000",
      nchar(numer_flokks) == 3 ~ paste0("0", numer_flokks),
      nchar(numer_flokks) == 2 ~ paste0("00", numer_flokks),
      nchar(numer_flokks) == 1 ~ paste0("000", numer_flokks),
      TRUE ~ as.character(numer_flokks)
    )
  ) %>%
  filter(!numer_flokks %in% froop_flokkar_tbl$froop) %>%
  # Vogir
  group_by(date) %>%
  mutate(vog = vog / sum(vog)) %>%
  ungroup() %>%

  # Froop vísitala
  mutate(
    visitala = as.numeric(visitala),
    impact = visitala * vog
  ) %>%
  group_by(date) %>%
  summarise(index = sum(impact)) %>%
  mutate(infl = index / lag(index, 12) - 1) %>%
  drop_na() %>%
  select(date, infl, index) %>%
  set_names("date", "non_froop", "non_froop_index")

# skeiti við vnv
froop_old_tbl <- froop_old_tbl %>%
  left_join(non_froop_old_tbl) %>%
  left_join(
    vnv_tbl %>%
      filter(flokkur == "Verðbólga") %>%
      select(date, verdbolga, visitala)
  ) |>
  filter(date >= date_from)

# 5.2.0 ný flokkun ----

print("froop - ný útgáfa")

vnv_vogir_tbl <- undirflokkar_raw_new_tbl %>%
  drop_na(visitala) %>%
  left_join(undirflokkar_vogir_new_raw_tbl) %>%
  group_by(undirflokkur) %>%
  fill(vog, .direction = "down") %>%
  drop_na(vog) %>%
  ungroup()


# Bý til númer flokks
froop_tbl <- vnv_vogir_tbl %>%
  # Númer flokka
  mutate(
    numer_flokks = parse_number(undirflokkur),
    numer_flokks = case_when(
      is.na(numer_flokks) ~ "0000",
      nchar(numer_flokks) == 3 ~ paste0("0", numer_flokks),
      nchar(numer_flokks) == 2 ~ paste0("00", numer_flokks),
      nchar(numer_flokks) == 1 ~ paste0("000", numer_flokks),
      TRUE ~ as.character(numer_flokks)
    )
  ) %>%
  filter(numer_flokks %in% froop_flokkar_tbl$froop) %>%
  # Vogir
  group_by(date) %>%
  mutate(vog = vog / sum(vog)) %>%
  ungroup() %>%

  # Froop vísitala
  mutate(
    visitala = as.numeric(visitala),
    impact = visitala * vog
  ) %>%
  group_by(date) %>%
  summarise(froop = sum(impact)) %>%
  mutate(infl = froop / lag(froop, 12) - 1) %>%
  drop_na() %>%
  select(date, infl, froop) %>%
  set_names("date", "froop", "froop_index")


non_froop_tbl <- vnv_vogir_tbl %>%
  # Númer flokka
  mutate(
    numer_flokks = parse_number(undirflokkur),
    numer_flokks = case_when(
      is.na(numer_flokks) ~ "0000",
      nchar(numer_flokks) == 3 ~ paste0("0", numer_flokks),
      nchar(numer_flokks) == 2 ~ paste0("00", numer_flokks),
      nchar(numer_flokks) == 1 ~ paste0("000", numer_flokks),
      TRUE ~ as.character(numer_flokks)
    )
  ) %>%
  filter(!numer_flokks %in% froop_flokkar_tbl$froop) %>%
  # Vogir
  group_by(date) %>%
  mutate(vog = vog / sum(vog)) %>%
  ungroup() %>%

  # Froop vísitala
  mutate(
    visitala = as.numeric(visitala),
    impact = visitala * vog
  ) %>%
  group_by(date) %>%
  summarise(index = sum(impact)) %>%
  mutate(infl = index / lag(index, 12) - 1) %>%
  drop_na() %>%
  select(date, infl, index) %>%
  set_names("date", "non_froop", "non_froop_index")


# Skeiti við vnv ----
print("Froop - bæti vnv við")
froop_tbl <- froop_tbl %>%
  left_join(non_froop_tbl) %>%
  left_join(
    vnv_tbl %>%
      filter(flokkur == "Verðbólga") %>%
      select(date, verdbolga, visitala)
  ) |>
  filter(date >= date_from)


# 5.3.0 Sameina eldri og nýrri ----
froop_final_tbl <- froop_old_tbl |>
  bind_rows(froop_tbl)


# 6.0.0 EUROSTAT - HICP ----

print("EUROSTAT - HICP")

hicp_raw_tbl <- get_eurostat(
  id = "prc_hicp_midx",
  filters = list(
    geo = c("IS", "DK", "NO", "FI", "SE", "EU27_2020"),
    coicop = c(
      paste0("CP0", 0:9),
      "CP10",
      "CP11",
      "CP12",
      "TOT_X_NRG",
      "SERV",
      "GD",
      "FOOD"
    ),
    unit = "I15",
    freq = "M"
  ),
  time_format = "date",
  cache = FALSE
) %>%
  as_tibble()


if (class(unique(hicp_raw_tbl$time)) == "Date") {
  hicp_tbl <- hicp_raw_tbl %>%
    select(-c(unit, freq)) %>%
    rename(
      "date" = "time",
      "flokkur" = "coicop",
      "svaedi" = "geo",
      "value" = "values"
    )
} else {
  hicp_tbl <- hicp_raw_tbl %>%
    mutate(date = ym(time)) %>%
    select(-c(time, unit, freq)) %>%
    rename(
      "flokkur" = "coicop",
      "svaedi" = "geo",
      "value" = "values"
    )
}

hicp_infl_tbl <- hicp_tbl %>%
  arrange(date, flokkur, svaedi) %>%
  group_by(flokkur, svaedi) %>%
  mutate(
    infl = value / lag(value, 12) - 1,
    infl_3m = value / lag(value, 3) - 1
  ) %>%
  drop_na() %>%
  ungroup()


# Finn minnsta samnefnara í date
max_date_hicp <- hicp_infl_tbl %>%
  filter(flokkur == "CP00") %>%
  group_by(svaedi) %>%
  filter(date == max(date)) %>%
  ungroup() %>%
  count(date) %>%
  filter(n == max(n)) %>%
  pull(date)

hicp_infl_tbl <- hicp_infl_tbl %>%
  filter(date <= max_date_hicp)


# Preparation for power bi
land_tbl <- tibble(
  svaedi = c("DK", "FI", "IS", "NO", "SE", "EU27_2020"),
  country = c("Danmörk", "Finnland", "Ísland", "Noregur", "Svíþjóð", "EU")
)

hicp_pbi_tbl <- hicp_infl_tbl %>%
  filter(flokkur == "CP00") %>%
  left_join(land_tbl) |>
  filter(date >= date_from)


# 6.1.0 By Coicip ----

coicop_flokkar_tbl <- tibble(
  coicop = paste0(
    "CP",
    c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")
  ),
  flokkur = c(
    "Matur og drykkjarvörur",
    "Áfengi og tóbak",
    "Föt og skór",
    "Húsnæði, hiti og rafmagn",
    "Húsgögn, heimilisbúnaður o.fl.",
    "Heilsa",
    "Ferðir og flutningar",
    "Póstur og sími",
    "Tómstundir og menning",
    "Menntun",
    "Hótel og veitingastaðir",
    "Aðrar vörur og þjónusta"
  )
)

coicop_inflation_tbl <- hicp_raw_tbl %>%
  filter(
    coicop %in%
      paste0(
        "CP",
        c(
          "01",
          "02",
          "03",
          "04",
          "05",
          "06",
          "07",
          "08",
          "09",
          "10",
          "11",
          "12"
        )
      )
  ) %>%
  filter(
    geo %in% c("IS", "EU27_2020"),
    unit == "I15",
    freq == "M"
  ) %>%
  arrange(coicop, geo, time) %>%
  group_by(coicop, geo) %>%
  mutate(inflation = values / lag(values, 12) - 1) %>%
  left_join(coicop_flokkar_tbl) %>%
  rename("date" = "time")


# HICP undirflokkar - power bi
hicp_undirflokkar_tbl <- hicp_infl_tbl %>%
  filter(
    flokkur %in% c("GD", "SERV", "TOT_X_NRG", "FOOD"),
    date == max(date)
  ) %>%
  mutate(
    flokkur = case_when(
      flokkur == "GD" ~ "Vörur",
      flokkur == "SERV" ~ "Þjónusta",
      flokkur == "FOOD" ~ "Matur",
      TRUE ~ "Verðbólga án orkugjafa"
    ),
    flokkur = enc2utf8(flokkur)
  ) %>%
  left_join(land_tbl)


# HIPC valuebox
hicp_valuebox_tbl <- hicp_pbi_tbl %>%
  filter(date == max(date)) %>%
  select(country, infl) %>%
  pivot_wider(names_from = country, values_from = infl)


# 7.0.0 HÚSNÆÐISVERÐ ----

# 7.1.0 eldri ----
print("Húsnæðisverð - eldri gögn")

hus_eldri_tbl <-
  read_csv2(
    "https://px.hagstofa.is:443/pxis/sq/34cafc5e-5977-4ea3-a5ce-1ea804fe506c"
  )

hus_unnid_eldri_tbl <- hus_eldri_tbl %>%
  mutate(Undirvísitala = str_remove(Undirvísitala, "^\\d+\\s*")) %>%
  set_names("date", "lidur", "visitala") %>%
  mutate(
    date = make_date(str_sub(date, 1, 4), str_sub(date, 6, 7))
  ) %>%
  arrange(date, lidur) %>%
  group_by(lidur) %>%
  mutate(infl = visitala / lag(visitala, 12) - 1) %>%
  drop_na() %>%
  ungroup() |>
  filter(date >= date_from)

# 7.2.0 nýrri ----
print("Húsnæðisverð - nýrri gögn")

hus_tbl <- read_csv2(
  "https://px.hagstofa.is:443/pxis/sq/81e52150-17d5-45a9-9d1d-7f6735b00ef6"
) |>
  select(-2)

hus_unnid_tbl <- hus_tbl %>%
  mutate(Undirvísitala = str_remove(Undirvísitala, "^\\d+\\s*")) %>%
  set_names("date", "lidur", "visitala") %>%
  mutate(
    date = make_date(str_sub(date, 1, 4), str_sub(date, 6, 7))
  ) %>%
  arrange(date, lidur) %>%
  group_by(lidur) %>%
  mutate(infl = visitala / lag(visitala, 12) - 1) %>%
  drop_na() %>%
  ungroup() |>
  filter(date >= date_from)

# sameina
hus_unnid_tbl <- bind_rows(
  hus_unnid_eldri_tbl,
  hus_unnid_tbl
)

# 7.3.0 Vextir ----
print("Vextir bankanna")

# Vextir bankanna
vextir_tbl <- readxl::read_excel(data_path, sheet = "vextir") |>
  mutate(
    date = date(date),
    Óverðtryggðir = Óverðtryggðir / 100,
    Verðtryggðir = Verðtryggðir / 100
  ) |>
  filter(date >= date_from)


# 7.3.1 Meginvextir ----
print("Hleð inn meginvexti")

meginvextir_tbl <- read_excel(data_path, sheet = "meginvextir") %>%
  set_names("date", "Meginvextir")

meginvextir_tbl <- meginvextir_tbl %>%
  mutate(
    date = dmy(date),
    date = floor_date(date, "month"),
    Meginvextir = as.numeric(str_replace(Meginvextir, ",", ".")) / 100
  ) %>%
  group_by(date) %>%
  summarise(Meginvextir = mean(Meginvextir))


vextir_tbl <- vextir_tbl %>%
  left_join(meginvextir_tbl) %>%
  drop_na() |>
  select(date, everything())


# 8.0.0 ÚTLÁN ----

print("Útlán")

# 8.1.0 Útlánastabbi ----

# Innlánsstofnanir
print("Útlán innlánsstofnana")
bankakerfi_tbl <- read_xlsx(data_path, "bank_stabbi") %>%
  slice(6, 9) %>%
  select(-c(1, 2)) %>%
  mutate(type = c("banki_vtr", "banki_ovtr")) %>%
  pivot_longer(cols = -type) %>%
  drop_na() %>%
  select(-name) %>%
  pivot_wider(names_from = type, values_from = value) %>%
  unnest(everything()) %>%
  mutate(
    date = seq.Date(
      from = as.Date("1997-12-01"),
      length.out = nrow(.),
      by = "month"
    )
  )


# Lífeyrissjóðir
print("Útlán lífeyrissjóða")

lifeyrissjodir_tbl <- read_xlsx(data_path, "lif_stabbi") %>%
  slice(16, 19) %>%
  select(-1) %>%
  mutate(type = c("lif_vtr", "lif_ovtr")) %>%
  pivot_longer(cols = -type) %>%
  drop_na() %>%
  select(-name) %>%
  pivot_wider(names_from = type, values_from = value) %>%
  unnest(everything()) %>%
  mutate(across(everything(), as.numeric)) %>%
  mutate(
    date = seq.Date(
      from = as.Date("1997-01-01"),
      length.out = nrow(.),
      by = "month"
    )
  )


# Lánasjóðir ríkisins
print("Útlán lánasjóða ríkisins")

lsj_tbl <- read_xlsx(data_path, "lanasjodir_stabbi") %>%
  slice(16, 19) %>%
  select(-1) %>%
  mutate(type = c("lanasjodir_vtr", "lanasjodir_ovtr")) %>%
  pivot_longer(cols = -type) %>%
  drop_na() %>%
  select(-name) %>%
  pivot_wider(names_from = type, values_from = value) %>%
  unnest(everything()) %>%
  mutate(across(everything(), as.numeric)) %>%
  mutate(
    date = seq.Date(
      from = as.Date("1992-03-01"),
      length.out = nrow(.),
      by = "month"
    )
  )


# sameina stabba
utlan_stada_tbl <- bankakerfi_tbl %>%
  left_join(lifeyrissjodir_tbl, by = "date") %>%
  left_join(lsj_tbl, by = "date") %>%
  drop_na() %>%
  mutate(
    "Verðtryggð" = (banki_vtr + lif_vtr + lanasjodir_vtr),
    "Óverðtryggð" = c(banki_ovtr + lif_ovtr + lanasjodir_ovtr)
  ) %>%
  select(-contains("_")) %>%
  pivot_longer(cols = -date) %>%
  group_by(date) %>%
  mutate(share = value / sum(value)) %>%
  filter(date >= date_from) %>%
  ungroup()


# 8.2.0 Ný útlán ----

# Lífeyrissjóðir
print("Ný útlán lífeyrissjóða")

lif_ny_utlan <- read_excel(data_path, "lif_ny_utlan") |>
  slice(4, 5) |>
  select(-c(1)) |>
  mutate(key = c("vtr", "ovtr")) |>
  pivot_longer(cols = -key) |>
  select(-name) |>
  pivot_wider(names_from = key, values_from = value) |>
  unnest(everything()) %>%
  mutate(across(everything(), as.numeric)) %>%
  mutate(
    date = seq.Date(
      from = as.Date("2009-01-01"),
      length.out = nrow(.),
      by = "month"
    )
  )


# Innlánsstofnanir
print("Ný útlán innlánsstofnana")
banki_ny_utlan_tbl <- read_excel(data_path, "bank_ny_utlan") |>
  slice(72, 73, 110, 111) |>
  select(-c(1)) |>
  mutate(across(is.character, as.numeric)) |>
  mutate(key = c("br_ovt", "fst_ovt", "br_vt", "fst_vt")) |>
  pivot_longer(cols = -key) |>
  select(-name) |>
  pivot_wider(names_from = key, values_from = value) |>
  unnest(everything()) %>%
  mutate(across(everything(), as.numeric)) %>%
  mutate(
    date = seq.Date(
      from = as.Date("2013-01-01"),
      length.out = nrow(.),
      by = "month"
    )
  )


# sameina ný útlán
ny_utlan_tbl <- lif_ny_utlan %>%
  left_join(banki_ny_utlan_tbl, by = "date") %>%
  drop_na()

ny_utlan_tbl <- ny_utlan_tbl %>%
  mutate(
    "Verðtryggð lán" = (vtr + br_vt + fst_vt),
    "Óverðtryggð lán" = (ovtr + br_ovt + fst_ovt)
  ) %>%
  select(date, contains("lán")) |>
  pivot_longer(cols = -date) |>
  filter(date >= date_from)


# 9.0.0 GENGI ----
print("Sæki upplýsingar um gengi af heimasíðu Seðlabankans")

get_si_gengi <- function(gjaldmidill) {
  currency_id <- switch(
    gjaldmidill,
    "eur" = 4064,
    "usd" = 4055,
    "visitala" = 4117
  )

  path_to_data <- paste0(
    "http://www.sedlabanki.is/xmltimeseries/Default.aspx?DagsFra=2005-01-01&DagsTil=",
    today(),
    "T00:00:00&TimeSeriesID=",
    currency_id,
    "&Type=xml"
  )

  doc <- GET(path_to_data)

  xml_1 <- xml2::read_xml(content(doc, "text"))
  xml_2 <- xmlParse(xml_1)

  gengi_tbl <- xmlToDataFrame(nodes = getNodeSet(xml_2, "//Entry")) %>%
    as_tibble() %>%
    set_names("date", "gengi")

  gengi_tbl %>%
    mutate(
      date = as.Date(mdy_hms(date)),
      gengi = as.numeric(gengi)
    ) %>%
    set_names("date", gjaldmidill)
}

# EUR: gjaldmidill = eur
# USD: gjaldmidill = usd
# gengisvíaitala: gjaldmidill = visitala

gengi_tbl <- get_si_gengi("eur") %>%
  left_join(get_si_gengi("usd")) %>%
  left_join(get_si_gengi("visitala")) %>%
  set_names("date", "EUR", "USD", "Gengisvísitala") %>%
  pivot_longer(cols = -date) %>%
  mutate(date = floor_date(date, "month")) %>%
  group_by(date, name) %>%
  summarise(value = mean(value)) |>
  filter(date >= date_from)


# 10.0.0 INNGRIP SEÐLABANKANS Á GJALDEYRISMARKAÐ ----

print("Inngrip seðlabankans")

inngrip_si_tbl <- read_xlsx(data_path, "inngrip_si") |>
  janitor::clean_names() |>
  select(date_reverse, velta_i_isk, fe_buy_m_kr, fe_sell_m_kr) |>
  set_names("date", "velta", "kaup", "sala") |>
  mutate(
    date = floor_date(dmy(date), "month"),
    velta = as.numeric(str_replace(velta, ",", ".")),
    kaup = as.numeric(str_replace(kaup, ",", ".")),
    sala = as.numeric(str_replace(sala, ",", "."))
  ) |>
  group_by(date) |>
  summarise(
    velta = sum(velta),
    kaup = sum(kaup),
    sala = sum(sala)
  ) |>
  mutate(kaup_hlutfall_velta = kaup / velta) |>
  filter(date < floor_date(today(), "month"))

inngrip_si_tbl <- inngrip_si_tbl |>
  left_join(
    gengi_tbl |>
      ungroup() |>
      filter(name == "EUR") |>
      select(-name) |>
      rename("EURISK" = "value")
  ) |>
  filter(date >= date_from)

# 11.0.0 VERÐBÓLGUVÆNTINGAR ----

print("Verðbólguvæntingar")

get_infl_exp <- function(exp_path, rows, sheet_name, date_from) {
  read_excel(exp_path, sheet = sheet_name) |>
    slice(rows) |>
    pivot_longer(cols = everything()) |>
    select(2) |>
    slice(-1) |>
    mutate(
      date = seq.Date(
        from = as.Date(date_from),
        length.out = nrow(cur_data()),
        by = "quarter"
      ),
      value = as.numeric(value) / 100
    ) |>
    drop_na()
}

# Heimili
infl_exp_heimili_tbl <- get_infl_exp(
  exp_path = exp_path,
  rows = 9,
  sheet_name = "Heimili_Households",
  date_from = "2003-01-01"
) |>
  mutate(key = "Heimili")

# Fyrirtæki
infl_exp_fyrirtaeki_tbl <- get_infl_exp(
  exp_path = exp_path,
  rows = 9,
  sheet_name = "Fyrirtæki_Businesses",
  date_from = "2003-01-01"
) |>
  mutate(key = "Fyrirtæki")

# Markaðsaðilar
infl_exp_markadsadilar_tbl <- read_excel(
  "data/Vaentingar_markadsadila.xlsx",
  sheet = "III-a"
) |>
  slice(9) |>
  select(-1) |>
  pivot_longer(cols = everything()) |>
  select(-name) |>
  mutate(
    date = seq.Date(
      from = as.Date("2012-01-01"),
      length.out = nrow(cur_data()),
      by = "quarter"
    ),
    value = as.numeric(value) / 100
  ) |>
  mutate(key = "Markaðsaðilar")


# sameina
infl_exp_tbl <- bind_rows(
  infl_exp_heimili_tbl,
  infl_exp_fyrirtaeki_tbl,
  infl_exp_markadsadilar_tbl
) |>
  filter(date >= date_from)

# Skuldabréfamarkaður
infl_exp_breakeven_tbl <- read_excel(
  exp_path,
  sheet = "Verðbólguálag_Breakeven rates"
) |>
  slice(4:7) |>
  pivot_longer(cols = -1) |>
  select(-name) |>
  set_names("bref", "value") |>
  mutate(value = as.numeric(value) / 100) |>
  pivot_wider(names_from = bref, values_from = value) |>
  unnest(everything()) |>
  set_names(
    "Verðbólguálag til 1 árs",
    "Verðbólguálag til 2 ára",
    "Verðbólguálag til 5 ára",
    "Verðbólguálag til 10 ára"
  ) |>
  mutate(
    date = seq.Date(
      from = as.Date("2003-01-01"),
      length.out = nrow(cur_data()),
      by = "quarter"
    ),
  ) |>
  pivot_longer(cols = -date) |>
  rename("key" = "name") |>
  filter(date >= date_from)


# # 11.1.0 Skuldabréfamarkaðurinn ----
# print("Skuldabréfamarkaðurinn")

# combine_interpolate_bonds <- function(
#   overdtryggd_tbl,
#   verdtryggd_tbl,
#   method = "linear"
# ) {
#   # Create sequence of monthly maturities
#   date_seq <- seq(
#     min(overdtryggd_tbl$maturity),
#     max(overdtryggd_tbl$maturity),
#     by = "month"
#   )

#   # Define interpolation function based on chosen method
#   interpolate_yield <- function(maturity, yield, xout, method) {
#     if (method == "linear") {
#       return(
#         approx(maturity, yield, xout = xout, method = "linear", rule = 2)$y
#       )
#     } else if (method == "spline") {
#       return(spline(maturity, yield, xout = xout, method = "natural")$y)
#     } else {
#       stop("Invalid method. Choose either 'linear' or 'spline'.")
#     }
#   }

#   # Apply interpolation
#   indexed_yield <- interpolate_yield(
#     verdtryggd_tbl$maturity,
#     verdtryggd_tbl$yield,
#     date_seq,
#     method
#   )
#   non_indexed_yield <- interpolate_yield(
#     overdtryggd_tbl$maturity,
#     overdtryggd_tbl$yield,
#     date_seq,
#     method
#   )

#   # Create interpolated yield tables
#   indexed_tbl <- tibble(maturity = date_seq, yield = indexed_yield)
#   non_indexed_tbl <- tibble(maturity = date_seq, yield = non_indexed_yield)

#   # Join datasets and calculate break-even inflation
#   combined_tbl <- inner_join(
#     indexed_tbl,
#     non_indexed_tbl,
#     by = "maturity",
#     suffix = c("_indexed", "_non_indexed")
#   ) %>%
#     mutate(
#       break_even_inflation = (1 + yield_non_indexed) / (1 + yield_indexed) - 1
#     ) %>%
#     select(maturity, break_even_inflation)

#   return(combined_tbl)
# }

# combine_interpolate_bonds_multiple_days <- function(
#   overdtryggd,
#   verdtryggd,
#   method = "linear"
# ) {
#   # Ensure we have valid dates
#   unique_dates <- unique(c(overdtryggd$date, verdtryggd$date))

#   # Nest data by date
#   nested_data <- tibble(date = unique_dates) %>%
#     mutate(
#       overd_data = map(date, ~ filter(overdtryggd, date == .x) %>% drop_na()),
#       verd_data = map(date, ~ filter(verdtryggd, date == .x) %>% drop_na())
#     )

#   # Apply function to each nested date
#   results <- nested_data %>%
#     mutate(
#       interpolated = map2(
#         overd_data,
#         verd_data,
#         ~ {
#           if (nrow(.x) == 0 || nrow(.y) == 0) {
#             return(NULL)
#           } # Handle missing cases
#           combine_interpolate_bonds(.x, .y, method = method)
#         }
#       )
#     ) %>%
#     unnest(interpolated) %>%
#     select(date, maturity, break_even_inflation)

#   return(results)
# }

# # 11.1.1 Söguleg gögn ----

# # * RIKB ----
# rikb_files <- list.files(paste0(base_path, "/00_data/skuldabref/"), pattern = "rikb_")

# rikb_ls <- rikb_files %>%
#   paste0(base_path, "/00_data/skuldabref/", .) %>%
#   map(., .f = function(x) read_csv2(x))

# names(rikb_ls) <- rikb_files

# rikb_tbl <- rikb_ls %>%
#   bind_rows(.id = "bref") %>%
#   mutate(
#     maturity = str_remove(bref, "rikb_|"),
#     maturity = str_remove(maturity, ".csv"),
#     maturity = ymd(maturity),
#     date = dmy(DateTime)
#   ) %>%
#   select(-DateTime) %>%
#   set_names("bref", "yield", "maturity", "date") %>%
#   mutate(
#     yield = yield / 100,
#     key   = "ovt"
#   )

# # Bý til smooth feril fyrir yield. Of mikið noise ef valinn er einn dagur
# rikb_tbl <- rikb_tbl %>%
#   arrange(bref, date) %>%
#   mutate(yield = rollapplyr(yield, width = 10, FUN = mean, partial = TRUE))

# # * RIKS ----
# riks_files <- list.files(paste0(base_path, "/00_data/skuldabref/"), pattern = "riks_")

# riks_ls <- riks_files %>%
#   paste0(base_path, "/00_data/skuldabref/", .) %>%
#   map(., .f = function(x) read_csv2(x))

# names(riks_ls) <- riks_files

# riks_tbl <- riks_ls %>%
#   bind_rows(.id = "bref") %>%
#   mutate(
#     maturity = str_remove(bref, "riks_|"),
#     maturity = str_remove(maturity, ".csv"),
#     maturity = ymd(maturity),
#     date = dmy(DateTime)
#   ) %>%
#   select(-DateTime) %>%
#   set_names("bref", "yield", "maturity", "date") %>%
#   mutate(
#     yield = yield / 100,
#     key   = "vt"
#   )

# # Bý til smooth feril fyrir yield. Of mikið noise ef valinn er einn dagur
# riks_tbl <- riks_tbl %>%
#   arrange(bref, date) %>%
#   mutate(yield = rollapplyr(yield, width = 10, FUN = mean, partial = TRUE))

# # * Verðbólguálag ----

# combined_bonds_over_time <- combine_interpolate_bonds_multiple_days(
#   rikb_tbl %>% filter(date >= "2023-10-01"),
#   riks_tbl %>% filter(date >= "2023-10-01")
# )

# # Define the current month
# current_month <- floor_date(Sys.Date(), "month")

# # Filter for the last 3 months
# last_3_months_dates <- seq(from = current_month - months(3),
#                            to = current_month - months(1),
#                            by = "month")

# # Find the last date available in each of the past 3 months
# dates_vector <- tibble(date = last_3_months_dates) %>%
#   mutate(last_available_date = map(date, ~ {
#     last_date <- max(
#       filter(combined_bonds_over_time, floor_date(date, "month") == .x)$date,
#       na.rm = TRUE
#     )
#     return(last_date)
#   })) %>%
#   unnest(last_available_date) %>%
#   pull(last_available_date)

# combined_bonds_over_time <- combined_bonds_over_time %>%
#   filter(date %in% c(today(), dates_vector))

# combined_bonds_over_time <- combined_bonds_over_time %>%
#   mutate(index = as.numeric(as_factor(as.character(date))))

# 12.0.0 Top 10 listinn ----
# Fínasta skipting hvers flokks. Finna svo top og botn 10 flokkana
# varðandi 12 mánaða og 1 mánaða breytingu

print("Top 10 listinn")

vnv_allir_flokkar_tbl <- read_csv2(
  "https://px.hagstofa.is:443/pxis/sq/aa943ac2-3b2f-46e8-8e57-99d3517cf4db",
  na = "."
) |>
  select(-2) |>
  set_names("date", "flokkur", "visitala") |>
  drop_na()


# 1) Finna "leaf" kóða (þ.e. kóða sem EKKI eru forskeyti fyrir neinum öðrum kóða)
cpi_leaf <- vnv_allir_flokkar_tbl %>%
  mutate(
    code = stringr::str_extract(flokkur, "^\\d+") # ná í tölukóðann fremst (ef til er)
  )

codes_tbl <- cpi_leaf %>%
  filter(!is.na(code)) %>%
  distinct(code)

leaf_codes <- codes_tbl %>%
  mutate(
    has_child = purrr::map_lgl(
      code,
      ~ any(codes_tbl$code != .x & stringr::str_starts(codes_tbl$code, .x))
    )
  ) %>%
  filter(!has_child) %>%
  pull(code)

# 2) Sía á leaf-kóða og fjarlægja tölukóðann úr heitinu
cpi_detailed <- cpi_leaf %>%
  filter(!is.na(code), code %in% leaf_codes) %>%
  mutate(
    flokkur = stringr::str_remove(flokkur, "^\\d+\\s+"),
    flokkur = case_when(
      code == "01221" ~ "Vatn (drykkjarvörur)",
      code == "0443" ~ "Vatn (innan heimilis)",
      TRUE ~ flokkur
    )
  ) %>%
  select(date, flokkur, visitala) |>
  mutate(visitala = as.numeric(visitala))

infl_allir_flokkar_tbl <- cpi_detailed |>
  mutate(date = make_date(str_sub(date, 1, 4), str_sub(date, 6, 7))) |>
  arrange(date, flokkur) |>
  group_by(flokkur) |>
  mutate(
    infl_1m = visitala / lag(visitala) - 1,
    infl_12m = visitala / lag(visitala, 12) - 1
  ) |>
  ungroup() |>
  drop_na() |>
  filter(date == max(date))

# bottom 10
botn_1m_tbl <- infl_allir_flokkar_tbl |>
  arrange(infl_1m) |>
  slice_head(n = 10) |>
  select(flokkur, infl_1m)

botn_12m_tbl <- infl_allir_flokkar_tbl |>
  arrange(infl_12m) |>
  slice_head(n = 10) |>
  select(flokkur, infl_12m)

# top 10
top_1m_tbl <- infl_allir_flokkar_tbl |>
  arrange(desc(infl_1m)) |>
  slice_head(n = 10) |>
  select(flokkur, infl_1m)

top_12m_tbl <- infl_allir_flokkar_tbl |>
  arrange(desc(infl_12m)) |>
  slice_head(n = 10) |>
  select(flokkur, infl_12m)


# 13.0.0 Umfang verðhækkana ----

print("Umfang verðhækkana - heatmap")

manudir_tbl <-
  tibble(
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

# 13.1.0 eldri ----

vnv_allir_flokkar_old_tbl <-
  read_csv2(
    "https://px.hagstofa.is:443/pxis/sq/73fd14fd-ab13-44ff-8f69-b888199b5d8f"
  ) |>
  set_names("date", "flokkur", "visitala") |>
  drop_na()


# 1) Finna "leaf" kóða (þ.e. kóða sem EKKI eru forskeyti fyrir neinum öðrum kóða)
cpi_leaf_old <- vnv_allir_flokkar_old_tbl %>%
  mutate(
    code = stringr::str_extract(flokkur, "^\\d+") # ná í tölukóðann fremst (ef til er)
  )

codes_old_tbl <- cpi_leaf_old %>%
  filter(!is.na(code)) %>%
  distinct(code)

leaf_codes_old <- codes_old_tbl %>%
  mutate(
    has_child = purrr::map_lgl(
      code,
      ~ any(
        codes_old_tbl$code != .x & stringr::str_starts(codes_old_tbl$code, .x)
      )
    )
  ) %>%
  filter(!has_child) %>%
  pull(code)

# 2) Sía á leaf-kóða og fjarlægja tölukóðann úr heitinu
cpi_detailed_old <- cpi_leaf_old %>%
  filter(!is.na(code), code %in% leaf_codes_old) %>%
  mutate(
    flokkur = stringr::str_remove(flokkur, "^\\d+\\s+"),
    flokkur = case_when(
      code == "01221" ~ "Vatn (drykkjarvörur)",
      code == "0443" ~ "Vatn (innan heimilis)",
      TRUE ~ flokkur
    )
  ) %>%
  select(date, flokkur, visitala) |>
  mutate(visitala = as.numeric(visitala))


# 13.1.1 vogir old ----

vnv_vog_old_tbl <- read_csv2(
  "https://px.hagstofa.is:443/pxis/sq/ab5cf065-2286-4011-bdbe-ec5185dd6364"
) |>
  janitor::clean_names()

vnv_vog_old_tbl <- vnv_vog_old_tbl |>
  separate_wider_delim(timi, delim = " ", names = c("manudur", "ar")) |>
  left_join(manudir_tbl) |>
  mutate(date = make_date(ar, man_no)) |>
  select(date, undirvisitala, visitala_neysluverds) |>
  set_names("date", "flokkur", "vog") |>
  mutate(flokkur = str_remove(flokkur, "^\\d+\\s*")) |>
  drop_na()


# 13.1.2 umfang ----
infl_umfang_old_tbl <- cpi_detailed_old |>
  mutate(date = make_date(str_sub(date, 1, 4), str_sub(date, 6, 7))) |>
  arrange(date, flokkur) |>
  group_by(flokkur) |>
  mutate(
    infl = visitala / lag(visitala, 12) - 1
  ) |>
  ungroup() |>
  drop_na()


infl_umfang_old_tbl <- infl_umfang_old_tbl |>
  left_join(
    vnv_vog_old_tbl
  ) |>
  group_by(flokkur) |>
  fill(vog, .direction = "down") |>
  filter(date >= "2020-01-01") |>
  mutate(
    vog = as.numeric(vog),
    haekkanir = case_when(
      infl < 0 ~ "Verðlækkun",
      infl < 0.025 ~ "0-2,5%",
      infl < 0.05 ~ "2,5-5%",
      infl < 0.075 ~ "5-7,5%",
      infl < 0.1 ~ "7,5%-10%",
      TRUE ~ "Meira en 10%"
    )
  ) |>
  group_by(date) |>
  mutate(vog = vog / sum(vog, na.rm = TRUE)) |>
  group_by(date, haekkanir) |>
  summarise(vaegi = sum(vog, na.rm = TRUE)) |>
  ungroup()


infl_umfang_old_tbl <- infl_umfang_old_tbl |>
  mutate(
    haekkanir = factor(
      haekkanir,
      levels = c(
        "Meira en 10%",
        "7,5%-10%",
        "5-7,5%",
        "2,5-5%",
        "0-2,5%",
        "Verðlækkun"
      )
    )
  )


# 13.2.0 nýrri ----

vnv_vog_tbl <- undirflokkar_vogir_new_raw_tbl |>
  set_names("date", "flokkur", "vog") |>
  mutate(
    flokkur = str_trim(str_remove_all(flokkur, "[:digit:]"))
  )


infl_umfang_tbl <- cpi_detailed |>
  mutate(date = make_date(str_sub(date, 1, 4), str_sub(date, 6, 7))) |>
  arrange(date, flokkur) |>
  group_by(flokkur) |>
  mutate(
    infl = visitala / lag(visitala, 12) - 1
  ) |>
  ungroup() |>
  drop_na()


infl_umfang_tbl <- infl_umfang_tbl |>
  left_join(
    vnv_vog_tbl
  ) |>
  group_by(flokkur) |>
  fill(vog, .direction = "down") |>
  filter(date >= "2020-01-01") |>
  mutate(
    vog = as.numeric(vog),
    haekkanir = case_when(
      infl < 0 ~ "Verðlækkun",
      infl < 0.025 ~ "0-2,5%",
      infl < 0.05 ~ "2,5-5%",
      infl < 0.075 ~ "5-7,5%",
      infl < 0.1 ~ "7,5%-10%",
      TRUE ~ "Meira en 10%"
    )
  ) |>
  group_by(date) |>
  mutate(vog = vog / sum(vog, na.rm = TRUE)) |>
  group_by(date, haekkanir) |>
  summarise(vaegi = sum(vog, na.rm = TRUE)) |>
  ungroup()

infl_umfang_tbl <- infl_umfang_tbl |>
  mutate(
    haekkanir = factor(
      haekkanir,
      levels = c(
        "Meira en 10%",
        "7,5%-10%",
        "5-7,5%",
        "2,5-5%",
        "0-2,5%",
        "Verðlækkun"
      )
    )
  )


# 13.3.0 Sameina umfang ----
infl_umfang_tbl <- infl_umfang_tbl |>
  bind_rows(infl_umfang_old_tbl)

# 14.0.0 Alþjóðlegur samanburður ----
altjodlegar_upplysingar_tbl <- read_csv("data/altjodlegar.csv")

# x.0.0 Vista ----

list(
  # Verðbólga
  pbi_inflation_verdbolga = vnv_tbl,
  pbi_inflation_verdbolga_valuebox = valuebox_verdbolga,

  # Undirliggjandi
  pbi_inflation_undirliggjandi = undirliggjandi_tbl,
  pbi_inflation_uppruni = edli_og_uppruna_tbl,
  pbi_inflation_froop = froop_final_tbl,

  # Framlag undirliða
  pbi_inflation_waterfall = undirflokkar_latest_tbl,
  phi_inflation_undirflokkar = undirflokkar_12m_tbl,
  phi_inflation_undirflokkar_1m = ahrif_a_visitolu_1m_tbl,
  pbi_inflation_stoplarit_1m = undirflokkar_1m_tbl,

  # HICP
  pbi_inflation_hicp = hicp_pbi_tbl,
  pbi_inflation_hicp_undirflokkar = hicp_undirflokkar_tbl,
  pbi_inflation_hicp_valuebox = hicp_valuebox_tbl,

  # Húsnæðisverð
  pbi_inflation_husnaedisverd = hus_unnid_tbl,
  pbi_inflation_husnaaedisvextir = vextir_tbl,

  # Útlán
  pbi_inflation_utlanastabbi = utlan_stada_tbl,
  pbi_inflation_utlan_ny = ny_utlan_tbl,

  # Gengi og væntingar
  #pbi_inflation_skuldabref = combined_bonds_over_time,
  pbi_inflation_vaentingar = infl_exp_tbl,
  pbi_inflation_vaentingar_skuldabrefamarkadur = infl_exp_breakeven_tbl,
  pbi_inflation_gengi = gengi_tbl,
  pbi_inflation_inngrip = inngrip_si_tbl,

  # Top listinn
  botn_1m = botn_1m_tbl,
  botn_12m = botn_12m_tbl,
  top_1m = top_1m_tbl,
  top_12m = top_12m_tbl,

  # hitakort
  hitakort = infl_umfang_tbl,

  # Alþjóðlegur samanburður
  althjodlegur = altjodlegar_upplysingar_tbl
) |>

  write_rds("data/final_data.rds")
