# Finn besta líkanið fyrir verðbólguspá. Bæði langtíma og skammtíma


# SETUP ----
library(tidyverse)

source("R/get_univar_function.R")


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



# Prófa stl-log arimax
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

 vnv_tbl <- vnv_tbl |> 
  left_join(gengi_tbl) |> 
   left_join(laun_tbl) |> 
  mutate(
    outcome = log(outcome),
    visitala = log(visitala),
    visitala = visitala - lag(visitala)
  ) |> 
   drop_na()

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
