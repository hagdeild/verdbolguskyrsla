library(tidyverse)
library(quantmod)


brent_yahoo <- getSymbols(
  "BZ=F",
  src = "yahoo",
  from = as.Date("2020-01-01"),
  to = Sys.Date() + 1,
  auto.assign = FALSE
) |>
  as_tibble(rownames = "date") |>
  transmute(
    date = as.Date(date),
    price = `BZ=F.Close`
  ) |>
  filter(!is.na(price))

brent_yahoo |>
  write_csv("data/greiningar/oliuverd.csv")
