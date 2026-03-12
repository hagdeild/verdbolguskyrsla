# get hicp

library(httr2)
library(tidyverse)

coicops <- c(
  "TOTAL",
  "TOT_X_NRG",
  "SERV",
  "GD",
  "FOOD"
)

geos <- c("IS", "DK", "NO", "FI", "SE", "EU27_2020")

fetch_hicp_minr <- function(geo) {
  url <- paste0(
    "https://ec.europa.eu/eurostat/api/dissemination/statistics/1.0/data/prc_hicp_minr",
    "?format=JSON",
    "&geo=",
    geo,
    "&unit=I25",
    "&freq=M",
    "&",
    paste0("coicop18=", coicops, collapse = "&")
  )

  resp <- request(url) %>%
    req_perform() %>%
    resp_body_json()

  dim_ids <- resp$id
  sizes <- unlist(resp$size)

  # Build ordered labels for each dimension
  dim_labels <- map(
    dim_ids,
    ~ {
      cats <- resp$dimension[[.x]]$category
      idx <- order(unlist(cats$index))
      names(cats$label)[idx]
    }
  ) %>%
    set_names(dim_ids)

  # Create full grid (row-major order matches Eurostat indexing)
  grid <- expand_grid(!!!map(dim_labels, ~.x))

  # Compute linear index for each row
  grid$idx <- seq_len(nrow(grid)) - 1L

  # Map sparse values by position index
  vals <- rep(NA_real_, nrow(grid))
  for (key in names(resp$value)) {
    pos <- as.integer(key) + 1L
    vals[pos] <- resp$value[[key]]
  }
  grid$value <- vals
  grid$idx <- NULL

  filter(grid, !is.na(value))
}

hicp_raw_tbl <- map(geos, fetch_hicp_minr) %>%
  bind_rows() %>%
  mutate(time = as.Date(paste0(time, "-01")))
