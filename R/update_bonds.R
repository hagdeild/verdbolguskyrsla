# 14.1.0 Stýrivextir ----
# Scrape central bank rates from cbrates.com
scrape_cbrates <- function() {
  page <- read_html("https://www.cbrates.com/")

  # The data is in the 3rd table (1st=header, 2nd=navigation, 3rd=data)
  rows <- page |>
    html_elements("table") |>
    pluck(3) |>
    html_elements("tr")

  # Parse each row, skipping headers/section titles
  df <- map_dfr(rows, function(row) {
    cells <- row |> html_elements("td")

    # Data rows have 6+ cells (flag, rate, change, arrow, country|rate_name, date, ...)
    if (length(cells) < 6) {
      return(NULL)
    }

    cell_texts <- cells |> html_text2() |> str_squish()

    # Cell layout: 1=flag, 2=rate, 3=change, 4=arrow, 5="Country | Rate Name", 6=date
    rate <- cell_texts[2]
    change <- cell_texts[3]
    country_rate <- cell_texts[5]
    date_text <- cell_texts[6]

    # Skip rows that don't look like data (section headers, empty rows)
    if (is.na(rate) | !str_detect(rate, "%")) {
      return(NULL)
    }

    # Country and rate name are combined with "|" separator
    parts <- str_split_fixed(country_rate, "\\|", 2)
    country <- str_squish(parts[1])
    rate_name <- str_squish(parts[2])

    tibble(
      country = country,
      rate_name = rate_name,
      rate = rate,
      change = change,
      date = date_text
    )
  })

  df |>
    filter(!is.na(country), country != "") |>
    mutate(
      # Parse rate as numeric (take midpoint for ranges like "3.50-3.75 %")
      rate_clean = str_remove(rate, "\\s*%\\s*$"),
      rate_numeric = case_when(
        str_detect(rate_clean, "-") ~ {
          parts <- str_split(rate_clean, "-")
          map_dbl(parts, ~ mean(as.numeric(.x)))
        },
        TRUE ~ as.numeric(rate_clean)
      ),
      change_numeric = as.numeric(change),
      date = dmy(date)
    ) |>
    select(country, rate_name, rate, rate_numeric, change, change_numeric, date)
}

# Run it
cb_rates <- scrape_cbrates()

cb_rates <- cb_rates |>
  select(country, rate_numeric) |>
  mutate(rate_numeric = rate_numeric / 100)


# 14.1.1 danmörk ----

# ==============================================================================
# Get Denmark's current-account rate from Danmarks Nationalbank
#
# The HTML table on the page is JavaScript-rendered, but Nationalbanken
# provides XML feeds directly. We use the current-account rate (FOL) feed.
# Fallback: scrape the HTML page with td/th if XML doesn't work.
# ==============================================================================

get_denmark_rate <- function() {
  # --- Try 1: XML feed (most reliable) ---
  xml_url <- "https://www.nationalbanken.dk/interestrates?lang=da&format=xml&typeCodes=FOL"

  rate <- tryCatch(
    {
      resp <- httr::GET(xml_url)
      xml_text_raw <- httr::content(resp, as = "text", encoding = "UTF-8")

      # Extract all rate="X,XX" attribute values with regex
      rate_matches <- str_extract_all(xml_text_raw, '(?<=rate=")[^"]+')[[1]]

      if (length(rate_matches) > 0) {
        # Take the first (most recent) rate, convert comma decimal to dot
        as.numeric(str_replace(rate_matches[1], ",", "."))
      } else {
        NA_real_
      }
    },
    error = function(e) {
      message("XML feed failed: ", e$message)
      NA_real_
    }
  )

  # --- Try 2: Scrape HTML page with td/th ---
  if (is.na(rate)) {
    message("Trying HTML scrape...")

    page <- read_html(
      httr::GET(
        "https://www.nationalbanken.dk/en/what-we-do/stable-prices-monetary-policy-and-the-danish-economy/official-interest-rates",
        httr::user_agent(
          "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36"
        )
      )
    )

    cells <- page |>
      html_elements("td, th") |>
      html_text2() |>
      str_squish()

    # Find "Current-account" or "Foliorent" then grab the next numeric value
    idx <- str_which(
      cells,
      regex("current.account|foliorente", ignore_case = TRUE)
    )[1]

    if (!is.na(idx)) {
      # Look at the next few cells for a number
      remaining <- cells[(idx + 1):min(idx + 5, length(cells))]
      nums <- suppressWarnings(as.numeric(str_replace(remaining, ",", ".")))
      rate <- nums[!is.na(nums)][1]
    }
  }

  # Return as tibble
  tibble(
    country = "Denmark",
    rate_numeric = rate / 100 # Convert from percentage to decimal if needed
  )
}

denmark_rate <- get_denmark_rate()

cb_rates <- cb_rates |>
  bind_rows(denmark_rate)

# 14.2.0 Verðbólga ----

scrape_te_inflation <- function() {
  url <- "https://tradingeconomics.com/country-list/inflation-rate-"

  # Trading Economics blocks default rvest user-agent, so we spoof a browser
  page <- read_html(
    httr::GET(
      url,
      httr::user_agent(
        "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36"
      )
    )
  )

  # Extract the table using td, th selectors
  tbl <- page |>
    html_element("table") |>
    html_table()

  tbl |>
    as_tibble() |>
    janitor::clean_names() |>
    mutate(
      last = as.numeric(last),
      previous = as.numeric(previous)
    )
}

# Your countries of interest (matched to Trading Economics names)
my_countries <- c(
  "Euro Area", # Eurozone (ECB)
  "United Kingdom", # United Kingdom (BoE)
  "United States", # USA (Fed)
  "Canada", # Canada (BoC)
  "Iceland",
  "Poland",
  "Sweden",
  "Switzerland",
  "New Zealand",
  "Norway",
  "Denmark"
)

# Mapping to your cbrates.com names
te_to_cbrates <- tribble(
  ~te_name         , ~country               ,
  "Euro Area"      , "Eurozone (ECB)"       ,
  "United Kingdom" , "United Kingdom (BoE)" ,
  "United States"  , "USA (Fed)"            ,
  "Canada"         , "Canada (BoC)"         ,
  "Iceland"        , "Iceland"              ,
  "Poland"         , "Poland"               ,
  "Sweden"         , "Sweden"               ,
  "Switzerland"    , "Switzerland (SNB)"    ,
  "New Zealand"    , "New Zealand"          ,
  "Norway"         , "Norway"               ,
  "Denmark"        , "Denmark"              ,
)

# Run
all_inflation <- scrape_te_inflation()

althjodleg_verdbolga_tbl <- all_inflation |>
  filter(country %in% my_countries) |>
  left_join(te_to_cbrates, by = c("country" = "te_name")) |>
  select(
    country = country.y,
    cpi_yoy = last,
    previous,
    reference,
    unit
  ) |>
  arrange(desc(cpi_yoy)) |>
  mutate(cpi_yoy = cpi_yoy / 100)


# 14.3.0 10-year bond rates ----

# ==============================================================================
# Scrape 10Y government bond yields from Trading Economics
# The /bonds page has multiple tables: Major10Y, Europe, America, Asia, etc.
# We parse td/th cells directly to avoid html_table() type-guessing issues.
# ==============================================================================

# Bond data is scraped locally with R/update_bonds.R and saved to data/te_bonds.csv
all_bonds <- read_csv("data/te_bonds.csv", show_col_types = FALSE)

# Mapping: Trading Economics name -> your cbrates.com name
te_to_cbrates <- tribble(
  ~te_name         , ~country               ,
  "United States"  , "USA (Fed)"            ,
  "United Kingdom" , "United Kingdom (BoE)" ,
  "Germany"        , "Eurozone (ECB)"       ,
  "Canada"         , "Canada (BoC)"         ,
  "Iceland"        , "Iceland"              ,
  "Poland"         , "Poland"               ,
  "Sweden"         , "Sweden"               ,
  "Switzerland"    , "Switzerland (SNB)"    ,
  "New Zealand"    , "New Zealand"          ,
  "Norway"         , "Norway"               ,
  "Denmark"        , "Denmark"
)


my_bonds_tbl <- all_bonds |>
  inner_join(te_to_cbrates, by = c("country" = "te_name")) |>
  select(country = country.y, te_name = country, yield_10y) |>
  arrange(desc(yield_10y)) |>
  mutate(yield_10y = yield_10y / 100)


# 14.4.0 Sameina ----
altjodlegar_upplysingar_tbl <- cb_rates |>
  left_join(althjodleg_verdbolga_tbl |> select(country, cpi_yoy)) |>
  drop_na() |>
  mutate(real_policy_rate = (1 + rate_numeric) / (1 + cpi_yoy) - 1) |>
  left_join(my_bonds_tbl |> select(-te_name))

# laga nöfn
altjodlegar_upplysingar_tbl <- altjodlegar_upplysingar_tbl |>
  mutate(
    country = case_when(
      country == "Eurozone (ECB)" ~ "Evrusvæðið",
      country == "United Kingdom (BoE)" ~ "Bretland",
      country == "USA (Fed)" ~ "Bandaríkin",
      country == "Canada (BoC)" ~ "Kanada",
      country == "Iceland" ~ "Ísland",
      country == "Poland" ~ "Pólland",
      country == "Sweden" ~ "Svíþjóð",
      country == "Switzerland (SNB)" ~ "Sviss",
      country == "New Zealand" ~ "Nýja Sjáland",
      country == "Denmark" ~ "Danmörk",
      TRUE ~ "Noregur"
    )
  )


altjodlegar_upplysingar_tbl |>
  write_csv("data/altjodlegar.csv")
