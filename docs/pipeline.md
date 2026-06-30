# Pipeline & repository structure

This document walks through every file that matters, the order things run in, and where
data comes from and goes. For the fragile bits and maintenance procedures, see
[gotchas.md](gotchas.md). For the high-level picture, start with the root
[README](../README.md).

> The project is written in **R**. This document explains *what each step does to the
> data*, not the R itself. If you need to come up to speed on R, [R for Data Science](https://r4ds.hadley.nz/) is the standard reference.

---

## 1. The big picture: the VM scrapes, GitHub renders

There are two stages, and they hand off through the git repository — specifically the committed
files under `data/`. The first stage runs on **the VM** (a Windows virtual machine running the
scheduled scripts); the second runs on **GitHub Actions** (a hosted service) and only ever reads
what the VM committed.

```
  ┌─────────────────────────────────────────────────────────────────┐
  │  STAGE 1 — the VM (Windows Task Scheduler)                        │
  │                                                                   │
  │  16:00  R/althjodleg_data.R                                       │
  │           scrape cbrates.com + Trading Economics (headless Chrome)│
  │           → writes data/altjodlegar.csv                           │
  │                                                                   │
  │  09:01  R/data_preparation.R                                      │
  │           pull ~18 Hagstofa queries + Eurostat HICP + CB FX feed  │
  │           read data/verdbolguskyrsla_data.xlsx (manual workbook)  │
  │           read data/altjodlegar.csv (from the 16:00 job)          │
  │           compute all tables + fit the 6-month forecast           │
  │           → writes data/final_data.rds  and  data/spar.rds        │
  │           → git add . && git commit && git push origin main  ◀────┼── auto
  └───────────────────────────────┬───────────────────────────────────┘
                                   │ push to main
                                   ▼
  ┌─────────────────────────────────────────────────────────────────┐
  │  STAGE 2 — GitHub Actions (in a Docker container)                 │
  │                                                                   │
  │  publish.yml          render index.qmd against the committed      │
  │                       .rds files (NO network data pull here)      │
  │                       → deploy _site/ to GitHub Pages             │
  │                                                                   │
  │  trigger-deploy.yml   send a repository-dispatch to               │
  │                       hagdeild/dashboard-portal to rebuild        │
  │                                                                   │
  │  build-image.yml      (only when Dockerfile changes) rebuild the  │
  │                       ghcr.io/hagdeild/verdbolguskyrsla:latest img │
  └─────────────────────────────────────────────────────────────────┘
```

**The single most important consequence:** the GitHub render reads only what was committed.
It does **not** scrape anything. All the fragile, network-dependent work happens on the VM.
If a scrape breaks, the dashboard does not error — it silently re-publishes the last good
data. (See [gotchas.md](gotchas.md).)

`publish.yml` is *also* on its own daily `cron` (`1 9 * * *`, 09:01 UTC) as a backstop, so
the site re-renders even on a day the local push didn't happen — again, against whatever
data is currently committed.

---

## 2. Run order / entry points

There are **two entry points**, both scheduled on the VM. Everything else is either a helper,
a dormant/experimental script, or a one-off analysis.

| Order | File | Trigger | Role |
|-------|------|---------|------|
| 1 | `R/althjodleg_data.R` | Task Scheduler on the VM, **16:00** daily | Produces `data/altjodlegar.csv` (international comparison). Must run *before* the 09:01 job uses it. |
| 2 | `R/data_preparation.R` | Task Scheduler on the VM, **09:01** daily | The core. Produces `data/final_data.rds` + `data/spar.rds`, then commits & pushes. |
| 3 | `index.qmd` | GitHub Action (on push + 09:01 UTC cron) | Renders the dashboard from the committed `.rds` files. Run via `quarto render .`. |

`R/schedule.R` is what *registers* jobs 1 and 2 with Windows Task Scheduler (using the
`taskscheduleR` package). You run it once on the VM to set the schedule up; it is not part of the
daily flow. Note it only registers `althjodleg_data.R` and `data_preparation.R` — the times in
that script (16:00 and 09:01) are the source of truth for the VM's schedule.

---

## 3. Directory tree (annotated)

```
verdbolguskyrsla/
├── index.qmd                  # THE DASHBOARD. Quarto dashboard, 14 pages. Reads data/*.rds only.
├── _quarto.yml                # Quarto project config: type=website, output-dir=_site
├── styles.css                 # Dashboard CSS (theme tweaks)
├── vr_logo.jpg                # Logo used in the dashboard
├── Dockerfile                 # Defines the CI render environment (R 4.5.0 + Quarto + packages)
│
├── R/
│   ├── data_preparation.R     # ENTRY POINT 2. Core daily pipeline (2081 lines). See §4.
│   ├── althjodleg_data.R      # ENTRY POINT 1. International scraping. See §5.
│   ├── schedule.R             # One-off: registers the two Task Scheduler jobs.
│   │
│   ├── get_univar_function.R  # Standalone univariate forecasting engine (ensemble of
│   │                          #   ARIMA/ETS/TBATS/THIEF/STL). NOT used by the daily pipeline.
│   ├── retune_forecast.R      # Manual tool: runs get_univar_function.R to pick the best model.
│   │                          #   Uses the private `vrmodel` package. Run by hand, ad hoc.
│   │
│   ├── depr_verdbolguspa.R    # DEPRECATED ("depr"). Older forecast script, superseded by the
│   │                          #   forecast section inside data_preparation.R. Not run.
│   ├── neuralforecast.R       # EXPERIMENTAL. Python/reticulate deep-learning forecaster
│   ├── neuralforecast_setup.R #   (NHITS/NBEATSx via a conda env). NOT in the pipeline. See §7.
│   ├── scrape_trading_econoimcs.R  # Scratch/throwaway chromote scraping snippet (sic: typo in name).
│   │
│   └── tilfallandi/           # "Ad hoc" — dated one-off analyses (oil shocks, VAT, carbon tax).
│                              #   Each reads data/spar.rds + bespoke CSVs. Not scheduled. See §8.
│
├── data/                      # COMMITTED to git. This is the hand-off between Half A and Half B.
│   ├── verdbolguskyrsla_data.xlsx   # MANUAL workbook. Updated by hand. The main manual input. See gotchas.md.
│   ├── Verdbolguvaentingar-a-mismunandi-maelikvarda.xlsx  # Inflation expectations (manual download).
│   ├── Vaentingar_markadsadila.xlsx # Market participants' expectations (manual download).
│   ├── altjodlegar.csv        # OUTPUT of althjodleg_data.R (international rates/inflation/yields).
│   ├── lond.csv               # Country ISO-3 / Icelandic-name lookup (static reference).
│   ├── final_data.rds         # OUTPUT of data_preparation.R. ~30 tables the dashboard reads.
│   ├── spar.rds               # OUTPUT of data_preparation.R. Forecast tables.
│   ├── debug_te_inflation.html# Debug dump written every run by althjodleg_data.R (overwritten).
│   ├── debug_te_bonds.html    # Debug dump written every run by althjodleg_data.R (overwritten).
│   ├── timeseries_byday_*.csv # Legacy daily series (not read by the live pipeline).
│   └── greiningar/            # Inputs for the "Greiningar" analysis page + tilfallandi scripts.
│       ├── oliuverd.csv                 # Oil price history (used by index.qmd).
│       ├── oliuverd_scenarios_varx.csv  # Oil-shock scenario IRF (used by index.qmd).
│       ├── oliuverd_scenarios*.csv      # Other scenario variants (used by tilfallandi).
│       └── greiningaradilar_sogulegar_spar.xlsx  # Historical forecasts of other analysts.
│
├── data-other/                # Reference PDFs only (COICOP code mappings). Not read by code.
│   ├── coicop_mappings.pdf
│   └── new_coicop.pdf
│
├── output/                    # Stray exported chart (output/fc.png). Not part of the pipeline.
│
├── .github/workflows/
│   ├── publish.yml            # Render index.qmd in the container, deploy to Pages. See §6.
│   ├── trigger-deploy.yml     # Notify hagdeild/dashboard-portal to rebuild. See §6.
│   └── build-image.yml        # Rebuild the Docker image when the Dockerfile changes.
│
├── _site/                     # Quarto render output (gitignored).
└── .quarto/                   # Quarto cache, incl. _freeze (gitignored).
```

---

## 4. `R/data_preparation.R` — the core pipeline

A single linear script (no functions exported) that runs top to bottom. It pulls everything,
computes everything, and ends by saving two `.rds` files and pushing to git.

**Packages loaded:** `tidyverse`, `eurostat`, `XML`, `httr`, `openxlsx`, `zoo`, `rvest`,
`readxl`, `here`, `fredr`, `modeltime`, `tidymodels`, `httr2`, `hicp`. (Note: `eurostat`,
`rvest`, `here`, and `fredr` are loaded but **never actually called** — Eurostat is hit via
raw `httr2`, there is no `rvest` scrape here, no `here()`, and no FRED call. See
[gotchas.md](gotchas.md) on the `FRED_API_KEY` red herring.)

### What it does, in order (each step = intent, not syntax)

1. **Setup.** Sets the working directory to the hardcoded project path, loads the country
   lookup, defines the 5-year display window (`date_from`) and the old/new COICOP changeover
   date (`new_coicop_date_from <- "2026-01-01"`).
2. **Headline CPI (with & without housing).** Reads CPI from Hagstofa; computes 12-month and
   1-month inflation and a 12-month-average "speed" measure for the headline value box.
3. **Inflation by nature & origin.** Reads the *old* and *new* COICOP "nature & origin"
   sub-indices and weights, aggregates to custom groups (domestic vs imported goods,
   services, housing), and splices old-before / new-after the changeover date.
4. **Core ("underlying") inflation.** Reads old and new core indices (Kjarnavísitala 1/2/4/5),
   computes 12-month changes, splices.
5. **Component contributions (waterfall).** For the latest 3 months, computes each
   sub-component's contribution to inflation (`contribution = inflation × weight`); also pulls
   Hagstofa's ready-made 1-month contributions.
6. **Sub-category bar charts.** Latest 12-month and 1-month change for every 2-digit category.
7. **FROOPP** (frequently- vs infrequently-purchased goods). Uses the `hicp` package to get the
   COICOP code set, splits the basket, computes inflation for each part, splices old + new.
8. **Eurostat HICP** (harmonised inflation). Fetches HICP for Iceland + Nordics + Euro area via
   the Eurostat JSON API, computes 12-month and 3-month inflation for the international comparison.
9. **Housing prices.** Reads old & new housing sub-indices, computes 12-month changes, splices.
10. **Interest rates.** Reads bank lending rates and the policy rate from the manual workbook;
    monthly-averages the policy rate.
11. **Lending (útlán).** Reads loan stocks and new loans (banks, pension funds, government
    funds) from the workbook; computes indexed vs non-indexed shares.
12. **Exchange rate (gengi).** Pulls EUR, USD, and the trade-weighted index from the Central
    Bank XML feed; monthly-averages.
13. **FX interventions.** Reads CB market-intervention data from the workbook; monthly buy/sell
    totals and buy-share.
14. **Inflation expectations.** Reads households, businesses, market participants, and
    bond-market breakeven expectations from the two expectations workbooks; combines them.
15. **Top/bottom 10.** Reads the full detailed CPI, detects "leaf" categories, computes 1m/12m
    changes, takes the 10 largest rises and falls.
16. **Price-increase breadth (heatmap).** Buckets each category's 12-month inflation into bands,
    weighted by its basket weight; splices old + new.
17. **International comparison + food.** Reads `data/altjodlegar.csv` (from the 16:00 job); reads
    food prices by 3-digit category and by origin from Hagstofa.
18. **Save `data/final_data.rds`** — a named list of ~30 tables (the names are all `pbi_*`/`phi_*`,
    a leftover Power BI naming convention; they are what `index.qmd` reads).
19. **Forecast (section "16.0.0 SPÁR").** Reads CPI back to 2003, fits a **THIEF temporal-hierarchy
    model with an ARIMA base** (`temporal_hierarchy(use_model="arima") |> set_engine("thief")`),
    forecasts 6 months, derives the 1m/6m/12m headline forecast figures, appends other analysts'
    and the Central Bank's forecasts (from the `spar` sheet of the workbook), optionally applies a
    temporary VAT adjustment, and saves **`data/spar.rds`**.
20. **Git push.** Runs `git stash → git pull --rebase origin main → git stash pop → git add . →
    git commit -m "Auto update data" → git push origin main` via `system()`. This is why the git
    history is a wall of "Auto update data" commits, and it is what triggers Half B.

### The forecast model (important)

The **live** forecast that appears on the dashboard is the inline THIEF/ARIMA model in step 19 —
**not** the elaborate ensemble engine in `R/get_univar_function.R`. The forecast depends on the
`thief` engine being installed (via `modeltime`/`tidymodels`); there is no fallback model.

### Outputs

| File | Contents | Consumed by |
|------|----------|-------------|
| `data/final_data.rds` | List of ~30 tables (CPI breakdowns, housing, rates, lending, FX, expectations, HICP, heatmap, top/bottom 10, international) | `index.qmd` |
| `data/spar.rds` | List: `verdbolguspa` (forecast series incl. other analysts) + `verdbolguspa_valuebox` (formatted 1m/6m/12m figures) | `index.qmd`, all `R/tilfallandi/*` scripts |

---

## 5. `R/althjodleg_data.R` — international comparison scraper

Runs at 16:00 so its output is fresh when the 09:01 job reads it.

**Packages:** `tidyverse`, `rvest`, `chromote` (plus `httr::` and `janitor::` via `::`).

### What it does, in order

1. **Policy rates** — scrapes `https://www.cbrates.com/` (static `rvest` read; takes the 3rd
   table) for central-bank key rates across many countries; converts to decimals and takes the
   midpoint of any quoted range.
2. **Denmark's policy rate** — fetched separately from Nationalbanken's XML feed (with an
   HTML-scrape fallback) and appended.
3. **CPI inflation by country** — scrapes Trading Economics
   (`https://tradingeconomics.com/country-list/inflation-rate-`) using **headless Chrome** via
   `chromote`, filtered to a hardcoded 11-country list.
4. **10-year government bond yields** — scrapes `https://tradingeconomics.com/bonds`, reusing the
   same Chrome session.
5. **Combine & compute** — joins policy rate + inflation + 10y yield, drops incomplete rows, and
   computes the **real policy rate** via the Fisher relation `(1 + rate)/(1 + cpi) − 1`.
6. **Translate to Icelandic** country names and **write `data/altjodlegar.csv`**.

### Outputs

| File | Contents |
|------|----------|
| `data/altjodlegar.csv` | One row per country: `country`, `rate_numeric` (policy rate), `cpi_yoy`, `real_policy_rate`, `yield_10y` |
| `data/debug_te_inflation.html` | Debug dump of the rendered TE inflation page, written every run |
| `data/debug_te_bonds.html` | Debug dump of the rendered TE bonds page, written every run |

This is the most fragile script in the project (browser automation + layout-dependent
scraping + anti-bot evasion). See [gotchas.md](gotchas.md) for the specific failure modes,
including the `TRUE ~ "Noregur"` catch-all bug.

---

## 6. `index.qmd` — the dashboard

A single Quarto **dashboard** (`format: dashboard`, theme `flatly`, `styles.css`). It is a pure
presentation layer.

- **Reads only:** `data/final_data.rds` and `data/spar.rds` (lines 18–19), plus
  `data/greiningar/oliuverd.csv` and `data/greiningar/oliuverd_scenarios_varx.csv` for the
  analysis page. **No network calls, no API keys, no `source()` at render time.**
- **Packages:** `tidyverse`, `scales`, `plotly`, `gt`.
- **14 pages** (Quarto turns each `#` header into a page):
  1. Verðbólga (headline inflation) · 2. Undirliggjandi verðbólga (core) · 3. Framlag undirliða 12m
  (12-month contributions) · 4. Framlag undirliða 1m · 5. Hitakort (heatmap) · 6. Undirflokkar
  (sub-categories) · 7. Top 10 listinn · 8. Samræmd vísitala (HICP) · 9. Samræmd vísitala –
  Undirflokkar · 10. Alþjóðlegur samanburður (international) · 11. Húsnæðismarkaðurinn (housing) ·
  12. Væntingar og gjaldeyrismarkaður (expectations & FX) · 13. Verðbólguspá (forecast) ·
  14. Greiningar (scenario analyses).
- **Outputs:** mostly interactive `plotly` charts (via a custom `vr_layout()` helper), several
  value boxes, static `waterfall_plot()` charts, one `gt` table (international rates), and a
  plotly subplot (FX intervention).

Because it reads committed `.rds` files with no fallback, **rendering fails if those files are
missing**, and silently uses stale data if they are out of date. See [gotchas.md](gotchas.md)
for the hardcoded dates and prose inside this file that go stale.

---

## 7. The GitHub Actions

| Workflow | Trigger | What it does |
|----------|---------|--------------|
| `publish.yml` | push to `main`, manual dispatch, **cron `1 9 * * *`** | Runs `quarto render .` inside the container `ghcr.io/hagdeild/verdbolguskyrsla:latest`, uploads `_site/` as a Pages artifact, deploys to GitHub Pages. Sets `FRED_API_KEY` from secrets (unused — see gotchas). |
| `trigger-deploy.yml` | push to `main` | Sends a `repository-dispatch` (`dashboard-updated`) to `hagdeild/dashboard-portal` using the `GH_PAT` secret, so the external portal rebuilds. |
| `build-image.yml` | push that changes `Dockerfile`, manual dispatch | Rebuilds and pushes the Docker render image to GHCR. |

The render container is defined by the [Dockerfile](../Dockerfile): `rocker/tidyverse:4.5.0`
base, Quarto CLI `1.5.57`, and a fixed list of R packages installed via `pak`. **Any package
`index.qmd` needs at render time must be in that list**, or the CI render fails. See
[gotchas.md](gotchas.md).

---

## 8. Dormant, experimental, and ad-hoc code

None of the following is part of the daily pipeline. They are kept for reference / manual use.

- **`R/get_univar_function.R`** — a self-contained univariate forecasting engine: fits ARIMA,
  ETS, TBATS, THIEF, and STL across four transformations (level/log/1m-roc/12m-roc), runs
  time-series cross-validation, and builds shrinkage-weighted ensembles. It is **not** sourced by
  the pipeline; the live forecast is the simpler inline THIEF model in `data_preparation.R`.
- **`R/retune_forecast.R`** — a short manual script that calls `get_univariate_forecasts()` to
  re-evaluate which model is best. Uses the **private `vrmodel` package** (`library(vrmodel)`),
  which is not on CRAN and not pinned anywhere — you would need access to it to run this.
- **`R/depr_verdbolguspa.R`** — the previous forecast script (the `depr` prefix means
  *deprecated*). Superseded by the forecast section inside `data_preparation.R`. It still
  contains a `write_rds("data/spar.rds")`, so **do not run it** or it will overwrite the live
  forecast with stale logic.
- **`R/neuralforecast.R` + `R/neuralforecast_setup.R`** — an experimental deep-learning
  forecaster (NHITS / NBEATSx / PatchTST / KAN) that calls Python via `reticulate` and a conda
  environment named `r-neuralforecast`. Requires conda + Python + the `neuralforecast` package.
  Not wired into anything that runs.
- **`R/scrape_trading_econoimcs.R`** — a throwaway snippet for experimenting with the TE scrape
  (note the typo in the filename). Not used.
- **`R/tilfallandi/`** — dated, single-use analyses (oil-price scenarios, a 24%→11% petrol VAT
  change, carbon-tax effects, a forecast comparison). Each reads `data/spar.rds` and/or files in
  `data/greiningar/`. They feed one-off reports and the figures that get hardcoded into the
  "Greiningar" page of `index.qmd`. Run by hand when needed; safe to ignore for keeping the
  dashboard alive.
