# Verðbólguskýrsla VR — inflation monitoring dashboard

A daily inflation-monitoring dashboard for **VR** (Iceland's largest trade union). It collects
official Icelandic and international price and interest-rate data, computes inflation breakdowns
and a short-term inflation forecast, and publishes an interactive 14-page report (the
*verðbólguskýrsla*) to GitHub Pages.

> This project is written in **R** (data pipeline) and **Quarto** (the report). This README
> explains the project's logic, not the language. If you need to learn R,
> [R for Data Science](https://r4ds.hadley.nz/) is the standard reference. You do **not** need
> to be an R expert to keep the dashboard running — you need to understand the data flow below
> and the [gotchas](docs/gotchas.md).

**Further reading:** [docs/pipeline.md](docs/pipeline.md) (full file-by-file walkthrough and data
sources) · [docs/gotchas.md](docs/gotchas.md) (fragile bits, manual steps, maintenance).

---

## 1. Purpose

VR negotiates wages and monitors the cost of living for its members, so it needs a current,
trustworthy view of inflation. This project automates that view. Every day it:

- Pulls the latest Consumer Price Index (CPI / *vísitala neysluverðs*) and its components from
  Statistics Iceland (Hagstofa), plus harmonised inflation (HICP) for Iceland and peer countries
  from Eurostat, exchange rates from the Central Bank, and international policy rates and bond
  yields by scraping.
- Computes the usual analytical cuts: headline vs. core inflation, inflation by origin
  (domestic/imported), component contributions, a price-increase "breadth" heatmap, the housing
  market, lending, inflation expectations, and an international comparison.
- Fits a **short-term (6-month) inflation forecast** and shows it alongside the forecasts of the
  banks and the Central Bank.
- Publishes all of this as an interactive dashboard and feeds a separate VR dashboard portal.

The output is consumed by VR's economists and, via the portal, more broadly within VR.

---

## 2. Quick start

### What you need on the VM (Windows)

- **R 4.5.0** (the version the project is built against).
- **Google Chrome / Chromium** (the international scraper drives a headless browser).
- **Git**, configured with push access to `github.com/hagdeild/verdbolguskyrsla` (the daily job
  pushes automatically).
- The R packages listed in [§6](#6-dependencies--environment). There is **no `renv` lockfile** —
  install them manually (see below).

### First-time setup

1. **Clone the repo** to a working directory.
2. **Fix the hardcoded paths.** Two scripts hardcode an absolute path. Open
   [R/data_preparation.R](R/data_preparation.R) and [R/althjodleg_data.R](R/althjodleg_data.R)
   and change the `setwd("c:/Users/hd/Rwd/verdbolguskyrsla")` line near the top to
   your checkout path. Open [R/schedule.R](R/schedule.R) and fix both the script paths and the
   `rexe` Rscript path. *(This is the #1 thing that breaks on a new machine — see
   [gotchas §1](docs/gotchas.md#1-hardcoded-absolute-paths-breaks-on-any-other-machine).)*
3. **Install R packages** (one-off). In R:
   ```r
   install.packages(c(
     "tidyverse", "eurostat", "XML", "httr", "httr2", "openxlsx", "zoo",
     "rvest", "readxl", "here", "fredr", "modeltime", "tidymodels",
     "hicp", "chromote", "janitor", "TTR", "scales", "plotly", "gt",
     "taskscheduleR", "quarto", "rmarkdown", "knitr"
   ))
   ```
   The `thief` engine used by the forecast comes in via `modeltime`/`tidymodels`.
4. **Install [Quarto CLI](https://quarto.org/docs/get-started/)** (the project is built against
   1.5.57).
5. **Register the scheduled jobs** by sourcing [R/schedule.R](R/schedule.R) once. This creates two
   Windows Task Scheduler tasks (16:00 and 09:01 daily).

### Run it manually (to verify, end to end)

```sh
# 1. International data (writes data/altjodlegar.csv)
Rscript R/althjodleg_data.R

# 2. Core pipeline (writes data/final_data.rds + data/spar.rds, then git-commits & pushes)
Rscript R/data_preparation.R

# 3. Render the dashboard locally (reads the .rds files written above)
quarto render .
```

> ⚠️ Step 2 **commits and pushes to `main`** at the end (that's the design — the push triggers the
> public deploy). If you only want to test without publishing, comment out the `system("git ...")`
> block at the bottom of [R/data_preparation.R](R/data_preparation.R) first.

### Verify it worked

- `data/final_data.rds`, `data/spar.rds`, and `data/altjodlegar.csv` have today's modified time.
- `quarto render .` completes and produces `_site/index.html`; open it in a browser and confirm
  the headline inflation date is current.
- `git log --oneline -3` shows a fresh **"Auto update data"** commit.
- The **Quarto Publish** GitHub Action run is green and the public Pages site shows today's data.

See the [post-run checklist](docs/gotchas.md#9-post-run-checklist-what-to-verify-after-each-day)
for the full daily verification.

---

## 3. Inputs

Every data source, where it lives, and whether it's automated or manual.

### Automated pulls (fetched by the scripts each run)

| Source | What | Access | Owner / where |
|--------|------|--------|---------------|
| **Hagstofa Íslands** (Statistics Iceland) | CPI and all its components, weights, core indices, housing, food | ~18 PXWeb "saved query" CSV URLs (`px.hagstofa.is/pxis/sq/<guid>`) read directly | Statistics Iceland. Full GUID list in [pipeline.md §4](docs/pipeline.md#4-rdata_preparationr--the-core-pipeline). |
| **Eurostat** | HICP (harmonised inflation) for IS, DK, NO, FI, SE, Euro area | JSON API (`prc_hicp_minr`), built by hand with `httr2` | Eurostat |
| **Seðlabanki Íslands** (Central Bank) | EUR, USD, and trade-weighted exchange rates | XML time-series feed (`sedlabanki.is/xmltimeseries`) | Central Bank of Iceland |
| **cbrates.com** | Central-bank policy rates by country | Static HTML scrape | cbrates.com (third-party) |
| **Trading Economics** | CPI inflation and 10-year bond yields by country | **Headless-Chrome scrape** (`chromote`) — fragile | tradingeconomics.com (third-party) |

Frequency: the scripts run **daily**, but the underlying series are mostly **monthly** (CPI is
published by Hagstofa once a month, typically late in the month).

### Manual inputs (you maintain these by hand)

| File | What | Download source |
|------|------|-----------------|
| [data/verdbolguskyrsla_data.xlsx](data/verdbolguskyrsla_data.xlsx) | Policy & bank rates, loan stocks & new loans, FX interventions, **and other analysts' forecasts** (multiple sheets) | See below |
| [data/Verdbolguvaentingar-a-mismunandi-maelikvarda.xlsx](data/Verdbolguvaentingar-a-mismunandi-maelikvarda.xlsx) | Inflation expectations (households, businesses, breakeven) | Central Bank (link below) |
| [data/Vaentingar_markadsadila.xlsx](data/Vaentingar_markadsadila.xlsx) | Market participants' expectations | Central Bank survey |

**Manual download sources** (carried over from the original README — these are where the workbook
data comes from; you copy figures into the sheets):

- **Lending (Útlán):** pension funds <https://gagnabanki.is/report/pension> · new pension-fund
  loans <https://gagnabanki.is/report/pensionloans> · deposit-institution loans
  <https://gagnabanki.is/report/monetary> · government funds <https://gagnabanki.is/report/funds>
- **Policy & bank rates:** <https://sedlabanki.is/gagnatorg/vextir/>
- **CB FX interventions:** <https://gagnabanki.is/report/fxturnover>
- **Inflation expectations:**
  <https://sedlabanki.is/peningastefna/verdbolguvaentingar-a-mismunandi-maelikvarda/>

> ⚠️ The exact per-sheet data-entry procedure (which columns, appending new months, layout rules)
> is **institutional knowledge not captured in code** — and the reads are sensitive to row order.
> See [gotchas §2](docs/gotchas.md#2-the-manual-workbook-dataverdbolguskyrsla_dataxlsx) and
> [open question #1](docs/gotchas.md#open-questions).

---

## 4. Pipeline / structure

The pipeline has **two stages joined by git**. The VM scrapes and computes, commits the results
into `data/`, and pushes; GitHub Actions then renders and deploys the committed data. **The
GitHub render never scrapes anything.**

```
 THE VM (Task Scheduler)                          GITHUB ACTIONS (in Docker)
 ─────────────────────────                        ────────────────────────────
 16:00  althjodleg_data.R ─► data/altjodlegar.csv
                                  │
 09:01  data_preparation.R ◄──────┘
          ├─ pull Hagstofa / Eurostat / CB
          ├─ read verdbolguskyrsla_data.xlsx
          ├─ compute tables + fit forecast
          ├─► data/final_data.rds
          ├─► data/spar.rds
          └─ git commit "Auto update data" + push ──►  publish.yml: quarto render .
                                                          (reads the .rds files)
                                                          └─► deploy _site/ to GitHub Pages
                                                       trigger-deploy.yml ─► portal rebuild
```

| File | One-line role |
|------|---------------|
| [R/althjodleg_data.R](R/althjodleg_data.R) | **Entry point 1** (16:00): scrape international policy rates, inflation, 10y yields → `data/altjodlegar.csv`. |
| [R/data_preparation.R](R/data_preparation.R) | **Entry point 2** (09:01): pull all CPI/HICP/FX data, read the manual workbook, compute every dashboard table, fit the forecast → `data/final_data.rds` + `data/spar.rds`, then auto-commit & push. |
| [index.qmd](index.qmd) | **The dashboard.** Renders the 14-page report from the two `.rds` files. No network, no secrets at render time. |
| [R/schedule.R](R/schedule.R) | One-off: registers the two Task Scheduler jobs. |
| [R/get_univar_function.R](R/get_univar_function.R) | Standalone forecasting engine (ARIMA/ETS/TBATS/THIEF/STL ensemble). **Not used by the daily pipeline** — the live forecast is a simpler inline model. |
| [R/retune_forecast.R](R/retune_forecast.R) | Manual model-selection tool (uses the private `vrmodel` package). |
| [R/depr_verdbolguspa.R](R/depr_verdbolguspa.R) | **Deprecated** older forecast script — do not run (would overwrite `spar.rds`). |
| [R/neuralforecast.R](R/neuralforecast.R), [R/neuralforecast_setup.R](R/neuralforecast_setup.R) | **Experimental** Python/conda deep-learning forecaster. Not in the pipeline. |
| [R/tilfallandi/](R/tilfallandi/) | Dated one-off analyses (oil shocks, VAT, carbon tax). Feed the "Greiningar" page; run by hand. |

For the full step-by-step of what each script does to the data, see
[docs/pipeline.md](docs/pipeline.md).

---

## 5. Outputs

| Output | Where it lands | Who consumes it |
|--------|----------------|-----------------|
| `data/final_data.rds`, `data/spar.rds` | Committed to git | `index.qmd` (and the `tilfallandi` analyses) |
| `data/altjodlegar.csv` | Committed to git | `data_preparation.R` (international section) |
| **The dashboard** (`_site/`) | **GitHub Pages** (deployed by `publish.yml`) | VR economists; the public Pages URL |
| Portal rebuild signal | `repository-dispatch` to `hagdeild/dashboard-portal` | The external VR dashboard portal |

The rendered site (`_site/`) and the Quarto cache (`.quarto/`) are **gitignored** — they are
build artifacts, regenerated on every render. The committed `data/` folder is what actually
travels from the VM to the GitHub render.

---

## 6. Dependencies & environment

### Two separate environments

1. **Your local PC** runs the data pipeline (`*.R`). Needs R 4.5.0, Chrome, Git, and the R
   packages from the [Quick start](#2-quick-start). **Package versions are not pinned anywhere for
   the local environment** — this is a known gap.
2. **GitHub Actions** runs only the render, inside the Docker image
   `ghcr.io/hagdeild/verdbolguskyrsla:latest`, defined by the [Dockerfile](Dockerfile):
   `rocker/tidyverse:4.5.0` + Quarto 1.5.57 + a fixed package list installed with `pak`.

There is **no `renv`** / lockfile. To change the CI environment, edit the `pak::pkg_install(...)`
list in the [Dockerfile](Dockerfile) and push — the **build-image.yml** workflow rebuilds the
image. **If `index.qmd` ever uses a package not in that list, the CI render fails.** See
[gotchas §7](docs/gotchas.md#7-the-ci-render-environment-docker-and-packages).

### Secrets

| Secret | Used by | Needed? |
|--------|---------|---------|
| `GH_PAT` | `trigger-deploy.yml` (poke the portal) | **Yes.** If it expires, the portal stops updating. |
| `GITHUB_TOKEN` | `build-image.yml` (push to GHCR) | Auto-provided by GitHub. |
| `FRED_API_KEY` | set in `publish.yml` | **No — red herring.** `fredr` is loaded but never called anywhere. No FRED key is actually required. See [gotchas §8](docs/gotchas.md#8-the-fred_api_key-red-herring). |

No other API keys: Hagstofa, Eurostat, the Central Bank feed, cbrates, and Trading Economics are
all accessed unauthenticated.

### Scheduled jobs

| Job | Where | When | What |
|-----|-------|------|------|
| `verdbolgu_update_althjodleg` | Windows Task Scheduler (the VM) | 16:00 daily | `R/althjodleg_data.R` |
| `verdb_data_prep` | Windows Task Scheduler (the VM) | 09:01 daily | `R/data_preparation.R` |
| Quarto Publish | GitHub Actions | push to `main` + `1 9 * * *` (09:01 UTC) | render + deploy |

The two Task Scheduler jobs are defined in [R/schedule.R](R/schedule.R). Confirm they are actually
registered/active on the VM with Task Scheduler or `Get-ScheduledTask`.

---

## 7. Gotchas & maintenance

The full list is in **[docs/gotchas.md](docs/gotchas.md)** — read it before touching anything.
The highlights:

- **Scraping happens on the VM, not in CI.** When the public dashboard is wrong, the fault is
  almost always the VM's 09:01/16:00 run. **There is no alerting** — watch the git log for the
  daily "Auto update data" commit.
- **Hardcoded absolute paths** in `data_preparation.R`, `althjodleg_data.R`, and `schedule.R` must
  be changed on a new machine.
- **The manual workbook reads are position-sensitive:** dates are *generated* from hardcoded start
  dates and rows are selected by `slice(<row numbers>)`. **Append new months at the bottom; never
  reorder or leave gaps**, or charts silently shift.
- **Trading Economics scraping is the most fragile piece** (headless Chrome + anti-bot evasion +
  layout assumptions). It has a latent bug where unmapped countries default to `"Noregur"`. Debug
  dumps are written to `data/debug_te_*.html`.
- **Calendar landmines:** the forecast anchor months (`"2026-08-01"`), a temporary VAT adjustment
  gated on `month == April`, and hardcoded prose/dates in `index.qmd` (oil price "15. apríl 2026",
  August-2026 labels, school-meals shading, y-axis caps at 11%) **go stale silently** and need
  manual bumping.
- **The auto git push can wedge the repo** on a rebase/stash conflict; if pushes stop, run
  `git status` and clear any in-progress rebase.

---

## 8. Open questions / unknowns

Things that could not be determined from the repository alone — genuine handover gaps (details in
[gotchas → Open questions](docs/gotchas.md#open-questions)):

1. **The exact manual-update procedure for `verdbolguskyrsla_data.xlsx`** and the expectations
   workbooks (column mapping, how months are appended). The download sources are known; the
   data-entry mechanics are not in code.
2. **Whether the Task Scheduler jobs are currently registered/active** on the VM, and under which
   user account.
3. **What `hagdeild/dashboard-portal` does** with the rebuild signal, and where it's hosted.
4. **How `data/greiningar/` files are refreshed** (oil-price scenarios, historical analyst
   forecasts) — they appear to come from ad-hoc analysis, not the daily pipeline.
5. **Where the private `vrmodel` package lives** and how to install it (only needed to re-tune the
   forecast model, not for the daily dashboard).
