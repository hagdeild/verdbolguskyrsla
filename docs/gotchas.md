# Gotchas & maintenance

The fragile bits, the manual steps, the things that go stale on a calendar, and how to fix the
common breakages. Read this before you touch anything. For how the pipeline fits together, see
[pipeline.md](pipeline.md).

Line numbers below refer to the files as of this writing; treat them as "near here", not exact
forever.

---

## 0. The mental model that prevents most confusion

- **Scraping happens on the VM, not in GitHub.** When the public dashboard looks wrong, the
  problem is almost always in the VM's 09:01 / 16:00 Task Scheduler runs, not in the GitHub
  Action. The Action only renders committed data.
- **A broken scrape does not raise an alarm.** If `data_preparation.R` errors partway, the git
  push at the end may not run, so the site just keeps showing yesterday's data. There is **no
  alerting**. The way you notice is: the dashboard date stops advancing, or the git log stops
  getting "Auto update data" commits. **Check the git history daily-ish.**
- **The committed `data/` folder is the contract.** If you ever need to publish without running
  the local job, you can hand-edit/replace the `.rds` files, commit, and push — the render will
  pick them up.

---

## 1. Hardcoded absolute paths (breaks on any other machine)

Both entry-point scripts start by setting the working directory to the repo's specific path on
the VM:

```r
setwd("c:/Users/hd/Rwd/verdbolguskyrsla")   # data_preparation.R line ~18
setwd("c:/Users/hd/Rwd/verdbolguskyrsla")   # althjodleg_data.R   line ~8
```

`R/schedule.R` also hardcodes this path and the R executable path
(`C:/PROGRA~1/R/R-4.5.0/bin/x64/Rscript.exe`).

**If the repo ever moves** (different user account, re-provisioned VM, or you relocate the
checkout): change these to the new path (and the Rscript path in `schedule.R`), then re-run
`schedule.R` to re-register the Task Scheduler jobs. There is no config file — these are edited
in the scripts directly.

---

## 2. The manual workbook: `data/verdbolguskyrsla_data.xlsx`

This is the **main human-maintained input**. `data_preparation.R` reads many sheets from it.
Nothing in the repo updates it — **you update it by hand** before (or independent of) the daily
run, and the daily run picks up whatever is in it.

Sheets the pipeline reads (sheet name → meaning):

| Sheet | Meaning |
|-------|---------|
| `vextir` | Bank lending interest rates |
| `meginvextir` | Central Bank policy rate |
| `bank_stabbi`, `lif_stabbi`, `lanasjodir_stabbi` | Loan **stocks** — banks, pension funds, government funds |
| `bank_ny_utlan`, `lif_ny_utlan` | **New** loans — banks, pension funds |
| `inngrip_si` | Central Bank FX-market interventions |
| `spar` | Other analysts' (banks') and the Central Bank's inflation **forecasts** |

The two other manual workbooks:

- `data/Verdbolguvaentingar-a-mismunandi-maelikvarda.xlsx` — inflation expectations (households,
  businesses, breakeven rates).
- `data/Vaentingar_markadsadila.xlsx` — market participants' expectations.

**Where the manual data comes from** (from the original README — these are the human download
sources; cadence is whatever the source publishes, typically monthly/quarterly):

- **Lending (Útlán):**
  - <https://gagnabanki.is/report/pension> — pension-fund loans
  - <https://gagnabanki.is/report/pensionloans> — new pension-fund loans
  - <https://gagnabanki.is/report/monetary> — deposit-institution loans & new loans
  - <https://gagnabanki.is/report/funds> — government-fund loans
- **Policy & bank rates (Meginvextir / Vextir bankanna):** <https://sedlabanki.is/gagnatorg/vextir/>
- **CB FX interventions (Inngrip SÍ á gjaldeyrismarkað):** <https://gagnabanki.is/report/fxturnover>
- **Inflation expectations (Verðbólguvæntingar):**
  <https://sedlabanki.is/peningastefna/verdbolguvaentingar-a-mismunandi-maelikvarda/>

> ⚠️ **Open question for the successor:** the exact per-sheet update procedure (which columns,
> which rows, what format) is not written down anywhere in code. The download URLs above are
> known; the data-entry mechanics are institutional knowledge. **See [open question #1](#open-questions).**

### The brittle part: row/position-based reads

Several workbook reads are tied to **exact spreadsheet layout**, not to labels:

- **Dates are *generated*, not read.** For the lending tables, the code creates the date column
  with `seq.Date(from = <hardcoded start>, length.out = nrow(.))` rather than reading dates from
  the sheet. The hardcoded starts are: `1997-12-01` (bank_stabbi), `1997-01-01` (lif_stabbi),
  `1992-03-01` (lanasjodir_stabbi), `2009-01-01` (lif_ny_utlan), `2013-01-01` (bank_ny_utlan).
  **If you insert or delete a row, or a month is missing, every date silently shifts** and the
  charts will be wrong without any error. When adding a new month, **append at the bottom and do
  not leave gaps.**
- **Hardcoded `slice()` row numbers** select specific rows: e.g. `slice(6, 9)`, `slice(16, 19)`,
  `slice(72, 73, 110, 111)`, `slice(9)`, `slice(4:7)`. If the source sheets are re-laid-out (rows
  reordered, header rows added), these grab the wrong rows. **Keep the sheet layout stable.**

---

## 3. Trading Economics scraping (the most fragile script)

`R/althjodleg_data.R` scrapes Trading Economics with **headless Chrome** (`chromote`) plus
anti-bot evasion (spoofed user-agent claiming Chrome 131, masked `navigator.webdriver`, faked
plugins/languages). This means:

- **Chrome/Chromium must be installed** on the VM (it runs the 16:00 job). If Chrome updates
  in a way `chromote` can't drive, the inflation + bonds halves break.
- **It waits on timers, not signals.** `scrape_te_page` polls up to 15× with `Sys.sleep(2)` for a
  table to appear, then falls through to a `Sys.sleep(5)` and proceeds **anyway** — so on a slow
  load it may parse an empty/partial page instead of erroring.
- **Layout-dependent parsing.** cbrates.com assumes the data is the **3rd table** and a fixed
  column order; TE inflation assumes the **first table**; TE bonds uses a "first non-numeric cell
  = country, first numeric = yield" heuristic plus a hardcoded skip-list of section headers
  (`"Major10Y", "Europe", "America", "Asia", "Africa"`). Any layout change on these sites
  silently corrupts or empties the result.
- **Latent bug — the `"Noregur"` catch-all.** The final Icelandic-name mapping ends with
  `TRUE ~ "Noregur"` (Norway) as the default. **Any country that isn't explicitly listed silently
  becomes "Noregur."** If TE's country naming drifts, you can get a wrong/duplicate Norway row
  rather than an error.
- **Two different Euro-area proxies.** The inflation map uses `"Euro Area"`; the bonds map uses
  `"Germany"` (the eurozone 10-year is the German Bund). Easy to misread as a typo — it's
  intentional.

**Debug aid:** every run writes `data/debug_te_inflation.html` and `data/debug_te_bonds.html` —
the actual rendered pages. When a scrape produces garbage, open these to see what TE actually
served.

**If TE breaks and you need to publish anyway:** `data/altjodlegar.csv` is a plain CSV with
columns `country, rate_numeric, cpi_yoy, real_policy_rate, yield_10y`. You can edit it by hand,
commit, and the international page will use your values.

---

## 4. Other scraped/remote sources that can drift

- **Hagstofa (Statistics Iceland) saved queries.** ~18 endpoints of the form
  `https://px.hagstofa.is:443/pxis/sq/<guid>`. Each GUID is a *saved query* (table + selection)
  created in Hagstofa's PXWeb. If a query is deleted/changed upstream, or Hagstofa rebases an
  index, the corresponding section breaks or shifts. The full GUID→meaning table is in
  [pipeline.md §4](pipeline.md#4-rdata_preparationr--the-core-pipeline). (A recent commit,
  "Fix Hagstofa URL", was exactly this kind of breakage.)
- **NA-string inconsistency.** Different Hagstofa reads pass different missing-value markers:
  most use `na = "."`, one uses `na = ".."`, several pass nothing. If Hagstofa changes its
  missing-value marker, the reads that don't specify it will silently mis-parse.
- **Eurostat HICP** is fetched by **manually building a JSON-stat URL** (`prc_hicp_minr`,
  `unit=I25` = index 2025=100) and reconstructing the sparse value array by integer position.
  This relies on Eurostat's row-major ordering and on `unit=I25` staying valid. There's already a
  defensive `if (class(...) == "Date")` guard because the time column sometimes won't parse.
- **Central Bank FX feed** is plain HTTP (`http://www.sedlabanki.is/xmltimeseries/...`) with
  hardcoded `TimeSeriesID`s (`eur=4064`, `usd=4055`, trade-weighted index `=4117`). If the Bank
  changes IDs or the XML structure, `get_si_gengi` breaks. Dates are parsed as US month-day-year.

---

## 5. Calendar/seasonal landmines (things you must bump by hand)

These are hardcoded to specific dates and **will silently go stale** — no error, just wrong or
frozen output.

### In `data_preparation.R`

- **Old/new COICOP splice:** `new_coicop_date_from <- "2026-01-01"` (line ~35). Hagstofa revised
  its COICOP classification; the code reads *both* old and new tables and stitches them at this
  date. This is a one-time changeover and should normally stay put, but it's the reason there are
  paired "old/new" reads everywhere.
- **Annual basket-weight revision shift.** A comment notes "desember 2025 á við 2026" — the
  December weight is shifted forward one month and forward-filled, with a hardcoded grid start
  `as.Date("2026-01-01")`. When Hagstofa publishes new annual weights (each January), this needs
  revisiting.
- **Forecast anchor months are literal dates.** The 6m/12m forecast figures are computed at
  `"2026-02-01"` and `"2026-08-01"` (not relative to `today()`). **These must be bumped each
  cycle** or the headline forecast numbers freeze on August 2026.
- **Temporary VAT adjustment (currently inactive).** `bein_ahrif <- 0.003742742` (the modelled
  direct effect of a 24%→11% petrol VAT change) is subtracted from the **VR** forecast over the
  hardcoded range `2026-05-01`→`2026-08-01`, **but only when `month(today()) == 4`** (April).
  Outside April the whole block does nothing except print a stray half-finished string. This is a
  hand-tuned, time-boxed hack; delete or replace it once the VAT episode is over. (As of mid-2026
  it is dormant.)

### In `index.qmd`

The dashboard has hardcoded dates and **prose with embedded numbers** that won't update with the
data. These are cosmetic-but-visible and need manual editing:

- The oil-price narrative text states brent crude "var um 71 dollari en er nú, **15. apríl 2026**,
  í um 98 dollurum ... tæp 40%" — a fixed date, fixed prices, fixed percentage.
- Value-box captions for collective-bargaining assumptions: `"Forsenduákvæði kjarasamninga: 4.7%"`
  / `4.4%` and a `"Verðbólguspá fyrir ágúst 2026"` label.
- A shaded "school meals" annotation region pinned to `2024-08-01`→`2025-09-01`, and reference
  `geom_vline`s at fixed dates.
- The VAT-and-oil analysis chunk uses bare constants (`bensin_vog <- 249/10000`,
  `diesel_vog <- 108/10000`, `avg_laekkun <- abs(1.11/1.24 - 1)`) whose original derivation is
  commented out just above — i.e. the live number is a hand-tuned replacement with no provenance
  in code.
- Several charts have **hardcoded y-axis caps** (`limits = c(0, 0.11)`, `ylim = c(NA, 0.4)`).
  Inflation above those caps would be **clipped off the chart** without warning.

> Practical rule: when the reporting month rolls over a half-year boundary, grep `index.qmd` and
> the forecast section of `data_preparation.R` for the previous anchor date (e.g. `2026-08`) and
> update every hit.

---

## 6. The auto git push can leave the repo wedged

`data_preparation.R` ends with `git stash → git pull --rebase → git stash pop → git add . →
git commit → git push` run via `system()`. If the rebase hits a conflict, or `stash pop`
conflicts, the script can leave the working tree **mid-operation**, and the next scheduled run
starts from a dirty state. If pushes stop appearing:

1. Open a terminal in the repo and run `git status` — look for an in-progress rebase or stash
   conflict.
2. Resolve/abort (`git rebase --abort`, then sort out the stash) and get back to a clean `main`.
3. Re-run the job (or just wait for the next scheduled run once the tree is clean).

Because `git add .` stages **everything**, the committed `data/` includes the debug HTML dumps
and any stray files — that's expected, not a mistake.

---

## 7. The CI render environment (Docker) and packages

The GitHub render runs in `ghcr.io/hagdeild/verdbolguskyrsla:latest`, built from the
[Dockerfile](../Dockerfile): `rocker/tidyverse:4.5.0` + Quarto `1.5.57` + a fixed package list
installed with `pak`.

- **There is no `renv`/lockfile.** Package versions are pinned only by (a) the rocker base image
  tag and (b) whatever `pak` resolves at image-build time. The Docker package list and the local
  PC's installed packages are **two separate, unsynchronised** dependency sets.
- **If `index.qmd` starts using a new package**, you must add it to the `pak::pkg_install(...)`
  list in the Dockerfile **and** push (which triggers `build-image.yml` to rebuild the image),
  or the CI render will fail with a "there is no package called …" error.
- The Dockerfile's package list is broader than what the *render* strictly needs (it includes
  modeling packages used by the local pipeline). That's fine; it just makes the image bigger.

To rebuild the image manually: trigger the **Build R/Quarto Image** workflow (workflow_dispatch),
or push any change to `Dockerfile`.

---

## 8. The `FRED_API_KEY` red herring

`publish.yml` sets `FRED_API_KEY` from repository secrets, and several scripts `library(fredr)`.
**But `fredr` is never actually called anywhere in the codebase** (verified by search). No FRED
data is used. The secret and the `library(fredr)` lines are leftovers. You do **not** need a FRED
key for anything to work. (Left documented so you don't go hunting for where it's "used.")

The secret you *do* need is **`GH_PAT`** — a GitHub personal access token used by
`trigger-deploy.yml` to poke the `hagdeild/dashboard-portal` repo. If the portal stops updating,
check that this token hasn't expired.

---

## 9. Post-run checklist (what to verify after each day)

1. **Did it push?** `git log --oneline -3` should show a fresh "Auto update data" commit dated
   today.
2. **Did the site deploy?** Check the **Quarto Publish** Action run is green, and the public
   Pages site shows today's date in the headline.
3. **Did the portal rebuild?** If the external dashboard portal is also stale, check the
   **Trigger Rebuild** Action and the `GH_PAT` validity.
4. **Sanity-check the international page** (most fragile): if every country looks like "Noregur",
   or values are blank, the TE scrape failed — inspect `data/debug_te_*.html`.
5. **Sanity-check the forecast** around month boundaries: if the 6m/12m figures look frozen,
   bump the hardcoded anchor dates (§5).

---

## Open questions

These could not be determined from the repository alone and are genuine handover gaps:

1. **The exact manual-update procedure for `verdbolguskyrsla_data.xlsx`** (and the two
   expectations workbooks): which report columns map to which sheet/columns, how new months are
   appended, and the per-source cadence. The *download sources* are known (§2); the *data-entry
   mechanics* are not in the repo.
2. **Whether the Task Scheduler jobs are currently registered and active** on the VM, and under
   which user account. `R/schedule.R` *defines* them, but the live state of Windows Task Scheduler
   can't be read from the repo. Confirm with `Get-ScheduledTask` / Task Scheduler GUI on the VM.
3. **What `hagdeild/dashboard-portal` does** with the `dashboard-updated` dispatch, and where that
   portal is hosted/consumed. It's a separate repo, out of scope here.
4. **Provenance of the data in `data/greiningar/`** (the oil-price scenario CSVs and historical
   analyst forecasts) — these appear to be produced by ad-hoc analysis (possibly the
   `R/tilfallandi/` scripts or external work), not by the daily pipeline. How/when they get
   refreshed is unclear.
5. **The private `vrmodel` package** used by `R/retune_forecast.R` — where it lives and how to
   install it. Not needed for the daily dashboard, but required if you want to re-tune the model.
