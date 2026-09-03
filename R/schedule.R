# Registration script for the verdbolguskyrsla scheduled jobs.
#
# WHY THE TASKS ARE PREFIXED `hd_`
#
# The unprefixed tasks (verdbolgu_update_althjodleg, verdb_data_prep) were
# registered during the vidar -> hd migration from an elevated session, so their
# files under C:\Windows\System32\Tasks are owned by BUILTIN\Administrators and
# grant VR\hd only Read. `hd` cannot overwrite them with /F, /Change them, or
# disable them -- every route returns "ERROR: Access is denied" -- and `hd` is
# not in any Administrators group.
#
# Registering under a name `hd` owns sidesteps that entirely. The old tasks must
# be DISABLED from an account that can (an administrator, or `vidar`), otherwise
# each job runs twice. See Rwd_scheduled_tasks_handoff.md section 4.
#
# ORDER MATTERS. althjodleg_data.R (16:00) writes data/altjodlegar.csv, which
# data_preparation.R consumes the NEXT morning at 09:01. The pipeline runs
# across midnight -- do not collapse these two into one slot.
#
# To register (or re-register after an R upgrade), from the repo root:
#     Rscript R/schedule.R
#
# Registered by calling schtasks directly rather than through
# taskscheduleR::taskscheduler_create(), following the pattern in
# starfsmennt_lykiltolur/R/schedule.R: it makes the resulting command visible in
# this file and adds no package dependency to a script whose only job is to
# shell out to schtasks.

# Find the newest R x64 Rscript.exe under the standard install root, rather than
# hardcoding a version. Several R majors are installed side by side on this
# machine and pinned paths drifted out of date during the vidar -> hd account
# migration. Re-run this script after an R upgrade to re-point the tasks.
find_newest_rscript <- function() {
  roots <- file.path(Sys.getenv("PROGRAMFILES"), "R")
  dirs <- list.dirs(roots, recursive = FALSE)
  dirs <- dirs[grepl("^R-[0-9]+[.][0-9]+[.][0-9]+$", basename(dirs))]
  if (length(dirs) == 0) stop("No R installation found under ", roots)
  ver <- numeric_version(sub("^R-", "", basename(dirs)))
  newest <- dirs[order(ver, decreasing = TRUE)][1]
  rscript <- file.path(newest, "bin", "x64", "Rscript.exe")
  if (!file.exists(rscript)) stop("Rscript.exe not found at ", rscript)
  # Return the 8.3 SHORT path (C:/PROGRA~1/...). This is essential, not
  # cosmetic. The whole /TR string below is quoted as ONE argument, so a path
  # inside it that contains "Program Files" splits on the space and schtasks
  # fails with a misleading "ERROR: Access is denied."
  gsub("\\\\", "/", utils::shortPathName(rscript))
}

rexe <- find_newest_rscript()
message("Scheduling tasks with: ", rexe)

project_dir <- "c:/Users/hd/Rwd/verdbolguskyrsla"

# `cmd /c ... >> log 2>&1` so the run leaves a transcript; Rscript on its own
# writes to a console nobody is watching. No path here contains a space, so no
# inner quoting is needed -- shQuote() wraps the whole /TR value in the one
# layer of quotes that keeps cmd from treating `>>` as its own redirection.
build_tr <- function(script) {
  full <- file.path(project_dir, script)
  if (!file.exists(full)) stop("Script not found: ", full)
  log <- sub("[.]R$", ".log", full)
  paste("cmd /c", rexe, full, ">>", log, "2>&1")
}

tasks <- list(
  list(
    name = "hd_verdbolgu_update_althjodleg",
    script = "R/althjodleg_data.R",
    # /F overwrites an existing task of the same name. Without it schtasks asks
    # "already exists, replace? (Y/N)", which nothing can answer under Rscript,
    # so re-registration silently fails.
    args = c("/SC", "DAILY", "/ST", "16:00", "/F"),
    what = "16:00 every day"
  ),
  list(
    name = "hd_verdb_data_prep",
    script = "R/data_preparation.R",
    args = c("/SC", "DAILY", "/ST", "09:01", "/F"),
    what = "09:01 every day"
  )
)

for (task in tasks) {
  tr <- build_tr(task$script)
  status <- system2("schtasks", c(
    "/Create", "/TN", shQuote(task$name), "/TR", shQuote(tr), task$args
  ))
  if (status != 0) {
    stop("schtasks /Create failed for ", task$name, " with exit code ", status)
  }
  message("Scheduled: ", task$name, " -- ", task$what)
  message("  Command: ", tr)
}

# The superseded tasks. Deleting them is EXPECTED to fail with "Access is
# denied" while running as `hd` -- see the header -- so the status is reported
# rather than checked.
for (old_task in c("verdbolgu_update_althjodleg", "verdb_data_prep")) {
  if (system2("schtasks", c("/Delete", "/TN", shQuote(old_task), "/F")) != 0) {
    message(
      "Could not remove the superseded task '", old_task, "'. It is still ",
      "ENABLED and will keep running alongside the new one. Disable or delete ",
      "it from an administrator or `vidar` session:\n",
      "    schtasks /Change /TN \"", old_task, "\" /DISABLE"
    )
  }
}
