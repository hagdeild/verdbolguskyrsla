# Schedule R code

library(taskscheduleR)


# Update bonds
taskscheduler_create(
  taskname = "verdbolgu_update_althjodleg",
  rscript = "c:/Users/vidar/Documents/Rwd/verdbolguskyrsla/R/althjodleg_data.R",
  schedule = "DAILY",
  starttime = "16:00",
  startdate = format(Sys.Date(), "%d.%m.%Y")
)

# data preparation
taskscheduler_create(
  taskname = "verdb_data_prep",
  rscript = "c:/Users/vidar/Documents/Rwd/verdbolguskyrsla/R/data_preparation.R",
  schedule = "DAILY",
  starttime = "09:01",
  startdate = format(Sys.Date(), "%d.%m.%Y")
)
