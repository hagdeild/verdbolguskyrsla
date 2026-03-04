# Schedule R code


library(vr)


# Update bonds
schedule_r_script(
  rscript = "R/update_bonds.R",
  taskname =  "verdbolgu_update_bonds",
  schedule = "DAILY",
  starttime = "16:00"
)

# data preparation
schedule_r_script(
  rscript = "R/data_preparation.R",
  taskname =  "verdbolguskyrsla_data_preparation.R",
  schedule = "DAILY",
  starttime = "09:01"
)
