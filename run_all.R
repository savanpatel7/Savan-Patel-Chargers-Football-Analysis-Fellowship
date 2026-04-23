# Run full pipeline from scratch

Sys.setenv(R_MAX_VSIZE = "32Gb")

rm(list = ls())
gc()

source("00_data.R")
source("01_helpers.R")
source("02_markov_first_pass.R")
source("03_terminal_adjustments.R")
source("04_markov_second_pass.R")
source("05_epa_computation.R")
source("06_second_short_analysis.R")
source("07_visualizations.R")

cat("Pipeline complete\n")
