# This script estimates stand age from RESULTS

# Load libraries
library(sf)
library(fs)
library(data.table)

in_dir <- "01-data_inputs"
out_dir <- "01-data_inputs"



#-- Load data-------------------------------------------------------------------
# import RESULTS overlap with plots
FROpenings <- fread(file.path(in_dir, "FRplots_Openings.csv")) 
setnames(FROpenings, "ID", "PlotID")
FROpenings[, FR_age:= fifelse(CumBurnSev %in% c("LM", "M", "MH", "H"), 2020 - FIRE_YEAR,
                              fifelse(CumBurnSev == "L", 2020 - as.integer(substr(DISTURBANC, nchar(DISTURBANC) - 3, nchar(DISTURBANC))),
                                      NA_real_))]
