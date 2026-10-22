# This script estimates stand age from RESULTS

# Load libraries
library(sf)
library(fs)
library(data.table)

in_dir <- "01-data_inputs"
out_dir <- "01-data_inputs"



#-- Load data-------------------------------------------------------------------
# Plot/treatment data:
FR_treatments <- fread(file.path(in_dir,"FR_Treatments.csv"))
setnames(FR_treatments,"ID", "PlotID")

# import RESULTS overlap with plots
FROpenings <- fread(file.path(in_dir, "FRplots_Openings.csv")) 
setnames(FROpenings, "ID", "PlotID")
FROpenings[, HARVEST_YEAR := as.integer(substr(DISTURBANC, nchar(DISTURBANC) - 3, nchar(DISTURBANC)))]
FROpenings<-FROpenings[DENUDATION == "L"]
FR_treatments<-merge(FR_treatments, FROpenings[, .(PlotID,DISTURBANC, DENUDATION,Age, HARVEST_YEAR)], by = "PlotID", all.x = TRUE) # four sites say L from FROpenings but not harvest from FR_treatments

FR_treatments[, Stand_Age := fcase(FIRE_YEAR > HARVEST_YEAR & CumBurnSev %in% c("M", "MH", "H"), 2020 - FIRE_YEAR,  # Fire occurred after harvest and burn severity was Moderate or High
                                FIRE_YEAR > HARVEST_YEAR & CumBurnSev %in% c("L", "LM"), 2020 - HARVEST_YEAR, # Fire occurred after harvest and burn severity was Low
                                FIRE_YEAR < HARVEST_YEAR, 2020 - HARVEST_YEAR, default = NA_real_)] # Harvest occurred after fire
#look into why so many NAs and check L/LM burns, and all Logegd sites
