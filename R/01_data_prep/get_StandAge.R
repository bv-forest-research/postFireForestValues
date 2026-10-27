# This script estimates stand age from RESULTS
#install.packages("tidyverse")
# Load libraries
library(sf)
library(fs)
library(data.table)
library(tidyverse)

in_dir <- "01-data_inputs"
out_dir <- "01-data_inputs"



#-- Load data-------------------------------------------------------------------
# Plot/treatment data:
FR_treatments <- fread(file.path(in_dir,"FR_Treatments.csv"))
setnames(FR_treatments,"ID", "PlotID")

# import RESULTS overlap with plots
FROpenings <- fread(file.path(in_dir, "FRplots_Openings.csv")) 
setnames(FROpenings, "ID", "PlotID")

#Extract harvest year from DISTURBANC
FROpenings[, HARVEST_YEAR := as.integer(substr(DISTURBANC, nchar(DISTURBANC) 
                                               - 3, nchar(DISTURBANC)))]
# Keep only logged sites
FROpenings<-FROpenings[DENUDATION == "L"]

# Join harvest info to treatment data
FR_treatments<-merge(FR_treatments, FROpenings[, .(PlotID,DISTURBANC, 
                    DENUDATION,Age, HARVEST_YEAR)], by = "PlotID", all.x = TRUE) 

# Join Tree info to treatment data
B1trees <- fread(file.path(in_dir,"B1trees.csv")) # do we only care about big trees?
TreeCounts <- B1trees[, .(
  Living_Trees = sum(Tree_class %in% c(1, 2), na.rm = TRUE),
  Dead_Trees = sum(Tree_class >= 3, na.rm = TRUE)
), by = PlotID]

FR_treatments<-merge(FR_treatments, TreeCounts, by = "PlotID") # what if no trees?

# Retain only variables of interest
FR_treatments<- FR_treatments[, .(PlotID, CumBurnSev, DENUDATION, FIRE_YEAR, HARVEST_YEAR, Living_Trees, Dead_Trees)]
FR_treatments |>
  arrange(DENUDATION, CumBurnSev)

# Calculate stand age
FR_treatments[, Stand_Age := fcase(is.na(HARVEST_YEAR) & !is.na(FIRE_YEAR), 2020 - FIRE_YEAR,
                            FIRE_YEAR > HARVEST_YEAR & CumBurnSev %in% c("M", "MH", "H"), 2020 - FIRE_YEAR,
                            FIRE_YEAR > HARVEST_YEAR & CumBurnSev %in% c("L", "LM") &
                              DENUDATION == "L" &
                              Dead_Trees > Living_Trees, 2020 - FIRE_YEAR,
                            FIRE_YEAR > HARVEST_YEAR & CumBurnSev %in% c("L", "LM") &
                              DENUDATION == "L" &
                              Living_Trees > Dead_Trees, 2020 - HARVEST_YEAR,
                            FIRE_YEAR > HARVEST_YEAR & CumBurnSev %in% c("L", "LM") &
                              DENUDATION == "L" &
                              Living_Trees == 0 & Dead_Trees ==0, 2020 - HARVEST_YEAR,
                            FIRE_YEAR < HARVEST_YEAR, 2020 - HARVEST_YEAR,
                            FIRE_YEAR == HARVEST_YEAR, 2020 - FIRE_YEAR,
                            default = NA_real_)]

FR_treatments |>
  arrange(DENUDATION, CumBurnSev)

