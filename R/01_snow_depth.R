# Estimate snow depth and calculate snow interception

# This script estimates snow depth and accumulation factors

# Nikki Beaudoin
# October 10, 2026

# Install packages
install.packages("terra")
install.packages("C:/Users/Nikki Beaudoin/Downloads/ClimateNAr.zip",repos = NULL, type = "win.binary")

# Load libraries
library(data.table)
library(tidyverse)
library(ggpubr)
library(terra)
library(ClimateNAr)

in_dir <- "01-data_inputs"
out_dir <- "01-data_inputs"

files_to_source <- list.files("./R/00-utils/", pattern = "Function", 
                              full.names = TRUE)
sapply(files_to_source, source)

#-- Load data-------------------------------------------------------------------
# Plot/treatment data:
FR_treatments <- fread(file.path(in_dir,"FR_Treatments.csv"))
setnames(FR_treatments, "ID", "PlotID")

# Plot location data:
Plot_latlong <-fread(file.path(in_dir, "FireRehabPlots_LatLong.csv"))
setnames(Plot_latlong, "ID", "PlotID")

#Tree data:
A1trees <- fread(file.path(in_dir,"A1trees.csv"))
B1trees <- fread(file.path(in_dir,"B1trees.csv"))
Regen <- fread(file.path(in_dir,"Regen.csv"))

# Cover
densiometer <- fread(file.path(in_dir,"FRdensiometer.csv"))
Cover <- fread(file.path(in_dir,"FRstrataCover.csv")) #B1= <2m and B2=2-10m shrub heights)
setnames(Cover, c("Total_B1", "Total_B2"), c("ShrubsB1", "ShrubsB2"))
ShrubVolume <- fread(file.path(in_dir,"FR_shrubVolumes.csv"))
structstage <- fread(file.path(in_dir,"FireRehab_structural_stage.csv"))

# scale function
scale_fn <- function(var){(var - min(var)) / (max(var) - min(var))}

# Cleaning (small)
#Plot treatment cleaning
FR_treatments[, `:=`(PlotID = factor(PlotID), Planted = factor(Planted),TimeSinceFire = 2020 - FIRE_YEAR)]
Plot_latlong[, `:=`(PlotID = factor(PlotID))]

#for this paper, we don't need all the columns:
Snow_data <- merge(FR_treatments[, .(PlotID, Aspect, elevation, Slope_PC)],
                        structstage, by = "PlotID",  all.x = TRUE)
Snow_data <- merge(Snow_data, Plot_latlong, by = "PlotID", all.x = TRUE)

# Calculate plot attributes ----------------------------------------------------------------------------------
# These attributes are metrics that will be used in one or many of the habitat indices

# Crown closure
# mean of densiometer readings and multiply by 1.04 to get canopy openness
PlotCrown <- densiometer[,.(SubPlotOpen=mean(Densiometer)), by = c("PlotID", "SubPlot")]
PlotCrown <- PlotCrown[,.(CrownOpen=mean(SubPlotOpen)*1.04), by = PlotID]
# convert openness to canopy closure
PlotCrown[, CrownClos:= 100-CrownOpen][, CrownOpen := NULL]
# ** what should we do with FR01 crown closure wasn't measured**

# Basal area
# live
PlotBAPHlive <- BAPHlive(A1trees, B1trees)
# dead
PlotBAPHdead <- BAPHdead(A1trees, B1trees)

# Tree density/sph
# live
PlotTree <- TreeDensity(A1trees, B1trees, ClassSize = 2)
# dead
PlotSnags <- SnagDensity(A1trees, B1trees)

# Tree composition
treeComp <- PlotTree[DBH_bin >=12, totalSPH := sum(SPH), by=c("PlotID")] # add detail as to why the cut off (I think because it's targetting the large trees)
treeComp <- treeComp[DBH_bin >=12, .(spComp=sum(SPH)/totalSPH), by=c("PlotID", "Species")]
treeComp <- unique(treeComp)
allPlots <- data.table(PlotID = sprintf("FR%02d", 1:75))
missingPlots <- allPlots[
  !PlotTree[DBH_bin >= 12, unique(PlotID)],
  on = "PlotID"
]
missingPlots[, `:=`(Species = NA_character_, spComp = 1)]
treeComp <- rbind(treeComp[, .(PlotID, Species, spComp)], missingPlots, fill = TRUE)

# Stand type
# Classify stand type - which is based on tree species composition
# Conifer (c) - Greater than 3/4 of total tree layer cover is coniferous
# Broadleaf (B) - Greater than 3/4 of total tree layer cover is broadleaf
# Mixed (M) - Neither coniferous or broadleaf account for > 75% of total tree layer cover
standType <- treeComp[, .(conComp = sum(spComp[Species %in% c("Sx", "Pl", "Bl", "UC", "Lw", "Fd")]),
                          broadComp = sum(spComp[Species %in% c("Ac", "At", "Ep")])), by = PlotID]
standType[, StandType := fifelse(conComp > 0.75, "C",
  fifelse(broadComp > 0.75, "B",
    fifelse(conComp > 0.25 & conComp < 0.75 & broadComp > 0.25 & broadComp < 0.75, "M",
      fifelse(conComp == 0 & broadComp == 0, "NF", NA_character_))))]

Snow_data <- merge(Snow_data, PlotCrown, by = "PlotID", all.x = TRUE)
Snow_data <- merge(Snow_data, standType, by = "PlotID", all.x = TRUE)

# Individual shrub species percent cover (calculated from their volumes) and average height
PlotShrubCov <- ShrubSpCov(ShrubVolume)
PlotShrubCov[Species == "MOHOAQU", Species := "MAHOAQU"]
PlotThermalForage <- ThermalForage(PlotShrubCov, PlotCrown)


# Climate
climate_input <- unique(Snow_data[, .(ID1 = PlotID, ID2 = ".", lat = Latitude, long = Longitude, el = elevation)])
climate_input <- climate_input[complete.cases(climate_input[, .(lat, long, el)])]
input_file <- file.path(tempdir(),"Snow_data_climate_input.csv")
fwrite(climate_input, input_file)
climate_result <- ClimateNA_API2(inputFile = input_file, period = "Normal_1991_2020.nrm", MSY = "S")
setnames(climate_result, "ID1", "PlotID")


# Relative Snow Index ####
Snow_data <- merge(Snow_data, climate_result, by = "PlotID", all.x = TRUE)
snow <- copy(Snow_data)
snow[, `:=`(PAS_wt = as.numeric(PAS_wt), Slope_PC = as.numeric(Slope_PC), Aspect = as.numeric(Aspect),CrownClos = as.numeric(CrownClos),
  StandType = toupper(trimws(as.character(StandType))))]
snow[, input_complete := complete.cases(PAS_wt, Slope_PC, Aspect, CrownClos, StandType)]

# Modifiers

# Slope modifier:
# 1.00 at 0-20% slope, declining linearly to 0.75 at 100% slope.
# This is a scenario assumption, not a measured relationship.
snow[, slope_mod := 1 - 0.25 * pmin(pmax(Slope_PC - 20, 0) / 80, 1)]

# Aspect modifier:
# Northness ranges from +1 (north) to -1 (south).
# South-facing plots receive up to a 15% reduction.
snow[, northness := cos(Aspect * pi / 180)]
snow[, aspect_mod := 1 - 0.15 * ((1 - northness) / 2)]

# Crown-closure modifier:
# Linear interception scenario: 0% closure = 1.00;
# 100% closure = 0.75.
# This deliberately does NOT claim to represent the full net canopy effect.
snow[, canopy_mod := 1 - 0.25 * CrownClos / 100]

# Stand-type modifier:
# Illustrative starting values; adjust and test in sensitivity analysis.
stand_modifiers <- c("C"  = 0.85, "M"  = 0.92, "B"  = 1.00, "NF" = 1.00)
snow[, stand_mod := unname(stand_modifiers[StandType])]
if (any(is.na(snow$stand_mod) & !is.na(snow$StandType))) {
  print(snow[is.na(stand_mod), .(PlotID, StandType)])
  warning("Some stand types have no modifier; check their codes.")
}

# Calculate Relative Snow Index
# This is an index in adjusted PAS-equivalent units, NOT measured SWE.
snow[, relative_snow_index := fifelse(input_complete & !is.na(stand_mod),
  PAS_wt * slope_mod * aspect_mod * canopy_mod * stand_mod, NA_real_)]

# Normalize to a 0-100 scale for easy plot comparison.
# This is relative to these plots only, not an absolute snow scale.
max_index <- max(snow$relative_snow_index, na.rm = TRUE)

snow[, relative_snow_score := fifelse(!is.na(relative_snow_index) & max_index > 0,
  100 * relative_snow_index / max_index, NA_real_)]

# Review plot-level results
snow[, .(PlotID, PAS_wt, Slope_PC, Aspect, CrownClos, StandType, slope_mod, aspect_mod, canopy_mod, stand_mod,
  relative_snow_index, relative_snow_score)][order(-relative_snow_score)]

