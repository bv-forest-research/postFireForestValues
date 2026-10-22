# Data prep for SKWERM model

# This script calculates/compiles the necessaru fields for SKWERM

# Nikki Beaudoin
# October 22, 2026


# Load libraries
library(data.table)
library(tidyverse)
library(ggpubr)

in_dir <- "C:/Users/Nikki Beaudoin/Documents/r_repo/postFireForestValues/01-data_inputs"
out_dir <- "C:/Users/Nikki Beaudoin/Documents/r_repo/postFireForestValues/02-prepped_values"

files_to_source <- list.files("./R/00-utils/", pattern = "Function", 
                              full.names = TRUE)
sapply(files_to_source, source)

#-- Load data-------------------------------------------------------------------
# Plot/treatment data:
FR_treatments <- fread(file.path(in_dir,"FR_Treatments.csv"))
setnames(FR_treatments,"ID", "PlotID")

# Plot location data with BEC subzones:
FRplots_dt <-fread(file.path(in_dir, "FRplots_BEC.csv"))
setnames(FRplots_dt, "Name", "PlotID")

# Structural stage
FR_structural_stage <- fread(file.path(in_dir,"FireRehab_structural_stage.csv"))

#Tree data:
A1trees <- fread(file.path(in_dir,"A1trees.csv"))
B1trees <- fread(file.path(in_dir,"B1trees.csv"))
Regen <- fread(file.path(in_dir,"Regen.csv"))

#Soils data:
Soils <- fread(file.path(in_dir,"Soils.csv"))

#Woody debris:
cwd <- fread(file.path(in_dir,"FireRehabData_CWD.csv"),stringsAsFactors = T)
fwd <- fread(file.path(in_dir,"FireRehabData_FWD.csv"),stringsAsFactors = T)
line <- fread(file.path(in_dir,"FireRehabData_TransectDistance.csv"),stringsAsFactors = T)
setnames(line, "Plot","PlotID")

# Cover
densiometer <- fread(file.path(in_dir,"FRdensiometer.csv"))
Cover <- fread(file.path(in_dir,"FRstrataCover.csv")) #B1= <2m and B2=2-10m shrub heights)
setnames(Cover, c("Total_B1", "Total_B2"), c("ShrubsB1", "ShrubsB2"))
ShrubVolume <- fread(file.path(in_dir,"FR_shrubVolumes.csv"))

# scale function
scale_fn <- function(var){(var - min(var)) / (max(var) - min(var))}

# Cleaning (small)
#Plot treatment cleaning
FR_treatments[, `:=`(PlotID = factor(PlotID), TimeSinceFire = 2020 - FIRE_YEAR)]
#for this paper, we don't need all the columns:
plot_treatments <- FR_treatments[,.(PlotID, Aspect, Slope_PC, elevation,  TimeSinceFire)]

# Calculate plot attributes ----------------------------------------------------------------------------------
# These attributes are metrics that will be used in one or many of the habitat indices
# CWD quality
PlotCQI <- cwdQI(cwd)
PlotCQI[, CQI:=round(CQI*0.01, digit=2)] #values between 0-1; 0=bad 1=best

# CWD piles
PlotPiles <- cwdPiles(cwd)
PlotPiles[PlotID=="FR17" & Transect=="1", PileCount := 2] # notes on raw data says in 2 large piles
PlotPiles <- PlotPiles[,.(PileCount = sum(PileCount)), by = PlotID]

# CWD volume
PlotCWD <- cwdVol(cwd, line)

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
 # live by species
plotBAPH_species<-BAPH_species(A1trees, B1trees)
plotBAPH_species_wide <- dcast(
  plotBAPH_species,
  PlotID ~ Species,
  value.var = "BAPH",
  fill = 0)
setnames(plotBAPH_species_wide, old = names(plotBAPH_species_wide)[names(plotBAPH_species_wide) != "PlotID"],
         new = paste0("BAPH_", names(plotBAPH_species_wide)[names(plotBAPH_species_wide) != "PlotID"]))
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
treeComp <- rbind(treeComp[, .(PlotID, Species, spComp)], missingPlots, fill = TRUE) #adding in plots with no large trees and assigning them a SpComp of 1 for NA

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
                                                 fifelse(conComp == 0 & broadComp == 0, "NF", NA_character_))))] #adding in mixed and non-forested class 


# Individual shrub species percent cover (calculated from their volumes) and average height
PlotShrubCov <- ShrubSpCov(ShrubVolume)
PlotShrubCov[Species == "MOHOAQU", Species := "MAHOAQU"]
# % cover of huckleberry berries
PlotHuckberry <- Huckberry(PlotShrubCov, PlotCrown)
PlotThermalForage <- ThermalForage(PlotShrubCov, PlotCrown)

# Combine plot data
plot_treatments<-merge(plot_treatments,FRplots_dt[,.(PlotID, Longitude,Latitude, ZONE, SUBZONE, VARIANT, PHASE)], by = "PlotID")
plot_treatments<-merge(plot_treatments,FR_structural_stage, by = "PlotID")
plot_treatments<-merge(plot_treatments,standType, by = "PlotID")
plot_treatments<-merge(plot_treatments,plotBAPH_species_wide, by = "PlotID")
plot_treatments<-merge(plot_treatments,PlotCrown, by = "PlotID")
setnames(plot_treatments, c("Aspect", "Slope_PC", "elevation", "structural_stage","CrownClos"),
         c("ASPECT","SLOPE", "ELEV", "STRCT","CR_CLOSURE"))
plot_treatments[,SITE_M3A := NA_character_]


