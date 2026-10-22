# Data prep for SKWERM model

# This script calculates/compiles the necessaru fields for SKWERM

# Nikki Beaudoin
# October 22, 2026


# Load libraries
library(data.table)
library(tidyverse)
library(ggpubr)

in_dir <- "01-data_inputs"
out_dir <- "02-prepped_values"

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
structstage <- fread(file.path(in_dir,"FireRehab_structural_stage.csv"))

# scale function
scale_fn <- function(var){(var - min(var)) / (max(var) - min(var))}

# Cleaning (small)
#Plot treatment cleaning
FR_treatments[, `:=`(PlotID = factor(PlotID), Planted = factor(Planted),TimeSinceFire = 2020 - FIRE_YEAR)]
#for this paper, we don't need all the columns:
plot_treatments <- FR_treatments[,.(PlotID, Planted, TimeSinceFire, CumBurnSev)]
