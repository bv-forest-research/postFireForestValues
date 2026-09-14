

library(data.table)
library(dplyr)
source("./R/CarbonFunctions.R")
source("./R/CWD_carbon.R")
source("./R/FWD_carbon.R")

## set pathways 
in_dir <- "01_data_inputs"
out_dir <- "02_prepped_values"


## Import data
cwd <- read.csv(file.path(in_dir,"FireRehabData_CWD.csv"), header = T, 
                stringsAsFactors = T)
fwd <- read.csv(file.path(in_dir,"FireRehabData_FWD.csv"), header = T, 
                stringsAsFactors = T)
line <- read.csv(file.path(in_dir,"FireRehabData_TransectDistance.csv"), header = T, 
                 stringsAsFactors = T) 
Soils <- fread(file.path(in_dir,"Soils.csv"))
FR_treatments <- fread(file.path(in_dir,"FR_Treatments.csv"))

#ground fuels (tonnes/ha) -------------------------------------------------
litter_biomass <- (Soils[, Litter_DryWgt + Litter_Wgt_inMin])/16
ff_biomass <- Soils[, sum(ForFloor_DryWgt, 
                           Wood_chunks_inFF,
                           Black_C_wgt_inFF,
                           na.rm=TRUE), by = "ID"]
################################## Forest floor carbon ##################################
Soils[,ForestFloor_C_g := sum((ForFloor_DryWgt*ForFloor_C_PC/100), 
                              Wood_chunks_inFF*0.5,
                              Black_C_wgt_inFF*0.75,
                              na.rm=TRUE), 
      by="ID"]
#tiny amt of FF in FR03 didn't make it to the lab, so just use 0.5
Soils[ID=="FR03",ForestFloor_C_g := ForFloor_DryWgt*0.5 +
        Wood_chunks_inFF*0.5 +
        Black_C_wgt_inFF*0.75 ]
#the only true NA (missing sample) is FR02
Soils[ID=="FR02",ForestFloor_C_g := NA]
FR_treatments[,ForestFloor_C_g := Soils[,ForestFloor_C_g]]
FR_treatments[,ForestFl_MgHa :=ForestFloor_C_g/16] #converting to Mg/Ha


cwd_biomass <- as.data.table(cwd_biom_plots(line_dat = line, cwd_dat = cwd))
fwd_biomass <- as.data.table(fwd_biom_plots(line_dat = line, fwd_dat = fwd))




