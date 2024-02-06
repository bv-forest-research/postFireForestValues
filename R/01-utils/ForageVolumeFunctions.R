# Forage Volume Functions

shrubs <- fread("./01-data_inputs/FR_shrubVolumes.csv")


# Shrub species percent cover
ShrubSpCov <- function(shrubDat){
  #-- Prep/clean the data
  # fix Species names to be 7 letter code and capitalize
  shrubs$Species <- toupper(shrubs$Species)
  # remove underscore
  shrubs$Species <- sub("_", "", shrubs$Species)
  #remove Pyro sp., ASTECON (not a shrub, not consistently sampled)
  shrubs <- shrubs[!(Species %in% c("PYROSP", "ASTECON"))]
  
  #-- Calculate percent cover
  # A1 plot = 5.64m radius = 100m2, c1/C2/C3/C4 = 3.99m radius = 50m2
  # Account for the sub-sampling of some plots
  shrubs[, AreaSearchM2 := ifelse(SubPlot == "A1", 100, 50)]
  # convert from cm2 to m2, / by subplot area, X100 = percent cover
  shrubs[, PerCov := (((Diam1*Diam2)*0.0001)/AreaSearchM2)*100]
  #shrubs[, .(PerCov = sum(PerCov)), by = c("PlotID", "Species")]
  
  # Account for sub-sampling of species
  # Some sub-samples didn't need to be sub-sampled i.e. have 10/10 sampled
  shrubs[Total_Number == 10, Total_Number := NA]
  shrubs[, PerCov := ifelse(!is.na(Total_Number), mean(PerCov)*Total_Number, PerCov), by = .(PlotID, SubPlot, Species)]
  # Now that the species sub-plots have been calculated only keep one of the rows to represent the perCov
  shrubs <- shrubs[!is.na(Total_Number), unique(shrubs, by = c("PlotID", "SubPlot", "Species", "Total_Number"))]
  
  # Combine salix species and calculate total percent cover per plot per species
  PlotShrubCov <- shrubs[grepl("SALI", Species), Species := "SALISPP"][, .(PerCov = sum(PerCov)), by = .(PlotID, Species)]
  
  return(PlotShrubCov)
}


# Grizzly bear available huckleberries
# This function uses only considers huckleberries in a canopy cover >50% to have berries (Nielson et a. 2004)
Huckberry <- function(PlotShrubCov, PlotCrown) {
  # huckleberry berry
  PlotHuckberry <- merge(PlotShrubCov[Species == "VACCMEM"], PlotCrown)
  PlotHuckberry[, HuckCov := ifelse(CrownClos < 0.5, PerCov, 0)]
  
  return(PlotHuckberry)
}


# Thermal cover with forage
ThermalForage <- function(PlotShrubCov, PlotCrown) {
  PlotShrubCov <- merge(PlotShrubCov, PlotCrown)
  # GB shrub/berry forage - not including VACCMEM, VACCCAE because they require more open canopies to produce fruit
  ThermalForage <- PlotShrubCov[Species %in% c("AMELALN", "CORNSTO", "LONIINV", "RIBELAC",
                                         "SHEPCAN", "VACCOVA", "VACCMYR", "VIBUEDU")]
  ThermalForage[, ThermForage := ifelse(CrownClos > 0.5, PerCov, 0)]
  PlotThermalForage <- ThermalForage[, .(ThermForage = sum(ThermForage)), by = PlotID]
  return(PlotThermalForage)
}

# This code calculates volume of available forage for hares (below 1.5 m in height).
ShrubVol <- function(shrubDat){
  #-- Prep/clean the data
  # fix Species names to be 7 letter code
  # capitalize
  shrubs$Species <- toupper(shrubs$Species)
  # remove underscore
  shrubs$Species <- sub("_", "", shrubs$Species)
  # combine Salix species
  shrubs[grepl("SALI", Species), Species := "SALISPP"]
  # when NA in Full_Height, takes values from Foliage_Height 
  shrubs$Full_Height <- coalesce(shrubs$Full_Height,shrubs$Foliage_Height)
  
  #-- Calculate the available length
  # This is the length of foliage that is available for hares to browse in all seasons
  # below 1.5m
  shrubs[, Avail_Length := ifelse(Full_Height > 150 & Lower_Canopy_Height > 150, 
                                  0,
                                  ifelse(Full_Height > 150,
                                         150 - Lower_Canopy_Height,
                                         Full_Height - Lower_Canopy_Height))]
  
  #-- Calculate total foliage volume/hectare
  # A1 plot = 5.64m radius = 100m2, c1/C2/C3/C4 = 3.99m radius = 50m2
  # convert to m3/ha: cm3/m2 / 100:
  # (shrub volume(cm3) / plot area(m2)) x (1m3 / 1,000,000cm3) x (10,000m2 / 1ha) 
  # also here, we account for the sub-sampling of some plots
  shrubs[, AreaSearchM2 := ifelse(SubPlot == "A1", 100, 50)]
  shrubs[, m3PerHa_fac := AreaSearchM2 * 100]
  shrubs[, shrubVolHa := (Avail_Length * Diam1 * Diam2) / m3PerHa_fac]
  
  # Now deal with sub-samples of shrub species (when >10 individuals of a species)
  # Calculate the mean vol of the sub-sample
  
  #Not accounting for unavailable forage, did not run this line of code: ** question for Joc **
  #shrubs$Avail_Browse <- ifelse(is.na(shrubs$UF),shrubs$Foliage_Vol,shrubs$Foliage_Vol*(1-(shrubs$UF/100)))
  PlotShrubVol <- shrubs[, .(shrubVolHa = ifelse(is.na(Total_Number),
                                           sum(shrubVolHa),
                                           mean(shrubVolHa) * Total_Number)),
                   by = .(PlotID, SubPlot, Species, Total_Number)]
  PlotShrubVol <- unique(PlotShrubVol)
  PlotShrubVol <- PlotShrubVol[, .(shrubVolHa = sum(shrubVolHa)), by = .(PlotID, Species)]
  PlotShrubVol <- PlotShrubVol[!is.na(shrubVolHa), ] # remove the NA rows
   return(PlotShrubVol)
}


# Hare forage volumes
HareForageVol <- function(shrubVolDat) {
  # Select species that are browsed by hares
  HareForage <- filter(PlotShrubVol,grepl('PINUCON|AMELALN|BETU|JUNISCO|
                                      SHEPCAN|ROSA|RUBU|SYMPALB|
                                      VACC|POPU|SALI',Species))
  # Combine forage species so have total vol of forage for hares
  PlotHareForage <- HareForage[, .(shrubVolHa = sum(shrubVolHa)), by = PlotID]
  return(PlotHareForage)
}


# Grizzly bear berries forage volume
# questions here: do we just do available forage or do we just do berry species and incorporate canopy cover
# in the indice?
GBForageVol <- function(ShrubVolDat) {
  GBForage <- filter(PlotShrubVol, grepl('AMELALN|CORNSTO|LONIINV|RIBELAC|
                                         SHEPCAN|VACC|VIBUEDU', Species))
  # Combine forage species to have total vol of forage for hares -- if we decide to go this route
  PlotGBForage <- GBForage[, .(shrubVolHa = sum(shrubVolHa)), by = PlotID]
  return(PlotGBForage)
}


#---------------------------------------------
# Not using this funciton - but keep
# Available forage for snowshoe hare

# ShrubVol <- function(shrubDat){
#   #-- Prep/clean the data
#   # fix Species names to be 7 letter code
#   # capitalize
#   shrubs$Species <- toupper(shrubs$Species)
#   # remove underscore
#   shrubs$Species <- sub("_", "", shrubs$Species)
#   # combine Salix species
#   shrubs[grepl("SALI", Species), Species := "SALISPP"]
#   # when NA in Full_Height, takes values from Foliage_Height 
#   shrubs$Full_Height <- coalesce(shrubs$Full_Height,shrubs$Foliage_Height)
#   
#   #-- Calculate the available length
#   # This is the length of foliage that is available for hares to browse in all seasons
#   # below 1.5m
#   shrubs[, Avail_Length := ifelse(Full_Height > 150 & Lower_Canopy_Height > 150, 
#                                   0,
#                                   ifelse(Full_Height > 150,
#                                          150 - Lower_Canopy_Height,
#                                          Full_Height - Lower_Canopy_Height))]
#   
#   #-- Calculate total foliage volume/hectare
#   # A1 plot = 5.64m radius = 100m2, c1/C2/C3/C4 = 3.99m radius = 50m2
#   # convert to m3/ha: cm3/m2 / 100:
#   # (shrub volume(cm3) / plot area(m2)) x (1m3 / 1,000,000cm3) x (10,000m2 / 1ha) 
#   # also here, we account for the sub-sampling of some plots
#   shrubs[, AreaSearchM2 := ifelse(SubPlot == "A1", 100, 50)]
#   shrubs[, m3PerHa_fac := AreaSearchM2 * 100]
#   shrubs[, shrubVolHa := (Avail_Length * Diam1 * Diam2) / m3PerHa_fac]
#   
#   # Now deal with sub-samples of shrub species (when >10 individuals of a species)
#   # Calculate the mean vol of the sub-sample
#   
#   #Not accounting for unavailable forage, did not run this line of code: ** question for Joc **
#   #shrubs$Avail_Browse <- ifelse(is.na(shrubs$UF),shrubs$Foliage_Vol,shrubs$Foliage_Vol*(1-(shrubs$UF/100)))
#   PlotShrubVol <- shrubs[, .(shrubVolHa = ifelse(is.na(Total_Number),
#                                                  sum(shrubVolHa),
#                                                  mean(shrubVolHa) * Total_Number)),
#                          by = .(PlotID, SubPlot, Species, Total_Number)]
#   PlotShrubVol <- unique(PlotShrubVol)
#   PlotShrubVol <- PlotShrubVol[, .(shrubVolHa = sum(shrubVolHa)), by = .(PlotID, Species)]
#   PlotShrubVol <- PlotShrubVol[!is.na(shrubVolHa), ] # remove the NA rows
#   return(PlotShrubVol)
# }


shrubs <- fread("./01-data_inputs/FR_shrubVolumes.csv")


