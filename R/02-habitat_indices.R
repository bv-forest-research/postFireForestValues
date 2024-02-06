# Habitat Preliminary Analysis

# This script summarizes habitat metrics for each species and bins each component 0 or 1. Each metric is added together / species to create
# one habitat value / species

# Ingrid Farnell, Alana Clason, Erica Lilles, Jocelyn Biro
# Dec 12, 2023

# Load libraries
library(data.table)
library(tidyverse)
library(ggpubr)

# Load data
# Plot
FR_treatments <- fread("./01-data_inputs/FR_Treatments.csv") # has field plot assessed treatments and fire year
# decided to change FR08 to NP because while it was a plantation, it was burned at low severity and not planted after the fire.
setnames(FR_treatments, "ID", "PlotID")
FR_treatments[,TimeSinceFire := 2020 - FIRE_YEAR]

# Tree data:
A1trees <- fread("./01-data_inputs/A1trees.csv")
B1trees <- fread("./01-data_inputs/B1trees.csv")
Regen <- fread("./01-data_inputs/Regen.csv")

# Shrubs
densiometer <- fread("./01-data_inputs/FRdensiometer.csv")
Cover <- fread("./01-data_inputs/FRstrataCover.csv") #B1= <2m and B2=2-10m shrub heights)
setnames(Cover, c("Total_B1", "Total_B2"), c("ShrubsB1", "ShrubsB2"))
shrubVolume <- fread("./01-data_inputs/FR_shrubVolumes.csv")

# Woody debris:
cwd <- fread("./01-data_inputs/FireRehabData_CWD.csv")
fwd <- fread("./01-data_inputs/FireRehabData_FWD.csv")
line <- fread("./01-data_inputs/FR_LineTransect.csv")

# scale function
scale_fn <- function(var){(var - min(var)) / (max(var) - min(var))}


#-- Plot attributes
# CWD quality
source("./R/01-utils/cwdQualityIndexFunction.R")
PlotCQI <- cwdQI(cwd)
PlotCQI[, CQI:=round(CQI*0.01, digit=2)] #values between 0-100; 0=bad 100=best

# CWD piles
source("./R/01-utils/cwdPilesFunction.R")
PlotPiles <- cwdPiles(cwd)
PlotPiles[PlotID=="FR17" & Transect=="1", PileCount := 2] # notes on raw data says in 2 large piles
PlotPiles <- PlotPiles[,.(PileCount = sum(PileCount)), by = PlotID]

# CWD volume
source("./R/01-utils/cwdVolumeFunction.R")
PlotCWD <- cwdVol(cwd, line)

# Crown closure
# mean of densiometer readings and multiply by 1.04 to get canopy openness
PlotCrown <- densiometer[,.(SubPlotOpen=mean(Densiometer)), by = c("PlotID", "SubPlot")]
PlotCrown <- PlotCrown[,.(CrownOpen=mean(SubPlotOpen)*1.04), by = PlotID]
# convert openness to canopy closure
PlotCrown[, CrownClos:= 100-CrownOpen][, CrownOpen := NULL]
# ** what should we do with FR01 crown closure wasn't measured**

# Basal area
source("./R/01-utils/BasalAreaFunctions.R")
# live
PlotBAPHlive <- BAPHlive(A1trees, B1trees)
# dead
PlotBAPHdead <- BAPHdead(A1trees, B1trees)

# Tree density/sph
source("./R/01-utils/DensityFunctions.R")
# live
PlotTree <- TreeDensity(A1trees, B1trees)
# dead
PlotSnags <- SnagDensity(A1trees, B1trees)

# Individual shrub species percent cover (calculated from their volumes)
source("./R/01-utils/ForageVolumeFunctions.R")
PlotShrubCov <- ShrubSpCov(ShrubVolume)
# % cover of huckleberry berries
PlotHuckberry <- Huckberry(PlotShrubCov, PlotCrown)
PlotThermalForage <- ThermalForage(PlotShrubCov, PlotCrown)


#-- MARTEN
# 1) quality cwd (Godbout and Ouellet, 2010; Lofroth 1993; Wiebe et al., 2014)
# 2) >=30-80% coniferous crown closure (more than 80% not any better) (Bull et al. 2005; Bull and Heater, 2001; Godbout and Ouellet, 2010)
# 3) Higher basal area and large diameter snags (Bull et al. 2005; Lofroth 1993)
# 4) shrubby understory 20-60% (Lofroth 1993). Avoid <20 >80. Majority between 20-60

# 1. CWD quality -- using index from van Galen et al. 2019
PlotMarten <- merge(FR_treatments, PlotCQI)

# 2. Coniferous crown closure 50% - 75% (more than 75% not any better)
PlotMarten <- merge(PlotMarten, PlotCrown, all.x=TRUE)
# using gaussian model 0-30 = 0, increases from 30, peaks around 60, >80 = 0
PlotMarten[is.na(CrownClos), CrownClos := 0]
PlotMarten[, CrownClos := 1*exp(-0.5*(((CrownClos-60)/10)^2))]

#3. Basal area of snags > 20cm DBH
# Lofroth looks at basal area and diameter - if he found >20cm important and high basal area (is it ok to do basal area >20cm dbh?)
PlotMarten <- merge(PlotMarten, PlotBAPHdead[DBH > 20, .(snagBA = sum(BAPH)), by=PlotID], all.x = TRUE)
PlotMarten[is.na(snagBA), snagBA := 0]
# apply logistic function - after a certain amount doesn't increase, large diameter snags are better
PlotMarten[, snagBA := exp(0.6/(1+10*exp(-1*snagBA)))-0.95]

# 4. Shrub cover (B1= <2m and B2=2-10m shrub heights), B1 > between 20 - 60%
columnstoadd <- c("PlotID", "ShrubsB1")
PlotMarten[Cover, (columnstoadd) := mget(columnstoadd), on = "PlotID"]
PlotMarten[is.na(ShrubsB1), ShrubsB1 := 0]
# apply logistic function - between 20-60% is good
PlotMarten[, lowShrubs := 1*exp(-0.5*(((ShrubsB1-40)/8)^2))]

# MARTEN HABITAT INDEX
# create a dummy datset with really good and bad marten habitat
dummy <- data.table(
  PlotID = c("FR01", "FR02"),
  CrownClos = c(60, 100),
  CQI = c(1, 0.2),
  ShrubsB1 = c(40, 10),
  snagBA = c(100, 0)
)
dummy[, CrownClos := 1*exp(-0.5*(((CrownClos-60)/10)^2))]
dummy[, snagBA := exp(0.6/(1+10*exp(-1*snagBA)))-0.95]
dummy[, lowShrubs := 1*exp(-0.5*(((ShrubsB1-40)/8)^2))]
dummy[, MartenHabitat := sum(1*(CrownClos), 1*(CQI), 0.6*(lowShrubs), 0.5*(snagBA)), by=PlotID]

# 1 * Crown closure (Bull et al. 2005; Bull and Heater, 2001; Godbout and Ouellet, 2010)
# 1 * CQI (Godbout and Ouellet, 2010; Mclaren et al., 2013; Sullivan ..)
# 0.6 * shrubsB1 (Lofroth 1993; Mclaren et al., 2013)
# 0.5 * snags (Lofroth 1993)
PlotMarten[, MartenHabitat := round(sum(1*(CrownClos), 1*(CQI), 0.6*(lowShrubs), 0.5*(snagBA)), digits = 2), by=PlotID]

#example of how to write the function similar to the Monkennen appendix
#w_age_fn <- function(age){
#  if(age < 20){
#    w_age <- 0
#  }else if(age > 20 & age <= 30){
#    w_age <- (0.1*age) - 2
#  }else if(age > 30 * age <= 60){
#    w_age <- 1
#  }else{
#    w_age <- -0.012*age + 1.72
#  }
#}

#-- FISHER ** needs to be adjusted so less 0 or 1s
# 1) quality CWD (movement)
# 2) large diseased and decaying aspen and cottonwood (>30 cm dbh) (resting, rearing)
# 3) Ac leading or secondary or tertiary or Sx as only species (resting, rearing)
# 4) QMD >=19.6 (rearing, resting)
# 5) Crown closure >=30% (resting, rearing, forage)
# 6) Large Sx >39 cm (more likely to have broom rusts for resting) 

# all metrics are from www.bcfisherhabitat.ca

# 1. CWD quality
PlotFisher <- merge(FR_treatments, PlotCQI)

# 2. large diseased and decaying aspen and cottonwood (>30 cm dbh)
# we sampled mesic sites, which isn't normally where large At/Ac grow.. should we remove this?
# filter to AC or At and > 30 cm dbh
LgAtAc <- PlotTree[Species %in% c("Ac", "At") & DBH_bin>=30,]
LgAtAc <- LgAtAc[,.(lgAtSPH=sum(SPH)), by=PlotID]

columnstoadd <- c("PlotID", "lgAtSPH")
PlotFisher[LgAtAc, (columnstoadd) := mget(columnstoadd), on = "PlotID"]
PlotFisher[, lgAtSPH := ifelse(lgAtSPH > 0, 1, 0)][is.na(lgAtSPH), lgAtSPH := 0]

# 3. At/Ac leading or secondary or Sx as only species
DecidLead <- PlotTree[DBH_bin >=10]
DecidLead <- DecidLead[,. (SPH=sum(SPH)), by=c("PlotID", "Species")]
# calculate rank of SPH for primary/secondary At, Ac and Sx
DecidLead[, Rank := frank(-SPH, ties.method = "min"), by = .(PlotID)]
# set decidSx if deciduous primary/secondary or Sx primary
DecidLead <- DecidLead[, decidSx := ifelse(
  (Species == "At" & Rank %in% c(1, 2)) |
    (Species == "Ac" & Rank %in% c(1, 2)) |
    (length(unique(Species)) == 1 && first(Species) == "Sx"), # Sx is only species
    SPH, 0), by = PlotID]
DecidLead <- DecidLead[,. (decidSx=sum(decidSx)), by= PlotID]
PlotFisher <- merge(PlotFisher, DecidLead, all.x=TRUE)
PlotFisher[, decidSx := ifelse(decidSx > 0, 1, 0)][is.na(decidSx), decidSx := 0] # deciduous leading or secondary or 100% Sx = 1, else 0

# 4. QMD >=19.6 (quadratic mean diameter)
QMD <- B1trees[,. (QMD=sqrt(sum(DBH^2)/.N)), by = PlotID]
PlotFisher <- merge(PlotFisher, QMD, all.x=TRUE)
PlotFisher[, QMD := ifelse(QMD >=19.6, 1, 0)][is.na(QMD), QMD := 0] # QMD >= 19.6 = 1, else 0

# 5. Crown closure >=30%
PlotFisher<- merge(PlotFisher, PlotCrown, all.x=TRUE)
PlotFisher[, CrownClos := ifelse(CrownClos >=30, 1, 0)] # crown closure >= 30% = 1, else 0
PlotFisher[is.na(CrownClos), CrownClos := 0]

# 6. Sx >39 cm DBH # not working still need to figure out why
PlotFisher <- merge(PlotFisher, PlotTree[Species == "Sx" & DBH_bin>=35, .(lgSx = sum(SPH)), by = PlotID], all.x = TRUE)

# FISHER HABITAT INDEX
PlotFisher[, FisherHabitat := sum(CQI, lgAtSPH, decidSx, QMD, CrownClos, lgSx), by=PlotID]


#-- GOSHAWK ** still needs to be adjusted so less 0 or 1s
# 1) large >30cm dbh snag retention 
# 2) large >30cm dbh live tree retention, 
# 3) large CWD, >30cm diameter 
# 4) clumped/piled CWD
# metrics are from a conversation Ingrid had with Frank Doyle Oct 6, 2023

# 1. large snag >30cm >12/ha retention post fire
PlotGoshawk <- merge(FR_treatments, PlotSnags[DBH_bin >= 30, .(snagSPH=sum(snagSPH)), by=PlotID], all.x=TRUE)
PlotGoshawk[, snagSPH := ifelse(snagSPH > 12, 1, 0)][is.na(snagSPH), snagSPH := 0] # snags >=30cm dbh present =1, else 0

# 2. large diameter >30cm > 12/ha live trees
PlotGoshawk <- merge(PlotGoshawk, PlotTree[DBH_bin >= 30, .(liveSPH=sum(SPH)), by=PlotID], all.x=TRUE)
PlotGoshawk[, liveSPH := ifelse(liveSPH > 12, 1, 0)][is.na(liveSPH), liveSPH := 0] # live trees >=30cm dbh present = 1, else 0

# 3. quality CWD
PlotGoshawk <- merge(PlotGoshawk, PlotCQI)

# 4. CWD piles
PlotGoshawk <- merge(PlotGoshawk, PlotPiles, all.x=TRUE)
PlotGoshawk[, PileCount := ifelse(PileCount > 0, 1, 0)][is.na(PileCount), PileCount := 0] # if plot has cwd piles = 1, else 0

# GOSHAWK HABITAT INDEX
PlotGoshawk[, GoshawkHabitat := sum((1*snagSPH), (1*liveSPH), (0.8*CQI), (0.7*PileCount)), by=PlotID]


#-- SNOWSHOE HARE
# 1) Higher canopy cover higher hare abundance (Cheng et al. 2015) between 40&60 St-Laurent et al. 2008
# 2) shrub cover (0-2m) increase slowly with increase cover up to 80% (Cheng et al. 2015) 
# 3) tree density >0 peaks around 3000 stems/ha (Thomas et al. 2019), (Kelly & Hodges, 2020)
# 4) abundant CWD (Berg et al., 2012), and 
# 5) available shrub forage * thinking of taking this out since cover seems to trump forage

# 1. High canopy cover
# <30 = 0, >30 exponential increase 
PlotHare <- merge(FR_treatments, PlotCrown, all.x = TRUE)
PlotHare[is.na(CrownClos), CrownClos := 0]
PlotHare[, CrownClos := ifelse(CrownClos >30, 0.1*1.1^(0.22*CrownClos), 0)]

# 2. Shrub cover B1 increase until 80% then decline
# gaussian function
columnstoadd <- c("PlotID", "ShrubsB1")
PlotHare[Cover, (columnstoadd) := mget(columnstoadd), on = "PlotID"]
PlotHare[is.na(ShrubsB1), ShrubsB1 := 0]
PlotHare[, lowShrubs := 1.1*exp(-0.5*(((ShrubsB1-75)/50)^2))-0.2] # shrubs (0-2m) >= 30% cover = 1, else 0

# 3. Tree density >=3000 stems/ha - asymptots ~3000
PlotHare <- merge(PlotHare, PlotTree[,. (SPH=sum(SPH)), by=c("PlotID")], all.x=TRUE)
PlotHare[is.na(SPH), SPH := 0]
PlotHare[, SPH := ifelse(SPH > 0, round(1.45/(1+exp(-0.0015*SPH))-0.5, digits = 2), 0)]

# 4. Abundant CWD
# if between 50-200 = 1, else 0
PlotHare <- merge(PlotHare, PlotCWD[,. (CWDvol=sum(VolHa)), by=c("PlotID")], all.x=TRUE)
PlotHare[, CWDvol := ifelse(CWDvol > 50 & CWDvol <200 , 1, 0)]

# 5. Available forage * not using.. I think
# source("./R/01-utils/ForageVolumeFunctions.R")
# PlotShrubVol <- ShrubVol(shrubVolume)
# PlotForageHare <- HareForageVol(PlotShrubVol)

# SNOWSHOE HARE HABITAT INDEX 
PlotHare[, HareHabitat := round(sum(1*CrownClos, 1*SPH, 0.8*lowShrubs, 0.6*CWDvol), digits = 2), by=PlotID]


#-- RED SQUIRRELS
# 1) large tree density (Kelly & Hodges, 2020; Russel et al. 2010)
# 2) tree composition 50% conifer (Fisher and Bradbury, 2006),
# 3) abundant cwd >=100m3 (Bakker 2006; Russel et al. 2010)

# 1. Large tree density >23 cm dbh
# logistic function replicating Kelly & Hodges 2020
PlotSquirrel <- merge(FR_treatments, PlotTree[DBH_bin >= 20, .(SPH = sum(SPH)), by=PlotID], all.x = TRUE)
PlotSquirrel[is.na(SPH), SPH := 0]
PlotSquirrel[, SPH := 1.8/(1+exp(-0.005*SPH))-0.9]

# 2. Tree composition at least 50% conifer
treeComp <- PlotTree[DBH_bin >=10, totalSPH := sum(SPH), by=c("PlotID")]
treeComp <- treeComp[DBH_bin >=10, .(spComp=sum(SPH)/totalSPH), by=c("PlotID", "Species")]
treeComp <- unique(treeComp)
PlotSquirrel <- merge(PlotSquirrel, treeComp[Species %in% c("Sx", "Pl", "Bl"), 
                                             .(conComp=sum(spComp)), by=PlotID], all.x=TRUE)
PlotSquirrel[, conComp := ifelse(conComp >= 0.5, 1, 0)][is.na(conComp), conComp := 0] # tree composition >=50% conifer = 1, else 0

# 3. abundant CWD >100m3
PlotSquirrel <- merge(PlotSquirrel, PlotCWD[,.(CWDvol=sum(VolHa)), by=PlotID], all.x = TRUE)
PlotSquirrel[, CWDvol := 1-exp(-0.0045*CWDvol)]

# #. Tree canopy cover >=30% - not using
# PlotSquirrel <- merge(FR_treatments, PlotCrown, all.x = TRUE)
# PlotSquirrel[, CrownClos := ifelse(CrownClos >=30, 1, 0)] # crown closure >= 30% = 1, else 0
# PlotSquirrel[is.na(CrownClos), CrownClos := 0][, CrownOpen := NULL]

# RED SQUIRREL HABITAT INDEX
PlotSquirrel[, SquirrelHabitat := round(sum(SPH, 0.75*conComp, 0.6*CWDvol), digits = 2), by=PlotID]


#-- SMALL MAMMALS (deer mice, southern red backed voles)
# 1) abundant (>50m3/ha) decayed (class 4&5) CWD (Fauteaux et al., 2012; Fauteux et al., 2013), and
# 2) live tree  (>9 cm dbh) basal area >= 15 m2/ha(Fauteux., 2013; Sullivan and Sullivan, 2002)

# 1. >50m3/ha cwd volume decay class 4 & 5
PlotSmMammal <- merge(FR_treatments, PlotCWD[Decay_class >=4, .(CWDvol=sum(VolHa)), by=PlotID], all.x=TRUE)
PlotSmMammal[, CWDvol := ifelse(CWDvol>= 50, 1, 0)][is.na(CWDvol), CWDvol := 0] # decayed (classes 4 &5) cwd >= 50 m3/ha = 1, else 0

# 2. Tree basal area (>9 cm dbh)
PlotSmMammal <- merge(PlotSmMammal, PlotBAPHlive[DBH >= 9, .(BAPH=sum(BAPH)), by=PlotID], all.x=TRUE)
PlotSmMammal[, BAPH := ifelse(BAPH >= 15 , 1, 0)][is.na(BAPH), BAPH := 0]

# SMALL MAMMAL HABITAT INDEX
PlotSmMammal[, SmMammalHabitat := sum(CWDvol, BAPH), by=PlotID]

# CWD late decay (class 3 & 4 Fauteux et al. 2012)
PlotCWD <- cwdVol(cwd, line)
# Voles
SmMammals <- PlotCWD[Decay_class >= 3, .(LateCWDvol= sum(VolHa)), by=c("PlotID")] # maybe take out class 5 if others also don't include it
SmMammals <- merge(FR_treatments, SmMammals, all.x=TRUE)
SmMammals[is.na(LateCWDvol), LateCWDvol:= 0]

# apply logistic equaiton to late decay cwd
SmMammals[, voleLateCWD := exp(0.5/(1+8*exp(-0.3*LateCWDvol/10)))-0.75]

plot(x=LateCWDvol$TimeSinceFire, y=LateCWDvol$voleLateCWD)

# Deer mice late decay
SmMammals[, dMiceLateCWD := exp(0.6/(1+9*exp(-0.18*LateCWDvol)))-1]

# voles and deer mice average together


# TREES *needs more work, Quebec paper only goes to 20 baph, we max at 40
PlotBAPH <- BAPH(A1trees, B1trees)
# Voles
voles.tree <- PlotBAPH[DBH >= 10, .(BA = sum(BAPH)), by = c("PlotID")]
voles.tree <- merge(FR_treatments, voles.tree, all.x = TRUE)
voles.tree[is.na(BA), BA := 0]

plot(x=voles.tree$TimeSinceFire, y=voles.tree$BA)


#-- GROUSE, SPRUCE & RUFFED
# 1) canopy cover >60% (Anich 2013; Casabona et al. 2021),
# 2) % conifer species >80% (Anich 2013) (spruce) OR deciduous density >4,900 & <14,800 sph, after 21,000 = 0 (Cade and Sousa 1985) (ruffed)
# 3) shrub (1-2m) cover >20% (Anich 2013; Casabona et al. 2021)

# 1. Canopy cover 
# linear increase 0-75%, >75% = 1
PlotGrouse <- merge(FR_treatments, PlotCrown, all.x = TRUE)
PlotGrouse[is.na(CrownClos), CrownClos := 0]
PlotGrouse[, CrownClos := ifelse(CrownClos <75, 0.0133*CrownClos+0, 1)]

# 2. % conifer species >=80% (spruce grouse) OR deciduous density >4,900 <14,800 sph (ruffed grouse)
treeComp <- PlotTree[, totalSPH := sum(SPH), by=c("PlotID")]
treeComp <- PlotTree[, .(spComp=sum(SPH)/totalSPH), by=c("PlotID", "Species")]
treeComp <- unique(treeComp)
# conifer
PlotGrouse <- merge(PlotGrouse, treeComp[Species %in% c("Sx", "Pl", "Bl"),
                                         .(conComp=sum(spComp)), by=PlotID], all.x=TRUE)
# deciduous
PlotGrouse <- merge(PlotGrouse, PlotTree[Species %in% c("At", "Ac", "Ep"), 
                                         .(decSPH=sum(SPH)), by=PlotID], all.x=TRUE) 
PlotGrouse[, Comp := ifelse(conComp >= 0.8 | (decSPH > 4900 & decSPH < 14800), 1, 0)][is.na(Comp), Comp := 0] # tree composition >=80% conifer = 1, else 0

# 3. shrub (1-2m) cover
# linear increase 0-25%, >25% = 1
columnstoadd <- c("PlotID", "ShrubsB1")
PlotGrouse[Cover, (columnstoadd) := mget(columnstoadd), on = "PlotID"]
PlotGrouse[is.na(ShrubsB1), ShrubsB1 := 0]
PlotGrouse[, ShrubsB1 := ifelse(ShrubsB1 < 25, 0.04*ShrubsB1+0, 1)]

# GROUSE HABITAT INDEX
PlotGrouse[, GrouseHabitat := sum(CrownClos, Comp, ShrubsB1), by=PlotID]


#-- GRIZZLY BEAR
# 1) CWD quality as a metric for ants (Frank et al., 2015; Higgins et al., 2017)
# 2) Available forage (grass, ferns, fireweed, huckleberry (Nielson et al. 2004))
# 3) Thermal cover with shrub berries (Pigeon et al. 2016)

# 1. CWD quality index
PlotGrizzly <- merge(FR_treatments, PlotCQI)

# 2. Available forage (grass, ferns, fireweed, huckleberries)
# FR01 and FR02 have no data - should these plots be dropped?
columnstoadd <- c("PlotID", "Graminoids", "Ferns", "CHAMANG")
PlotGrizzly[Cover, (columnstoadd) := mget(columnstoadd), on = "PlotID"]
columnstoadd <- c("PlotID", "HuckCov")
PlotGrizzly[PlotHuckberry, (columnstoadd) := mget(columnstoadd), on = "PlotID"]
# Add all available forage & weight huckleberry X2
PlotGrizzly[, ForageCov := (sum(Graminoids, Ferns, CHAMANG, HuckCov*2))*0.01, by = PlotID]

# 2. Thermal cover with forage
PlotGrizzly <- merge(PlotGrizzly, PlotThermalForage, all.x = TRUE)
PlotGrizzly[, ThermForage := ThermForage*0.01]
PlotGrizzly[is.na(PlotGrizzly)] <- 0

# GRIZZLY BEAR HABITAT INDEX
PlotGrizzly[, GrizzlyHabitat := sum(CQI, ForageCov, ThermForage), by = PlotID]

#-- BIRDS * am I doing a habitat index for each of the species or merging into one?
# 1) Shrub-associates: 
# shrub cover <56% 0.01*cover+0.044, if >56 = 1
# 2) Snag-associates
# snags >30cm dbh = snag SPH/1.02+SPH
# 3) Conifer forest species: 
# mature conifers >30cm DBH = (0.5/(1 + 2000 * e^-0.017* SPH)) + (0.5*SPH/50 + SPH)
# 4) Open-forest species:
# conifers <5 = 0, conifers > 5, 0.5*e^-0.5*(-(ln(conifersSPH?-2.4))^2 + 0.5*e^-0.5*(-(ln(conifersSPH?-3.5))^2))
# 5) Forest-edge species:
# conifer sph between 1-800, -0.24* ln(SPH) + 0.137 * ln(SPH)^2 -0.052 * ln(SPH)^3
# deciduous sph between 1-800, -0.24 * ln(SPH) +0.137* ln(SPH)^2 - 0.052*ln(SPH)^3
# decidous snags >30cm dbh, 0.15 * sngasSPH? / 1.02 * snagsSPH?
# then add up

#-- ALL SPECIES 
HabitatIndices <- FR_treatments[PlotMarten, ("MartenHabitat") := mget("MartenHabitat"), on = "PlotID"]
#HabitatIndices <- HabitatIndices[PlotFisher, ("FisherHabitat") := mget("FisherHabitat"), on = "PlotID"]
HabitatIndices <- HabitatIndices[PlotGoshawk, ("GoshawkHabitat") := mget("GoshawkHabitat"), on = "PlotID"]
HabitatIndices <- HabitatIndices[PlotHare, ("HareHabitat") := mget("HareHabitat"), on = "PlotID"]
HabitatIndices <- HabitatIndices[PlotSquirrel, ("SquirrelHabitat") := mget("SquirrelHabitat"), on = "PlotID"]
#HabitatIndices <- HabitatIndices[PlotSmMammal, ("SmMammalHabitat") := mget("SmMammalHabitat"), on = "PlotID"]
HabitatIndices <- HabitatIndices[PlotGrouse, ("GrouseHabitat") := mget("GrouseHabitat"), on = "PlotID"]
HabitatIndices <- HabitatIndices[PlotGrizzly, ("GrizzlyHabitat") := mget("GrizzlyHabitat"), on = "PlotID"]

# export 
#write.csv(HabitatIndices, "./02-prepped_values/HabitatIndices.csv")



#-----------------------
#-- Preliminary Figures

m <- ggplot(HabitatIndicies, aes(x = TimeSinceFire)) +
  geom_smooth(aes(y = MartenHabitat, color = Planted))
f <- ggplot(HabitatIndicies, aes(x = TimeSinceFire)) +
  geom_smooth(aes(y = FisherHabitat, color = Planted))
g <- ggplot(HabitatIndicies, aes(x = TimeSinceFire)) +
  geom_smooth(aes(y = GoshawkHabitat, color = Planted))
sm <- ggplot(HabitatIndicies, aes(x = TimeSinceFire)) +
  geom_smooth(aes(y = SmMammalHabitat, color = Planted))
s <- ggplot(HabitatIndicies, aes(x = TimeSinceFire)) +
  geom_smooth(aes(y = SquirrelHabitat, color = Planted))
gr <- ggplot(HabitatIndicies, aes(x = TimeSinceFire)) +
  geom_smooth(aes(y = GrouseHabitat, color = Planted))

plot <- ggarrange(m, f, g, sm, s, gr,
                   ncol = 1)
plot

#alana modified -----------------------------------------------------------------------
# change the layout
#update so all indices are 0-1
hab_ind <- melt(HabitatIndices, id.vars = c("PlotID","Planted","TimeSinceFire"),
                measure.vars = c("MartenHabitat", "GoshawkHabitat", 
                                 "HareHabitat", "SquirrelHabitat",
                                 "GrouseHabitat", "GrizzlyHabitat"))
setnames(hab_ind, c("variable","value"), c("species","habitat_index"))





ggplot(data= hab_ind)+
  geom_point(aes(x = TimeSinceFire, y = hab_ind_sc, colour = species))+
  geom_smooth(aes(x = TimeSinceFire, y = hab_ind_sc, colour = species), alpha = 0.2)+
  labs(color = "Wildlife species")+
  scale_color_manual(labels = c("Marten", "Goshawk", "Hare", "Squirrel",
                                "Grouse", "Grizzly"), 
                     values = custom_color_scale)+
  xlab("Time since fire")+
  ylab("Habitat index")+
  facet_wrap(~Planted)
  
 
  
