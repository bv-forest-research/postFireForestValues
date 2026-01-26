# Habitat Preliminary Analysis

# This script summarizes habitat metrics for each species and bins each component 0 or 1. Each metric is added together / species to create
# one habitat value / species

# Ingrid Farnell, Alana Clason, Erica Lilles, Jocelyn Biro
# Dec 12, 2023

# Load libraries
library(data.table)
library(tidyverse)
#library(ggpubr)


in_dir <- "01-data_inputs"
out_dir <- "02_prepped_values"

files_to_source <- list.files("./R/00-utils/", pattern = "Function", 
                              full.names = TRUE)
sapply(files_to_source, source)

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


#-- Plot attributes
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
# dead
PlotBAPHdead <- BAPHdead(A1trees, B1trees)

# Tree density/sph
# live
PlotTree <- TreeDensity(A1trees, B1trees)
# dead
PlotSnags <- SnagDensity(A1trees, B1trees)

# Individual shrub species percent cover (calculated from their volumes)
PlotShrubCov <- ShrubSpCov(ShrubVolume)
# % cover of huckleberry berries
PlotHuckberry <- Huckberry(PlotShrubCov, PlotCrown)
PlotThermalForage <- ThermalForage(PlotShrubCov, PlotCrown)


#-- MARTEN
# 1) quality cwd (Godbout and Ouellet, 2010; Lofroth 1993; Wiebe et al., 2014)
# 2) >=30-80% crown closure (more than 80% not any better) (Bull et al. 2005; Godbout and Ouellet, 2010)
# 3) Higher basal area and large diameter snags (Bull et al. 2005; Lofroth 1993)
# 4) shrubby understory 20-60% (Lofroth 1993. Avoid <20 >80. Majority between 20-60

# 1. CWD quality -- using index from van Galen et al. 2019
PlotMarten <- merge(FR_treatments, PlotCQI)

# 2. Crown closure 50% - 75% (more than 75% not any better)
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
# 1 * Crown closure (Bull et al. 2005; Bull and Heater, 2001; Godbout and Ouellet, 2010)
# 1 * CQI (Godbout and Ouellet, 2010; Mclaren et al., 2013; Sullivan ..)
# 0.6 * shrubsB1 (Lofroth 1993; Mclaren et al., 2013)
# 0.5 * snags (Lofroth 1993)
PlotMarten[, MartenHabitat := round(sum(1*(CrownClos), 1*(CQI), 0.6*(lowShrubs), 
                                        0.5*(snagBA)), digits = 2), by=PlotID]


#-- FISHER
# 1) quality CWD (movement)
# 2) large diseased and decaying aspen and cottonwood (hollow), 
# and large Sx (more likely to have broom rusts) (>30 cm dbh) (resting, rearing)
# 3) QMD >=19.6 (rearing, resting)
# 4) Crown closure 30-60% (resting, rearing, forage) (Proulx 2006)
# 5) shrub cover > 20% (prey habitat) (Proulx 2006)

# all metrics are from www.bcfisherhabitat.ca

# 1. CWD quality
PlotFisher <- merge(FR_treatments, PlotCQI)

# 2. large diseased and decaying aspen, cottonwood, and Sx (>30 cm dbh)
# we sampled mesic sites, which isn't normally where large At/Ac grow
# filter to AC or At or Sx and > 30 cm dbh
LgAtAcSx <- PlotTree[Species %in% c("Ac", "At", "Sx") & DBH_bin>=30,]
LgAtAcSx <- LgAtAcSx[,.(lgAtAcSxSPH=sum(SPH)), by=PlotID]

columnstoadd <- c("PlotID", "lgAtAcSxSPH")
PlotFisher[LgAtAcSx, (columnstoadd) := mget(columnstoadd), on = "PlotID"]
PlotFisher[, lgAtAcSxSPH := ifelse(lgAtAcSxSPH > 0, 1, 0)][is.na(lgAtAcSxSPH), lgAtAcSxSPH := 0]

# 3. QMD >=19.6 (quadratic mean diameter)
# linear increase with higher QMD
QMD <- B1trees[,. (QMD=sqrt(sum(DBH^2)/.N)), by = PlotID]
PlotFisher <- merge(PlotFisher, QMD, all.x=TRUE)
PlotFisher[is.na(QMD), QMD := 0]
PlotFisher[, QMD := ifelse(QMD >=19.6, 0.025*QMD+0, 0)]

# 4. Crown closure >=30%, decline after 60
PlotFisher<- merge(PlotFisher, PlotCrown, all.x=TRUE)
PlotFisher[is.na(CrownClos), CrownClos := 0]
PlotFisher[, CrownClos := ifelse(CrownClos >= 30, 1.1*exp(-0.5*(((CrownClos-60)/30)^2))-0.2, 0)]

# 5. Shrub cover > 20%
# gaussian function (same as snowshoe hare)
columnstoadd <- c("PlotID", "ShrubsB1")
PlotFisher[Cover, (columnstoadd) := mget(columnstoadd), on = "PlotID"]
PlotFisher[is.na(ShrubsB1), ShrubsB1 := 0]
PlotFisher[, lowShrubs := ifelse(ShrubsB1 >= 20, 1.1*exp(-0.5*(((ShrubsB1-75)/50)^2))-0.2, 0)]

# FISHER HABITAT INDEX
# 1 * CQI
# 1 * large At, Ac, Sx
# 1 * QMD
# 1 * crown closure
# 1 * shrub cover

PlotFisher[, FisherHabitat := round(sum(1*CQI, 1*lgAtAcSxSPH, 1*QMD, 
                                        1*CrownClos, 1*lowShrubs), digits = 2), by=PlotID]


#-- GOSHAWK
# 1) retention - large >30cm dbh snag and/or live trees
# 2) quality cwd
# 3) clumped/piled CWD
# 4) shrubs, prey habitat 
# metrics are from a conversation Ingrid had with Frank Doyle Oct 6, 2023 and from best management practices doc

# 1. Retention - large snag and/or live trees >30cm >12/ha retention post fire
PlotGoshawk <- merge(FR_treatments, PlotSnags[DBH_bin >= 30, .(snagSPH=sum(snagSPH)), by=PlotID], all.x=TRUE)
PlotGoshawk <- merge(PlotGoshawk, PlotTree[DBH_bin >= 30, .(liveSPH=sum(SPH)), by=PlotID], all.x=TRUE)
PlotGoshawk[is.na(liveSPH), liveSPH := 0][is.na(snagSPH), snagSPH := 0]
PlotGoshawk[, retention := sum(liveSPH, snagSPH), by = PlotID]
PlotGoshawk[, retention := ifelse(retention > 12, 0.003*retention+0, 0)] # linear increase

# 2. quality CWD
PlotGoshawk <- merge(PlotGoshawk, PlotCQI)

# 3. CWD piles
# MM increase
PlotGoshawk <- merge(PlotGoshawk, PlotPiles, all.x=TRUE)
PlotGoshawk[is.na(PileCount), PileCount := 0]
PlotGoshawk[, PileCount := ifelse(PileCount > 0, ((1*PileCount)/((2/1)+PileCount)), 0)]

# 4. Shrub cover for grouse
# linear increase 0-25%, >25% = 1
columnstoadd <- c("PlotID", "ShrubsB1")
PlotGoshawk[Cover, (columnstoadd) := mget(columnstoadd), on = "PlotID"]
PlotGoshawk[is.na(ShrubsB1), ShrubsB1 := 0]
PlotGoshawk[, ShrubsB1 := ifelse(ShrubsB1 < 25, 0.04*ShrubsB1+0, 1)]

# GOSHAWK HABITAT INDEX
# 2*retention - the most important factor, if no structure no goshawks
PlotGoshawk[, GoshawkHabitat := round(sum(2*retention, 0.8*CQI, 0.7*PileCount, 0.8*ShrubsB1), digits = 2), by=PlotID]


#-- SNOWSHOE HARE
# 1) Higher canopy cover higher hare abundance (Cheng et al. 2015) 
# 2) shrub cover (0-2m) increase slowly with increase cover up to 80% (Cheng et al. 2015) 
# 3) tree density >0 peaks around 3000 stems/ha (Thomas et al. 2019; Kelly & Hodges, 2020)
# 4) abundant CWD (Berg et al., 2012)

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
PlotHare[, lowShrubs := 1.1*exp(-0.5*(((ShrubsB1-75)/50)^2))-0.2]

# 3. Tree density >=3000 stems/ha - asymptots ~3000
PlotHare <- merge(PlotHare, PlotTree[,. (SPH=sum(SPH)), by=c("PlotID")], all.x=TRUE)
PlotHare[is.na(SPH), SPH := 0]
PlotHare[, SPH := ifelse(SPH > 0, (1.45/(1+exp(-0.0015*SPH)))-0.5, 0)]

# 4. Abundant CWD
# if between 50-200 = 1, else 0
PlotHare <- merge(PlotHare, PlotCWD[,. (CWDvol=sum(VolHa)), by=c("PlotID")], all.x=TRUE)
PlotHare[, CWDvol := ifelse(CWDvol > 50 & CWDvol <200 , 1, 0)]

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

# RED SQUIRREL HABITAT INDEX
PlotSquirrel[, SquirrelHabitat := round(sum(1*SPH, 0.75*conComp, 0.6*CWDvol), digits = 2), by=PlotID]


#-- SMALL MAMMALS (deer mice (Peromyscus maniculatus), southern red backed voles (C. gapperi))
# 1) abundant (>50m3/ha) decayed (class 4&5) CWD (Fauteaux et al., 2012; Fauteux et al., 2013), and
# 2) live tree  (>9 cm dbh) basal area >= 15 m2/ha (Fauteux., 2013; Sullivan and Sullivan, 2002)

# 1. CWD late decay (class 3 & 4 Fauteux et al. 2012)
PlotSmMammal <- merge(FR_treatments, PlotCWD[Decay_class >= 3, .(LateCWDvol= sum(VolHa)), by=c("PlotID")], all.x = TRUE) # maybe take out class 5 if others also don't include it
PlotSmMammal[is.na(LateCWDvol), LateCWDvol:= 0]
# Voles
# apply logistic equation to late decay cwd
PlotSmMammal[, voleLateCWD := exp(0.5/(1+8*exp(-0.3*LateCWDvol)))-0.75]
# Deer mice
PlotSmMammal[, dMiceLateCWD := exp(0.6/(1+9*exp(-0.18*LateCWDvol)))-1]
# voles and deer mice average together
PlotSmMammal[, LateCWD := rowMeans(.SD), by = PlotID, .SDcols = c("voleLateCWD", "dMiceLateCWD")]

# 2. Basal area
PlotSmMammal <- merge(PlotSmMammal, PlotBAPHlive[DBH >= 10, .(BA = sum(BAPH)), by = c("PlotID")], all.x = TRUE)
PlotSmMammal[is.na(BA), BA := 0]
PlotSmMammal[, BA := ifelse(BA >= 15, 0.02*BA+0, 0)]

# SMALL MAMMAL HABITAT INDEX
PlotSmMammal[, SmMammalHabitat := round(sum(LateCWD, BA), digits = 2), by = PlotID]


#-- GROUSE, SPRUCE & RUFFED
# 1) canopy cover >60% (Anich 2013; Casabona et al. 2022),
# 2) % conifer species >80% (Anich 2013) (spruce)
# 3) deciduous density >4,900 & <14,800 sph, after 21,000 = 0 (Cade and Sousa 1985) (ruffed)
# 4) shrub (1-2m) cover >20% (Anich 2013; Casabona et al. 2021)

# 1. Canopy cover 
# linear increase 0-75%, >75% = 1
PlotGrouse <- merge(FR_treatments, PlotCrown, all.x = TRUE)
PlotGrouse[is.na(CrownClos), CrownClos := 0]
PlotGrouse[, CrownClos := ifelse(CrownClos <75, 0.0133*CrownClos+0, 1)]

# 2. Conifer species >=80% & <1000 SPH (spruce grouse) OR deciduous 
treeComp <- PlotTree[, totalSPH := sum(SPH), by=c("PlotID")]
treeComp <- PlotTree[, .(spComp=sum(SPH)/totalSPH), by=c("PlotID", "Species")]
treeComp <- unique(treeComp)
PlotGrouse <- merge(PlotGrouse, treeComp[Species %in% c("Sx", "Pl", "Bl"),
                                         .(conComp=sum(spComp)), by=PlotID], all.x=TRUE)
QMD <- B1trees[,. (QMD=sqrt(sum(DBH^2)/.N)), by = PlotID]
PlotGrouse <- merge(PlotGrouse, QMD, all.x = TRUE)
PlotGrouse[is.na(PlotGrouse)] <- 0
PlotGrouse[, conifer := ifelse(conComp >= 0.8, 1*exp(-0.5*(((QMD-12)/4)^2)), 0)]

# 3. Deciduous species, density >4,900 <14,800 sph (ruffed grouse)
PlotGrouse <- merge(PlotGrouse, PlotTree[Species %in% c("At", "Ac", "Ep"), 
                                         .(decSPH=sum(SPH)), by=PlotID], all.x=TRUE)
PlotGrouse[is.na(decSPH), decSPH := 0]
PlotGrouse[, deciduous := ifelse(decSPH > 4900 & decSPH < 14800, 1, 0)] 

# 4. shrub (1-2m) cover
# linear increase 0-25%, >25% = 1
columnstoadd <- c("PlotID", "ShrubsB1")
PlotGrouse[Cover, (columnstoadd) := mget(columnstoadd), on = "PlotID"]
PlotGrouse[is.na(ShrubsB1), ShrubsB1 := 0]
PlotGrouse[, ShrubsB1 := ifelse(ShrubsB1 < 25, 0.04*ShrubsB1+0, 1)]

# GROUSE HABITAT INDEX
PlotGrouse[, GrouseHabitat := round(sum(CrownClos, conifer, deciduous, ShrubsB1), digits = 2), by=PlotID]


#-- GRIZZLY BEAR
# 1) CWD quality and seral age as a metric for ants (Frank et al., 2015; Higgins et al., 2017)
# 2) Available forage (grass, ferns, fireweed) (Karine) * consider adding a seral age component for ant presence (Higgins et al. 2017)
# 3) Available huckleberries (Nielson et al. 2004)
# 4) Thermal cover with shrub berries (Pigeon et al. 2016)

# 1. CWD quality index & seral age
PlotGrizzly <- merge(FR_treatments, PlotCQI)
PlotGrizzly[, Ants := ifelse(TimeSinceFire > 2, CQI*(1*exp(-0.5*(((TimeSinceFire-15)/9)^2))), 0)]

# 2. Available forage (grass, ferns, fireweed)
# FR01 and FR02 have no data - should these plots be dropped?
columnstoadd <- c("PlotID", "Graminoids", "Ferns", "CHAMANG")
PlotGrizzly[Cover, (columnstoadd) := mget(columnstoadd), on = "PlotID"]
# Add all available forage
PlotGrizzly[, ForageCov := (sum(Graminoids, Ferns, CHAMANG))*0.01, by = PlotID]

# 3. Available huckleberries
columnstoadd <- c("PlotID", "HuckCov")
PlotGrizzly[PlotHuckberry, (columnstoadd) := mget(columnstoadd), on = "PlotID"]
PlotGrizzly[, HuckCov := HuckCov*0.01, by = PlotID]

# 4. Thermal cover with forage
PlotGrizzly <- merge(PlotGrizzly, PlotThermalForage, all.x = TRUE)
PlotGrizzly[, ThermForage := ThermForage*0.01]
PlotGrizzly[is.na(PlotGrizzly)] <- 0

# GRIZZLY BEAR HABITAT INDEX
PlotGrizzly[, GrizzlyHabitat := sum(Ants, ForageCov, 2*HuckCov, ThermForage), by = PlotID]


#-- BIRDS *somethings are not right here*
# 1) Snag-associates
# snags >30cm dbh = snag SPH/1.02+SPH
PlotBirds <- merge(FR_treatments, PlotSnags[DBH_bin >=30, .(snagSPH = sum(snagSPH)), by = PlotID], all.x = TRUE)
PlotBirds[is.na(snagSPH), snagSPH := 0]
PlotBirds[, snag := snagSPH/(1.02 + snagSPH)]

# 2) Shrub-associates: 
# shrub cover <56% 0.01*cover+0.044, if >56 = 1
# which shrub layer? B1 or B2?
columnstoadd <- c("PlotID", "ShrubsB1")
PlotBirds[Cover, (columnstoadd) := mget(columnstoadd), on = "PlotID"]
PlotBirds[is.na(ShrubsB1), ShrubsB1 := 0]
# ***This is a placeholder - confirm which shrub layers***
PlotBirds[, shrub := ifelse(ShrubsB1 < 56, 0.01*ShrubsB1+0.044, 1)]

# 3) Conifer forest species: *check equation
# mature conifers >30cm DBH = (0.5/(1 + 2000 * e^-0.017* SPH)) + ((0.5*SPH)/(50 + SPH))
PlotBirds <- merge(PlotBirds, PlotTree[DBH_bin >= 30 & Species %in% c("Pl", "Sx", "Bl"), 
                                       .(lgconSPH=sum(SPH)), by=PlotID], all.x = TRUE)
PlotBirds[is.na(lgconSPH), lgconSPH := 0]
PlotBirds[, conifer := (0.5/(1 + 2000 * (exp(-0.017* lgconSPH)))) + ((0.5*lgconSPH)/(50 + lgconSPH))]
#** double check this equation, make sure the exponent is done correctly

# 4) Open-forest species:
# conifers <5 = 0, conifers > 5, 0.5*e^-0.5*(-(ln(conifersSPH?-2.4))^2 + 0.5*e^-0.5*(-(ln(conifersSPH?-3.5))^2))
PlotBirds <- merge(PlotBirds, PlotTree[Species %in% c("Pl", "Sx", "Bl"), .(conSPH = sum(SPH)), by = PlotID], all.x = TRUE)
PlotBirds[is.na(conSPH), conSPH := 0]
PlotBirds[, open := ifelse(conSPH >= 5, ((0.5*exp(-0.5*(-(log(conSPH-2.4))^2))) + (0.5*exp(-0.5*(-(log(conSPH-3.5))^2)))), 0)]

# 5) Forest-edge species:
# conifer sph between 1-800, -0.24* ln(SPH) + 0.137 * ln(SPH)^2 -0.052 * ln(SPH)^3
PlotBirds[, fSPHc := ifelse(conSPH >=1 & conSPH <=800, (-0.24*log(conSPH) + 0.137*log(conSPH)^2 - 0.052*log(conSPH)^3), 0), by = PlotID]
# deciduous sph between 1-800, -0.24 * ln(SPH) +0.137* ln(SPH)^2 - 0.052*ln(SPH)^3
PlotBirds <- merge(PlotBirds, PlotTree[Species %in% c("At", "Ac", "Ep"), .(decSPH = sum(SPH)), by = PlotID], all.x = TRUE)
PlotBirds[is.na(decSPH), decSPH := 0]
PlotBirds[, fSPHd := ifelse(decSPH >=1 & decSPH <=800, (-0.24*log(decSPH) + 0.137*log(decSPH)^2 - 0.052*log(decSPH)^3), 0), by = PlotID]
# decidous snags >30cm dbh, 0.15 * sngasSPH? / 1.02 * snagsSPH?
PlotBirds <- merge(PlotBirds, PlotSnags[DBH_bin >=30 & Species %in% c("At", "Ac", "Ep"), 
                                            .(decSnagSPH = sum(snagSPH)), by = PlotID], all.x = TRUE)
PlotBirds[, fDecSnags := (0.15*decSnagSPH)/(1.02*decSnagSPH), by = PlotID]
PlotBirds[is.na(fDecSnags), fDecSnags := 0]
# then add up
PlotBirds[, edge := ifelse(conSPH <1 | conSPH >800 & decSPH <1 | decSPH >800, 0.1 + fDecSnags,
                           ifelse(conSPH >=1 & conSPH <= 800 & decSPH <1 | decSPH >800, 0.1 + fDecSnags + fSPHc, 
                                  ifelse(decSPH >=1 & decSPH <=800 & conSPH >=1 & conSPH <=800, 0.1 + fDecSnags + fSPHd, 
                                         ifelse(conSPH >=1 & conSPH <=800 & decSPH >=1 & decSPH <=800, 0.1+fDecSnags + fSPHc + fSPHd, 0))))]
# ** there are negatives ** is this correct?

#-- ALL SPECIES 
HabitatIndices <- FR_treatments[PlotMarten, ("MartenHabitat") := mget("MartenHabitat"), on = "PlotID"]
HabitatIndices <- HabitatIndices[PlotFisher, ("FisherHabitat") := mget("FisherHabitat"), on = "PlotID"]
HabitatIndices <- HabitatIndices[PlotGoshawk, ("GoshawkHabitat") := mget("GoshawkHabitat"), on = "PlotID"]
HabitatIndices <- HabitatIndices[PlotHare, ("HareHabitat") := mget("HareHabitat"), on = "PlotID"]
HabitatIndices <- HabitatIndices[PlotSquirrel, ("SquirrelHabitat") := mget("SquirrelHabitat"), on = "PlotID"]
HabitatIndices <- HabitatIndices[PlotSmMammal, ("SmMammalHabitat") := mget("SmMammalHabitat"), on = "PlotID"]
HabitatIndices <- HabitatIndices[PlotGrouse, ("GrouseHabitat") := mget("GrouseHabitat"), on = "PlotID"]
HabitatIndices <- HabitatIndices[PlotGrizzly, ("GrizzlyHabitat") := mget("GrizzlyHabitat"), on = "PlotID"]
HabitatIndices <- HabitatIndices[PlotBirds, ("BirdsSnagHabitat") := mget("snag"), on = "PlotID"]
HabitatIndices <- HabitatIndices[PlotBirds, ("BirdsShrubHabitat") := mget("shrub"), on = "PlotID"]
HabitatIndices <- HabitatIndices[PlotBirds, ("BirdsConiferHabitat") := mget("conifer"), on = "PlotID"]
HabitatIndices <- HabitatIndices[PlotBirds, ("BirdsOpenHabitat") := mget("open"), on = "PlotID"]
HabitatIndices <- HabitatIndices[PlotBirds, ("BirdsEdgeHabitat") := mget("edge"), on = "PlotID"]

# export 
write.csv(HabitatIndices, file.path(out_dir,"hab_ind.csv"), row.names = FALSE)



#-----------------------
#-- Preliminary Figures

#alana modified -----------------------------------------------------------------------
# change the layout
#update so all indices are 0-1
hab_ind <- melt(HabitatIndices, id.vars = c("PlotID","Planted","TimeSinceFire"),
                measure.vars = c("MartenHabitat", "FisherHabitat", "GoshawkHabitat", 
                                 "HareHabitat", "SquirrelHabitat", "SmMammalHabitat",
                                 "GrouseHabitat", "GrizzlyHabitat", "BirdsSnagHabitat",
                                 "BirdsShrubHabitat", "BirdsConiferHabitat", "BirdsOpenHabitat",
                                 "BirdsEdgeHabitat"))
setnames(hab_ind, c("variable","value"), c("species","habitat_index"))
#Scale the indices between 0 and 1
scale_fn <- function(var){(var - min(var)) / (max(var) - min(var))}

hab_ind[,hab_ind_sc := scale_fn(habitat_index),by = .(species)]
hab_ind_d <- dcast(hab_ind, PlotID + Planted + TimeSinceFire ~ species,
                   value.var = "hab_ind_sc")
hab_ind_d <- hab_ind_d[,.(PlotID, MartenHabitat, FisherHabitat, GoshawkHabitat, HareHabitat,
                          SquirrelHabitat, SmMammalHabitat, GrouseHabitat, GrizzlyHabitat,
                          BirdsSnagHabitat, BirdsShrubHabitat, BirdsConiferHabitat, BirdsOpenHabitat,
                          BirdsEdgeHabitat)]
#write.csv(hab_ind_d, "./02-prepped_values/hab_indi.csv", row.names = F)


custom_color_scale <- c("#1f78b4", "#33a02c", "#e31a1c", "#ff7f00", "#6a3d9a", "#a6cee3", 
                        "#b15928", "deeppink", "aquamarine", "blue", "chartreuse", "darkmagenta",
                        "black")


ggplot(data= hab_ind)+
  geom_point(aes(x = TimeSinceFire, y = hab_ind_sc, colour = species))+
  geom_smooth(aes(x = TimeSinceFire, y = hab_ind_sc, colour = species), alpha = 0,
              method = "gam")+
  labs(color = "Wildlife species")+
  scale_color_manual(labels = c("Marten", "Fisher", "Goshawk", "Hare", "Squirrel",
                                "SmMammal", "Grouse", "Grizzly", "SnagBirds", "ShrubBirds",
                                "ConiferBirds", "OpenBirds", "EdgeBirds"), 
                     values = custom_color_scale)+
  xlab("Time since fire")+
  ylab("Habitat index")+
  facet_wrap(~Planted)

# birds only
hab_ind_birds <- hab_ind[species %in% c("BirdsSnagHabitat", "BirdsShrubHabitat", "BirdsConiferHabitat",
                         "BirdsOpenHabitat", "BirdsEdgeHabitat"), ]
 
ggplot(data= hab_ind_birds)+
  geom_point(aes(x = TimeSinceFire, y = hab_ind_sc, colour = species))+
  geom_smooth(aes(x = TimeSinceFire, y = hab_ind_sc, colour = species), alpha = 0)+
  labs(color = "Wildlife species")+
  scale_color_manual(labels = c("SnagBirds", "ShrubBirds",
                                "ConiferBirds", "OpenBirds", "EdgeBirds"), 
                     values = custom_color_scale)+
  xlab("Time since fire")+
  ylab("Habitat index")+
  facet_wrap(~Planted)

