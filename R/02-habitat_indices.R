# Habitat Preliminary Analysis

# This script summarizes habitat metrics for each species and bins each component 0 or 1. Each metric is added together / species to create
# one habitat value / species

# Ingrid Farnell, Alana Clason
# Dec 7, 2023

# Load libraries
library(data.table)
library(tidyverse)

# Load data
# Plot
FR_treatments <- fread("./Inputs/FR_Treatments.csv") # has field plot assessed treatments and fire year
# decided to change FR08 to NP because while it was a plantation, it was burned at low severity and not planted after the fire.
setnames(FR_treatments, "ID", "PlotID")
FR_treatments[,TimeSinceFire := 2020 - FIRE_YEAR]

# Tree data:
A1trees <- fread("./Inputs/A1trees.csv")
B1trees <- fread("./Inputs/B1trees.csv")
Regen <- fread("./Inputs/Regen.csv")

# Cover
densiometer <- fread("./Inputs/FRdensiometer.csv")
shrubCover <- fread("./Inputs/FRstrataCover.csv") #B1= <2m and B2=2-10m shrub heights)
setnames(shrubCover, c("Total_B1", "Total_B2"), c("ShrubsB1", "ShrubsB2"))

# Woody debris:
cwd <- fread("./Inputs/FireRehabData_CWD.csv")
fwd <- fread("./Inputs/FireRehabData_FWD.csv")
line <- fread("./Inputs/FR_LineTransect.csv")


#-- MARTEN
# marten require 1) quality cwd, [REF] 2) cwd clumps/piles [REF], 3) not 100% deciduous and >20% canopy closure [REF]
# 4) presence of snags [REF] and 5) shrubby understory >10% [REF]

# 1. CWD habitat quality -- using index from van Galen et al. 2019
source("./R/cwdHabitatQualityIndexFunction.R")
PlotHQI <- cwdHQI(cwd) #values between 0-100; 0=bad 100=best
PlotMarten <- merge(FR_treatments, PlotHQI)
PlotMarten[, HQI:=round(HQI*0.01, digit=1)]

# 2. CWD clumps/piles
source("./R/cwdPilesFunction.R")
PlotPiles <- cwdPiles(cwd)
PlotPiles[PlotID=="FR17" & Transect=="1", PileCount := 2] # notes on raw data says in 2 large piles
PlotPiles <- PlotPiles[,.(PileCount = sum(PileCount)), by = PlotID]
PlotMarten <- merge(PlotMarten, PlotPiles, all.x=TRUE)
PlotMarten[, PileCount := ifelse(PileCount > 0, 1, 0)][is.na(PileCount), PileCount := 0] # if plot has a pile of CWD = 1, else 0

# 3. Crown closure >20% and not 100% deciduous (we didn't sample plots that were 100% decid)
# mean of densiometer readings and multiply by 1.04 to get canopy openness
PlotCrown <- densiometer[,.(SubPlotOpen=mean(Densiometer)), by = c("PlotID", "SubPlot")]
PlotCrown <- PlotCrown[,.(CrownOpen=mean(SubPlotOpen)*1.04), by = PlotID]
# convert openness to canopy closure
PlotCrown[, CrownClos:= 100-CrownOpen]
PlotMarten <- merge(PlotMarten, PlotCrown, all.x=TRUE)
PlotMarten[, CrownClos := ifelse(CrownClos >= 20, 1, 0)] # closure >20% = 1, else 0
PlotMarten[is.na(CrownClos), CrownClos := 0][, CrownOpen := NULL]

# 4. Presence of snags >20cm dbh
source("./R/DensityFunctions.R")
PlotSnags <- SnagDensity(A1trees, B1trees)
# only care about presence >20cm, not species, so sum SPH per plot
PlotMarten <- merge(PlotMarten, PlotSnags[DBH_bin >= 20, .(snagSPH=sum(snagSPH)), by=PlotID], all.x=TRUE)
PlotMarten[, snags := ifelse(is.na(snagSPH), 0, 1)] # snags present = 1, else 0
PlotMarten[, snagSPH := NULL]

# 5. Shrub cover (B1= <2m and B2=2-10m shrub heights), B1 > 10%
columnstoadd <- c("PlotID", "ShrubsB1")
PlotMarten[shrubCover, (columnstoadd) := mget(columnstoadd), on = "PlotID"]
PlotMarten[, ShrubsB1 := ifelse(ShrubsB1 >= 10, 1, 0)][is.na(ShrubsB1), ShrubsB1 := 0] # shrubs 0-2m >10% cover = 1, else 0

# MARTEN HABITAT INDEX
PlotMarten[, MartenHabitat := sum(HQI, PileCount, CrownClos, snags, ShrubsB1), by=PlotID]


#-- FISHER
# fisher require 1) CWD quality similar to marten 2) large diseased and decaying 
# aspen and cottonwood (>30 cm dbh) 3) Ac leading or secondary or tertiary or Sx as 
# only species 4) QMD >=19.6 5) Crown closure >=30%  6) shrub cover > 20%
# all metrics are from www.bcfisherhabitat.ca

# 1. CWD quality similar to marten 
# already calculated this for marten
PlotFisher <- merge(FR_treatments, PlotHQI)
PlotFisher[, HQI:=round(HQI*0.01, digit=1)]

# 2. large diseased and decaying aspen and cottonwood (>30 cm dbh)
# we sampled mesic sites, which isn't normally where large At/Ac grow.. should we remove this?
#source("./R/DensityFunctions.R")
PlotTree <- TreeDensity(A1trees, B1trees)
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
# already calculated for marten
PlotFisher<- merge(PlotFisher, PlotCrown, all.x=TRUE)
PlotFisher[, CrownClos := ifelse(CrownClos >=30, 1, 0)] # crown closure >= 30% = 1, else 0
PlotFisher[is.na(CrownClos), CrownClos := 0][, CrownOpen := NULL]

# 6. shrub cover(0-2m) > 20%
# already calculated for marten (just filter for >=20%)
columnstoadd <- c("PlotID", "ShrubsB1")
PlotFisher[shrubCover, (columnstoadd) := mget(columnstoadd), on = "PlotID"]
PlotFisher[, ShrubsB1 := ifelse(ShrubsB1 >=20, 1, 0)][is.na(ShrubsB1), ShrubsB1 := 0] # shrubs (0-2m) >= 20% cover = 1, else 0

# FISHER HABITAT INDEX
PlotFisher[, FisherHabitat := sum(HQI, lgAtSPH, decidSx, QMD, CrownClos, ShrubsB1), by=PlotID]


#-- GOSHAWK
# goshawk require 1) large >30cm dbh snag retention and 2) large >30cm dbh live tree retention, 
# 3) large CWD, >30cm diameter 4) clumped/piled CWD
# metrics are from a conversation Ingrid had with Frank Doyle Oct 6, 2023

# 1. large snag >30cm retention post fire
# only care about presence, not species, so sum SPH per plot
PlotGoshawk <- merge(FR_treatments, PlotSnags[DBH_bin >= 30, .(snagSPH=sum(snagSPH)), by=PlotID], all.x=TRUE)
PlotGoshawk[, snagSPH := ifelse(snagSPH > 0, 1, 0)][is.na(snagSPH), snagSPH := 0] # snags >=30cm dbh present =1, else 0

# 2. large diameter >30cm live trees
PlotGoshawk <- merge(PlotGoshawk, PlotTree[DBH_bin >= 30, .(liveSPH=sum(SPH)), by=PlotID], all.x=TRUE)
PlotGoshawk[, liveSPH := ifelse(liveSPH > 0, 1, 0)][is.na(liveSPH), liveSPH := 0] # live trees >=30cm dbh present = 1, else 0

# 2. large CWD >30cm diam / quality CWD
# already calculated for marten & fisher
PlotGoshawk <- merge(PlotGoshawk, PlotHQI)
PlotGoshawk[, HQI:=round(HQI*0.01, digit=1)] 

# 3. CWD piles
PlotGoshawk <- merge(PlotGoshawk, PlotPiles, all.x=TRUE)
PlotGoshawk[, PileCount := ifelse(PileCount > 0, 1, 0)][is.na(PileCount), PileCount := 0] # if plot has cwd piles = 1, else 0

# GOSHAWK HABITAT INDEX
PlotGoshawk[, GoshawkHabitat := sum(snagSPH, liveSPH, HQI, PileCount), by=PlotID]


#-- SNOWSHOE HARE
# Snowshoe hare require 1) canopy cover >= 30% (Berg et al., 2012; Cheng et al. 2015) and 2) shrub cover (0-2m) >=30% (Bois et al., 2012; Gigliotti et al., 2018), 
# 3) tree density >=3000 stems/ha (Chisholm 2023), # 4) abundant CWD (Berg et al., 2012), and 5) browse (Joc)

# 1. Canopy cover >= 30%
PlotHare <- merge(FR_treatments, PlotCrown, all.x = TRUE)
PlotHare[, CrownClos := ifelse(CrownClos >= 30, 1, 0)]
PlotHare[is.na(CrownClos), CrownClos := 0][, CrownOpen := NULL]

# 2. Shrub cover (B1= <2m and B2=2-10m shrub heights), B1 >= 30%
columnstoadd <- c("PlotID", "ShrubsB1")
PlotHare[shrubCover, (columnstoadd) := mget(columnstoadd), on = "PlotID"]
PlotHare[, ShrubsB1 := ifelse(ShrubsB1 >=30, 1, 0)][is.na(ShrubsB1), ShrubsB1 := 0] # shrubs (0-2m) >= 30% cover = 1, else 0

# 3. Tree density >=3000 stems/ha
PlotHare <- merge(PlotHare, PlotTree[,. (SPH=sum(SPH)), by=c("PlotID")], all.x=TRUE)
PlotHare <- PlotHare[, SPH := ifelse(SPH >= 3000, 1, 0)][is.na(SPH), SPH := 0] # tree density >= 3,000 sph = 1, else 0

# 4. Presence of CWD
source("./R/cwdVolumeFunction.R")
PlotCWD <- cwdVol(cwd, line)
PlotHare <- merge(PlotHare, PlotCWD[,. (CWDvol=sum(VolHa)), by=c("PlotID")], all.x=TRUE)
PlotHare[, CWDvol := ifelse(CWDvol > 0 , 1, 0)] # if CWd present = 1, else 0

# 5. Browse info *Jocelyn*

# SNOWSHOE HARE HABITAT INDEX *although still waiting for Joeclyn's stuff*
PlotHare[, HareHabitat := sum(CrownClos, ShrubsB1, SPH, CWDvol), by=PlotID]


#-- RED SQUIRRELS
# Red squirrels require 1) tree canopy cover >=30% (Aubry et al., 2003), 2) large conifers >=30cm dbh (Fisher and Bradbury, 2006; Holloway and Malcolm, 2006), 
# 3) large snags >=40cm (Fisher and Wilkinson, 2005), 4) tree composition 50% conifer (Fisher and Bradbury, 2006),
# 5) 60% spruce preferred (Fisher and Bradbury, 2006), and 6) abundant cwd >=100m3 (Bakker 2006)

# 1. Tree canopy cover >=30%
PlotSquirrel <- merge(FR_treatments, PlotCrown, all.x = TRUE)
PlotSquirrel[, CrownClos := ifelse(CrownClos >=30, 1, 0)] # crown closure >= 30% = 1, else 0
PlotSquirrel[is.na(CrownClos), CrownClos := 0][, CrownOpen := NULL]

# 2. Large conifers >=30cm dbh
PlotSquirrel <- merge(PlotSquirrel, PlotTree[Species %in% c("Sx", "Pl", "Bl") & DBH_bin>=30, 
                                              .(lgConSPH=sum(SPH)), by=c("PlotID")], all.x=TRUE)
PlotSquirrel[, lgConSPH := ifelse(lgConSPH > 0, 1, 0)][is.na(lgConSPH), lgConSPH := 0] # large conifers >=30cm dbh present =1, else 0

# 3. Large snags >=40cm dbh
PlotSquirrel <- merge(PlotSquirrel, PlotSnags[DBH_bin>=40, .(snagSPH=sum(snagSPH)), by=PlotID], all.x=TRUE)
PlotSquirrel[, snagSPH := ifelse(snagSPH > 0, 1, 0)][is.na(snagSPH), snagSPH:=0] # large snags >=30cm dbh present = 1, else 0

# 4. Tree composition at least 50% conifer
treeComp <- PlotTree[DBH_bin >=10, totalSPH := sum(SPH), by=c("PlotID")]
treeComp <- treeComp[DBH_bin >=10, .(spComp=sum(SPH)/totalSPH), by=c("PlotID", "Species")]
treeComp <- unique(treeComp)
PlotSquirrel <- merge(PlotSquirrel, treeComp[Species %in% c("Sx", "Pl", "Bl"), 
                                             .(conComp=sum(spComp)), by=PlotID], all.x=TRUE)
PlotSquirrel[, conComp := ifelse(conComp >= 0.5, 1, 0)][is.na(conComp), conComp := 0] # tree composition >=50% conifer = 1, else 0

# 5. Tree composition at least 60% Sx is preferred
PlotSquirrel <- merge(PlotSquirrel, treeComp[Species=="Sx", .(SxComp=sum(spComp)), by=PlotID], all.x=TRUE)
PlotSquirrel[, SxComp := ifelse(SxComp >= 0.6, 1, 0)][is.na(SxComp), SxComp := 0] # tree composition >=60% spruece = 1, else 0

# 6. CWD >100m3
PlotSquirrel <- merge(PlotSquirrel, PlotCWD[,.(CWDvol=sum(VolHa)), by=PlotID], all.x = TRUE)
PlotSquirrel[, CWDvol := ifelse(CWDvol >= 100, 1, 0)] # CWD >= 100m3/ha = 1, else 0

# RED SQUIRREL HABITAT INDEX
PlotSquirrel[, SquirrelHabitat := sum(CrownClos, lgConSPH, snagSPH, conComp, SxComp, CWDvol), by=PlotID]


#-- SMALL MAMMALS (deer mice, southern red backed voles)
# Small mammals require 1) abundant (>50m3/ha) decayed (class 4&5) CWD (Fauteaux et al., 2012; Fauteux et al., 2013), and
# 2) live tree  (>9 cm dbh) basal area >= 15 m2/ha(Fauteux., 2013; Sullivan and Sullivan, 2002)

# 1. >50m3/ha cwd volume decay class 4 & 5
PlotSmMammal <- merge(FR_treatments, PlotCWD[Decay_class >=4, .(CWDvol=sum(VolHa)), by=PlotID], all.x=TRUE)
PlotSmMammal[, CWDvol := ifelse(CWDvol>= 50, 1, 0)][is.na(CWDvol), CWDvol := 0] # decayed (classes 4 &5) cwd >= 50 m3/ha = 1, else 0

# 2. Tree basal area (>9 cm dbh)
source("./R/BasalAreaFunctions.R")
PlotBAPH <- BAPH(A1trees, B1trees)
PlotSmMammal <- merge(PlotSmMammal, PlotBAPH[DBH >= 9, .(BAPH=sum(BAPH)), by=PlotID], all.x=TRUE)
PlotSmMammal[, BAPH := ifelse(BAPH >= 15 , 1, 0)][is.na(BAPH), BAPH := 0]

# SMALL MAMMAL HABITAT INDEX
PlotSmMammal[, SmMammalHabitat := sum(CWDvol, BAPH), by=PlotID]


#-- ALL SPECIES 
HabitatIndicies <- FR_treatments[PlotMarten, ("MartenHabitat") := mget("MartenHabitat"), on = "PlotID"]
HabitatIndicies <- HabitatIndicies[PlotFisher, ("FisherHabitat") := mget("FisherHabitat"), on = "PlotID"]
HabitatIndicies <- HabitatIndicies[PlotGoshawk, ("GoshawkHabitat") := mget("GoshawkHabitat"), on = "PlotID"]
HabitatIndicies <- HabitatIndicies[PlotHare, ("HareHabitat") := mget("HareHabitat"), on = "PlotID"]
HabitatIndicies <- HabitatIndicies[PlotSquirrel, ("SquirrelHabitat") := mget("SquirrelHabitat"), on = "PlotID"]
HabitatIndicies <- HabitatIndicies[PlotSmMammal, ("SmMammalHabitat") := mget("SmMammalHabitat"), on = "PlotID"]

# export 
#write.csv(HabitatIndicies, "./Outputs/HabitatIndicies.csv")
