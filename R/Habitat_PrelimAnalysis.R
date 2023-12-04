# Habitat Preliminary Analysis

# this script summarizes habitat components for each species 
# Ingrid Farnell, Alana Clason
# Sept 27, 2023

# load libraries
library(data.table)
library(tidyverse)
library(ggpubr)
library(svglite)

# load data
#Plot
FR_treatments <- fread("./Inputs/FR_Treatments.csv") # has field plot assessed treatments and fire year
#decided to change FR08 to NP because while it was a plantation, it was burned at low severity and not planted after the fire.
setnames(FR_treatments, "ID", "PlotID")
FR_treatments[,TimeSinceFire := 2020 - FIRE_YEAR]

#Tree data:
A1trees <- fread("./Inputs/A1trees.csv")
B1trees <- fread("./Inputs/B1trees.csv")
Regen <- fread("./Inputs/Regen.csv")

# Cover
densiometer <- fread("./Inputs/FRdensiometer.csv")
shrubCover <- fread("./Inputs/FRstrataCover.csv") #B1= <2m and B2=2-10m shrub heights)
setnames(shrubCover, c("Total_B1", "Total_B2"), c("ShrubsB1", "ShrubsB2"))

#Woody debris:
cwd <- fread("./Inputs/FireRehabData_CWD.csv")
fwd <- fread("./Inputs/FireRehabData_FWD.csv")
line <- fread("./Inputs/FR_LineTransect.csv")


#-- MARTEN
# marten require 1) quality cwd, 2) cwd clumps/piles, 3) not 100% deciduous and >20% canopy closure 
# 4) presence of snags and 5) shrubby understory >10% 

# 1. CWD habitat quality -- using index from van Galen et al. 2019
source("./R/cwdHabitatQualityIndexFunction.R")
PlotHQI <- cwdHQI(cwd) #values between 0-100; 0=bad 100=best
PlotMarten <- merge(FR_treatments, PlotHQI)

# 2. CWD clumps/piles
source("./R/cwdPilesFunction.R")
PlotPiles <- cwdPiles(cwd)
PlotPiles <- PlotPiles[,.(PileCount = sum(PileCount)), by = PlotID]
PlotMarten <- merge(PlotMarten, PlotPiles, all.x=TRUE)

# 3. Crown closure >20% and not 100% deciduous (we didn't sample plots that were 100% decid)
# mean of densiometer readings and multiply by 1.04 to get canopy openness
PlotCrown <- densiometer[,.(SubPlotOpen=mean(Densiometer)), by = c("PlotID", "SubPlot")]
PlotCrown <- PlotCrown[,.(CrownOpen=mean(SubPlotOpen)*1.04), by = PlotID]
# convert openness to canopy closure
PlotCrown[, CrownClos:= 100-CrownOpen]
PlotMarten <- merge(PlotMarten, PlotCrown[CrownClos > 20,], all.x = TRUE)
PlotMarten[is.na(CrownClos), CrownClos := 0][is.na(CrownOpen), CrownOpen := 0]

# 4. Presence of snags >20cm dbh
source("./R/DensityFunctions.R")
PlotSnags <- SnagDensity(A1trees, B1trees)
# only care about presence >20cm, not species, so sum SPH per plot
PlotMarten <- merge(PlotMarten, PlotSnags[DBH_bin >= 20, .(snagSPH=sum(snagSPH)), by=PlotID], all.x=TRUE)
PlotMarten[is.na(snagSPH), snagSPH := 0]

# 5. Shrub cover (B1= <2m and B2=2-10m shrub heights), B1 > 10%
columnstoadd <- c("PlotID", "ShrubsB1")
PlotMarten[shrubCover[ShrubsB1 > 10], (columnstoadd) := mget(columnstoadd), on = "PlotID"]
PlotMarten[is.na(ShrubsB1), ShrubsB1 := 0]


# Figures
p.hqi <- ggplot(PlotMarten, aes(x=TimeSinceFire, y=HQI, colour=Planted)) +
  geom_point() +
  geom_smooth() +
  labs(y="cwd HQI", x=NULL)
p.hqi

p.crownclos <- ggplot(PlotMarten, aes(x=TimeSinceFire, y=CrownClos, colour=Planted)) +
  geom_point() +
  geom_smooth() +
  labs(x=NULL)
p.crownclos

p.snags <- ggplot(PlotMarten, aes(x=TimeSinceFire, y=snagSPH, colour=Planted)) +
  geom_point() +
  geom_smooth() +
  labs(x=NULL)
p.snags

p.shrub <- ggplot(PlotMarten, aes(x=TimeSinceFire, y=ShrubsB1, colour=Planted)) +
  geom_point() +
  geom_smooth() +
  labs(y="Shrubs <2m \n% cover")
p.shrub

# multi plot figure
marten <- ggarrange(p.hqi, p.crownclos, p.snags, p.shrub,
                    ncol = 1,
                    common.legend = TRUE,
                    align = "v")
marten <- annotate_figure(marten, 
                          top=text_grob("Marten Habitat Components"))
marten
#ggsave("./Outputs/martenHab.jpg", width=8, height=8)


#-- FISHER
# fisher require 1) CWD quality similar to marten 2) large diseased and decaying 
# aspen and cottonwood (>30 cm dbh) 3) Ac leading or secondary or tertiary or Sx as 
# only species 4) QMD >=19.6 5) Crown closure >=30%  6) shrub cover > 20%

# 1. CWD quality similar to marten 
# already calculated this for marten
PlotFisher <- merge(FR_treatments, PlotHQI)

# 2. large diseased and decaying aspen and cottonwood (>30 cm dbh)
# we sampled mesic sites, which isn't normally where large At/Ac grow.. should we remove this?
#source("./R/DensityFunctions.R")
PlotTree <- TreeDensity(A1trees, B1trees)
# filter to AC or At and > 30 cm dbh
LgAtAc <- PlotTree[Species %in% c("Ac", "At") & DBH_bin>=30,]
LgAtAc <- LgAtAc[,.(lgAtSPH=sum(SPH)), by=PlotID]

columnstoadd <- c("PlotID", "lgAtSPH")
PlotFisher[LgAtAc, (columnstoadd) := mget(columnstoadd), on = "PlotID"]
PlotFisher[is.na(lgAtSPH), lgAtSPH := 0]

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
PlotFisher[is.na(decidSx), decidSx := 0]

# 4. QMD >=19.6 (quadratic mean diameter)
QMD <- B1trees[,. (QMD=sqrt(sum(DBH^2)/.N)), by = PlotID]
PlotFisher <- merge(PlotFisher, QMD[QMD >=19.6,], all.x=TRUE)
PlotFisher[is.na(QMD), QMD := 0]

# 5. Crown closure >=30%
# already calculated for marten
PlotFisher<- merge(PlotFisher, PlotCrown[CrownClos >= 30], all.x=TRUE)
PlotFisher[is.na(CrownClos), CrownClos := 0][is.na(CrownOpen), CrownOpen := 0]

# 6. shrub cover(0-2m) > 20%
# already calculated for marten (just filter for >=20%)
columnstoadd <- c("PlotID", "ShrubsB1")
PlotFisher[shrubCover[ShrubsB1 >= 20], (columnstoadd) := mget(columnstoadd), on = "PlotID"]
PlotFisher[is.na(ShrubsB1), ShrubsB1 := 0]


# Figures
f.hqi <- ggplot(PlotFisher, aes(x=TimeSinceFire, y=HQI, colour=Planted)) +
  geom_point() +
  geom_smooth() +
  labs(y="cwd HQI", x=NULL)
f.hqi

f.lgAt <- ggplot(PlotFisher, aes(x=TimeSinceFire, y=lgAtSPH, colour=Planted)) +
  geom_point() +
  geom_smooth() +
  labs(x=NULL)
f.lgAt

f.decid <- ggplot(PlotFisher, aes(x=TimeSinceFire, y=decidSx, colour=Planted)) +
  geom_point() +
  geom_smooth() +
  labs(x=NULL)
f.decid

f.qmd <- ggplot(PlotFisher, aes(x=TimeSinceFire, y=QMD, colour=Planted)) +
  geom_point() +
  geom_smooth() +
  labs(x=NULL)
f.qmd

f.shrub <- ggplot(PlotFisher, aes(x=TimeSinceFire, y=ShrubsB1, colour=Planted)) +
  geom_point() +
  geom_smooth() +
  labs(y="Shrubs <2m \n% cover")
f.shrub

# multi plot figure
fisher <- ggarrange(f.hqi, f.lgAt, 
                    f.decid, f.qmd, 
                    f.shrub,
                    ncol = 1,
                    common.legend = TRUE,
                    align = "v")
fisher <- annotate_figure(fisher, 
                          top=text_grob("Fisher Habitat Components"))
fisher
#ggsave("./Outputs/fisherHab.jpg", width=8, height=8)


#-- GOSHAWK
# goshawk require 1) snag and live tree retention, large diameter snags >30cm dbh
# 2) large CWD, >30cm diameter & >7m long 3) clumped/piled CWD

# 1. large snag >30cm and live tree retention post fire
# only care about presence, not species, so sum SPH per plot
PlotGoshawk <- merge(FR_treatments, PlotSnags[DBH_bin >= 30, .(snagSPH=sum(snagSPH)), by=PlotID], all.x=TRUE)
PlotGoshawk[is.na(snagSPH), snagSPH := 0]

# 2. large CWD >30cm diam / quality CWD
# already calculated for marten & fisher
PlotGoshawk <- merge(PlotGoshawk, PlotHQI)

# 3. CWD piles
PlotGoshawk <- merge(PlotGoshawk, PlotPiles, all.x=TRUE)

#-- SNOWSHOE HARE
# Snowshoe hare require 1) dense canopy cover and 2) shrub cover >30%, 3) tree density >3000 stems/ha, 
# 4) abundant CWD, and 5) browse (Joc)

# 1. Dense canopy cover, minimum 30%
PlotHare <- merge(FR_treatments, PlotCrown[CrownClos > 30,], all.x = TRUE)
PlotHare[is.na(CrownClos), CrownClos := 0][is.na(CrownOpen), CrownOpen := 0]

# 2. Shrub cover (B1= <2m and B2=2-10m shrub heights), B1 >= 30%
columnstoadd <- c("PlotID", "ShrubsB1")
PlotHare[shrubCover[ShrubsB1 > 30], (columnstoadd) := mget(columnstoadd), on = "PlotID"]
PlotHare[is.na(ShrubsB1), ShrubsB1 := 0]

# 3. Tree density >3000 stems/ha
PlotHare <- merge(PlotHare, PlotTree[,. (SPH=sum(SPH)), by=c("PlotID")])
PlotHare <- PlotHare[SPH <= 3000, SPH := 0]

# 4. Abundant CWD
source("./R/cwdVolumeFunction.R")
PlotCWD <- cwdVol(cwd, line)
PlotHare <- merge(PlotHare, PlotCWD[,. (CWDvol=sum(VolHa)), by=c("PlotID")])

# 5. Browse info *Jocelyn*


#-- RED SQUIRRELS
# Red squirrels require 1) tree canopy cover >30%, 2) large conifers >30cm dbh, 
# 3) large snags >40cm, 4) tree composition 50% conifer, 5) 60% spruce preferred, and
# 6) abundant cwd >100m3 

# 1. Tree canopy cover >30%
PlotSquirrel <- merge(FR_treatments, PlotCrown[CrownClos > 30,], all.x = TRUE)
PlotSquirrel[is.na(CrownClos), CrownClos := 0][is.na(CrownOpen), CrownOpen := 0]

# 2. Large conifers >30cm dbh
PlotSquirrel <- merge(PlotSquirrel, PlotTree[Species %in% c("Sx", "Pl", "Bl") & DBH_bin>=30, 
                                              .(lgConSPH=sum(SPH)), by=c("PlotID")], all.x=TRUE)
PlotSquirrel[is.na(lgConSPH), lgConSPH := 0]

# 3. Large snags >40cm dbh
PlotSquirrel <- merge(PlotSquirrel, PlotSnags[DBH_bin>=40, .(snagSPH=sum(snagSPH)), by=PlotID], all.x=TRUE)
PlotSquirrel[is.na(snagSPH), snagSPH:=0]

# 4. Tree composition at least 50% conifer
treeComp <- PlotTree[DBH_bin >=10, totalSPH := sum(SPH), by=PlotID]
treeComp <- treeComp[DBH_bin >=10, .(spComp=sum(SPH)/totalSPH), by=c("PlotID", "Species")]
treeComp <- unique(treeComp)
PlotSquirrel <- merge(PlotSquirrel, treeComp[Species %in% c("Sx", "Pl", "Bl"), 
                                             .(conComp=sum(spComp)), by=PlotID], all.x=TRUE)
PlotSquirrel[conComp < 0.5, conComp := 0][is.na(conComp), conComp := 0]

# 5. Sx composition at least 60%
PlotSquirrel <- merge(PlotSquirrel, treeComp[Species=="Sx", .(SxComp=sum(spComp)), by=PlotID], all.x=TRUE)
PlotSquirrel[SxComp <0.6, SxComp := 0][is.na(SxComp), SxComp := 0]

# 6. CWD >100m3
PlotSquirrel <- merge(PlotSquirrel, PlotCWD[,.(CWDvol=sum(VolHa)), by=PlotID], all.x = TRUE)
PlotSquirrel[CWDvol < 100, CWDvol := 0]


#-- SMALL MAMMALS (deer mice, southern red backed voles)
# Small mammals require 1) abundant (>100m3/ha) decayed (class 4&5) CWD, 2) high live
# tree basal area

# 1. >44m3/ha cwd volume decay class 4 & 5
PlotSmMammal <- merge(FR_treatments, PlotCWD[Decay_class >=4, .(CWDvol=sum(VolHa)), by=PlotID], all.x=TRUE)
PlotSmMammal[CWDvol < 44, CWDvol := 0][is.na(CWDvol), CWDvol := 0] # confirm with Erica she agrees with 100m3/ha

# 2. Tree basal area (> 9cm dbh)
source("./R/BasalAreaFunctions.R")
PlotBAPH <- BAPH(A1trees, B1trees)
PlotSmMammal <- merge(PlotSmMammal, PlotBAPH[DBH >= 9, .(BAPH=sum(BAPH)), by=PlotID], all.x=TRUE)
PlotSmMammal[is.na(BAPH), BAPH := 0]
