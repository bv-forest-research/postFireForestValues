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
shrubCover <- fread("./Inputs/FRstrataCover.csv")
setnames(shrubCover, c("Total_B1", "Total_B2"), c("ShrubsB1", "ShrubsB2"))

#Woody debris:
cwd <- fread("./Inputs/FireRehabData_CWD.csv")
fwd <- fread("./Inputs/FireRehabData_FWD.csv")
line <- fread("./Inputs/FR_LineTransect.csv")


#-- MARTEN
# marten require 1) quality cwd, 2) cwd clumpiness, 3) canopy closure, 4) presence of snags,
# and 5) shrubby understory

# 1. CWD habitat quality -- using index from van Galen et al. 2019
source("./R/cwdHabitatQualityIndexFunction.R")
PlotHQI <- cwdHQI(cwd) #values between 0-100; 0=bad 100=best
PlotMarten <- merge(FR_treatments, PlotHQI)

# 2. CWD clumpiness (still need to learn how to do wavelets)

# 3. Crown closure
# mean of densiometer readings and multiply by 1.04 to get canopy openness
PlotCrown <- densiometer[,.(SubPlotOpen=mean(Densiometer)), by = c("PlotID", "SubPlot")]
PlotCrown <- PlotCrown[,.(CrownOpen=mean(SubPlotOpen)*1.04), by = PlotID]
# convert openness to canopy closure
PlotCrown[, CrownClos:= 100-CrownOpen]
PlotMarten <- merge(PlotMarten, PlotCrown)

# 4. Presence of snags
source("./R/DensityFunctions.R")
PlotSnags <- SnagDensity(A1trees, B1trees)
# only keep diam > 20cm
PlotSnags <- PlotSnags[DBH_bin >= 20]
# only care about presence, not species, so sum SPH per plot
PlotSnags <- PlotSnags[,.(snagSPH=sum(snagSPH)), by=PlotID]
PlotMarten <- merge(PlotMarten, PlotSnags, all.x=TRUE)
PlotMarten[is.na(snagSPH), snagSPH := 0]

# 5. Shrub cover (B1= <2m and B2=2-10m shrub heights)
columnstoadd <- c("PlotID", "ShrubsB1", "ShrubsB2")
PlotMarten[shrubCover, (columnstoadd) := mget(columnstoadd), on = "PlotID"]
PlotMarten[is.na(ShrubsB1), ShrubsB1 := 0][is.na(ShrubsB2), ShrubsB2 := 0]


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

ggsave("./Outputs/martenHab.jpg", width=8, height=8)


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

# 3. Ac leading or secondary or tertiary or Sx as only species
DecidLead <- PlotTree[DBH_bin >=10]
DecidLead <- DecidLead[,. (SPH=sum(SPH)), by=c("PlotID", "Species")]
DecidLead <- DecidLead[, decidSxLead := ifelse(
  (Species == "At" & SPH == max(SPH, na.rm = TRUE)) |
    (Species == "Ac" & SPH == max(SPH, na.rm = TRUE)) |
    (length(unique(Species)) == 1 && first(Species) == "Sx"), # Sx is only species
  1, 0), by = PlotID]
columnstoadd <- c("PlotID", "decidSxLead")
PlotFisher[DecidLead, (columnstoadd) := mget(columnstoadd), on = "PlotID"]
PlotFisher[is.na(decidSxLead), decidSxLead := 0]
# ** should this be kept binary or should it be a percent At Ac cover? **

# 4. QMD >=19.6 (quadratic mean diameter)
QMD <- B1trees[,. (QMD=sqrt(sum(DBH^2)/.N)), by = PlotID]
PlotFisher <- merge(PlotFisher, QMD, all.x=TRUE)
PlotFisher[is.na(QMD), QMD := 0]
# ** should I filter this or leave it **

# 5. Crown closure >=30%
# already calculated for marten
PlotFisher<- merge(PlotFisher, PlotCrown)
PlotFisher[is.na(CrownClos), CrownClos := 0]
# ** should I filter this or leave it **

# 6. shrub cover > 20%
# already calculated for marten
columnstoadd <- c("PlotID", "ShrubsB1", "ShrubsB2")
PlotFisher[shrubCover, (columnstoadd) := mget(columnstoadd), on = "PlotID"]
PlotFisher[is.na(ShrubsB1), ShrubsB1 := 0][is.na(ShrubsB2), ShrubsB2 := 0]
# ** should I filter this or leave it **

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

f.decid <- ggplot(PlotFisher, aes(x=TimeSinceFire, y=decidSxLead, colour=Planted)) +
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

ggsave("./Outputs/fisherHab.jpg", width=8, height=8)

#-- GOSHAWK
# goshawk require 