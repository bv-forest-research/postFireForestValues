# Stand structure Preliminary Analysis

# This script summarizes the data into stand structure variables
# Ingrid Farnell
# June 8, 2023

# Import data
#Plot
FR_treatments <- fread("./Inputs/FR_Treatments.csv") # has field plot assessed treatments and fire year
#decided to change FR08 to NP because while it was a plantation, it was burned at low severity and not planted after the fire.
setnames(FR_treatments, "ID", "PlotID")
FR_treatments[,TimeSinceFire := 2020 - FIRE_YEAR]

#Tree data:
A1trees <- fread("./Inputs/A1trees.csv")
B1trees <- fread("./Inputs/B1trees.csv")
Regen <- fread("./Inputs/Regen.csv")

#Woody debris:
cwd <- fread("./Inputs/FireRehabData_CWD.csv")
fwd <- fread("./Inputs/FireRehabData_FWD.csv")
line <- fread("./Inputs/FR_LineTransect.csv")


# Variables
# Basal area per hectare (BAPH)
source("./R/BasalAreaFunctions.R")
BAtrees <- BAPH(A1trees, B1trees)
PlotBAtrees <- BAtrees[,.(BAPH=sum(BAPH)), by="PlotID"]
PlotBAtrees <- merge(PlotBAtrees, FR_treatments)

# 7.5+, 12.5+, 17.5+, 22.5+ (size classes from BC VRI )
BAdiamClass <- BAPHsizeCl(A1trees, B1trees)
BAdiamClass <- merge(BAdiamClass, FR_treatments)

# Live trees stems per hectare (density)
source("./R/DensityFunctions.R")
PlotTree <- TreeDensity(A1trees, B1trees) # ask Alana why 2cm size bins
# AC - changed it to 5cm (2 is for SORTIE)
PlotTree <- merge(PlotTree, FR_treatments)

# Snags per hectare
PlotSnags <- SnagDensity(A1trees, B1trees)
PlotSnags <- merge(PlotSnags, FR_treatments)

# CWD HQI
source("./R/cwdHabitatQualityIndexFunction.R")
PlotHQI <- cwdHQI(cwd)
PlotCWD <- cwdVar(cwd)
PlotCWD <- merge(PlotCWD, PlotHQI)

# CWD volume
source("./R/cwdVolumeFunction.R")
CWDvol <- cwdVol(cwd, line) #volume by species & decay class
PlotCWDvol <- CWDvol[,.(VolHa=sum(VolHa)), by="PlotID"]
PlotCWD <- merge(PlotCWD, PlotCWDvol)
PlotCWD <- merge(PlotCWD, FR_treatments)

cwd <- merge(cwd, FR_treatments)
#--------------------------------------------------------------------
# Prelim figures

# Basal area
BA <- ggplot(PlotBAtrees, aes(x=TimeSinceFire, y=BAPH)) +
  geom_point(aes(colour=Planted)) +
  geom_smooth(method="lm",aes(colour=Planted,fill=Planted))
BA

# Basal area diameter classes
BAcl <- ggplot(BAdiamClass, aes(x=DBH_bin, y=BAPH)) +
  geom_col(aes(colour=Species), position="dodge") +
  facet_grid(rows=vars(Planted))
BAcl

# large live trees
lglive <- PlotTree %>%
  filter(DBH_bin >=20) %>%
  ggplot(aes(x=DBH_bin, y=SPH)) +
  geom_point(aes(colour=Planted)) +
  facet_grid(rows=vars(Planted))
lglive

# large cottonwood or aspen
liveAcAt <- PlotTree %>%
  filter(Species=="Ac" | Species=="At") %>%
  filter(DBH_bin >= 20) %>%
  ggplot(aes(x=DBH_bin, y=SPH)) +
  geom_point(aes(colour=Species)) +
  facet_grid(rows=vars(Planted))
liveAcAt

# large snags
snags <- PlotSnags %>% 
  filter(DBH_bin >= 20) %>%
  ggplot(aes(x=DBH_bin, y=SPH)) +
  geom_point(aes(colour=Planted)) +
  facet_grid(rows=vars(Planted))
snags

# CWD
hqi <- ggplot(PlotCWD, aes(x=Planted, y=HQI)) +
  geom_boxplot()
hqi

cwdVol <- ggplot(PlotCWD, aes(x=Planted, y=VolHa)) +
  geom_boxplot()
cwdVol

cwdMaxDiam <- ggplot(PlotCWD, aes(x=Planted, y=diamMax)) +
  geom_boxplot()
cwdMaxDiam

cwdN <- ggplot(PlotCWD, aes(x=Planted, y=PlotN)) +
  geom_boxplot()
cwdN

cwdLoc <- cwd %>%
  mutate(Location_m = if_else(condition = Transect==2,
                              true=Location_m+50,
                              false=Location_m)) %>% 
  ggplot(aes(x=Location_m, y=PlotID)) +
  geom_point() +
  facet_grid(rows=vars(Planted))
cwdLoc # i am able to map cwd piece location - not sure how to put this into piles & dispersed
