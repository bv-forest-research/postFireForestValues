#stand structure with treatment

library(data.table)
library(ggplot2)
in_dir <- "01_data_inputs"
out_dir <- "02_prepped_values"


# Import data

files_to_source <- list.files("./R/00-utils/", pattern = "Function", 
                              full.names = TRUE)
sapply(files_to_source, source)

# Treatments ---------------------------------------------------------------
treat_hist <- fread(file.path(in_dir,"treatment_history.csv"), na.strings = "n/a")

#dcast(treat_hist[,.(PlotID, pre_post)])
# Treatments ---------------------------------------------------------------
FR_treatments <- fread(file.path(in_dir,"FR_Treatments.csv"))

#Plot treatment cleaning
FR_treatments[,`:=`(PlotID = as.factor(ID), Planted = as.factor(Planted))]
FR_treatments[, TimeSinceFire := 2020 - FIRE_YEAR]
#for this paper, we don't need all the columns:
plot_treatments <- FR_treatments[,.(PlotID, Planted, TimeSinceFire)]
FR_treatments[,TSF := ifelse(TimeSinceFire <= 10, "<10",
                             ifelse(TimeSinceFire <= 20, "10-20",
                                    ifelse(TimeSinceFire <= 40, "20-40",
                                           "40-60+")))]
FR_treatments[, Planted := factor(Planted, levels = c("P", "NP"))]

treatments <- merge(FR_treatments, treat_hist, by.x = "ID", by.y ="PlotID")

# Plot data ----------------------------------------------------------------
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

#canopy openness:
canopy <- fread(file.path(in_dir,"fires20_canopy-open.csv")) #check with Jocelyn on NAs - 0 or NA?
canopy <- merge(FR_treatments, canopy, by.y = "site.id", by.x = "PlotID")
canopy <- canopy[, can.close := 100-can.open]


# scale function
scale_fn <- function(var){(var - min(var)) / (max(var) - min(var))}


# Variables ---------------------------------------------------------------------------
# Basal area per hectare (BAPH)
BAtrees <- BAPHlive(A1trees, B1trees)
PlotBAtrees <- BAtrees[,.(BAPH=sum(BAPH)), by="PlotID"]
PlotBAtrees <- merge(PlotBAtrees, FR_treatments, 
                     by = "PlotID",all.y = TRUE)
PlotBAtrees[is.na(BAPH), BAPH:= 0]


# 7.5+, 12.5+, 17.5+, 22.5+ (size classes from BC VRI )
BAdiamClass <- BAPHsizeCl(A1trees, B1trees)
BAdiamClass <- merge(BAdiamClass, FR_treatments)

# Live trees stems per hectare (density)
PlotTree <- TreeDensity(A1trees, B1trees, ClassSize = 2.5) 
PlotTree <- merge(PlotTree, FR_treatments)
RegenTree <- merge(Regen, FR_treatments)

# Snags per hectare
PlotSnags <- SnagDensity(A1trees, B1trees)
PlotSnags <- merge(PlotSnags, FR_treatments)

# CWD HQI
PlotHQI <- cwdQI(cwd)
PlotCWD <- PlotHQI

# CWD volume
CWDvol <- cwdVol(cwd, line) #volume by species & decay class
PlotCWDvol <- CWDvol[,.(VolHa=sum(VolHa)), by="PlotID"]
PlotCWD <- merge(PlotCWD, PlotCWDvol)
PlotCWD <- merge(PlotCWD, FR_treatments)

cwd <- merge(cwd, FR_treatments)

# Treatment explore ----------------------------------------------------------------------

#BA diam class no species (not working the way I want it to):
BA_diams <- BAdiamClass[, .(BAHA = sum(BAPH)), 
                        by = .(PlotID, Planted,TimeSinceFire,TSF,DBH_bin)]
class3_trees <- PlotTree[DBH_bin < 8, .(class_3 = sum(SPH)),
                         by = .(PlotID, Planted, TimeSinceFire)]
class4_trees <- RegenTree[`Live/Dead`=="L", .(class_4 = sum(Tally)),
                          by = .(PlotID, Planted, TimeSinceFire)]
# large live trees
lglive <- PlotTree %>%
  filter(DBH_bin >=20) 

  ggplot(aes(x=DBH_bin, y=SPH)) +
  geom_point(aes(colour=Planted)) +
  facet_grid(rows=vars(Planted))

# large cottonwood or aspen
liveAcAt <- PlotTree %>%
  filter(Species=="Ac" | Species=="At") %>%
  filter(DBH_bin >= 20) 

ggplot(aes(x=DBH_bin, y=SPH)) +
  geom_point(aes(colour=Species)) +
  facet_grid(rows=vars(Planted))

# large snags
snags <- PlotSnags %>% 
  filter(DBH_bin >= 20)

  ggplot(aes(x=DBH_bin, y=SPH)) +
  geom_point(aes(colour=Planted)) +
  facet_grid(rows=vars(Planted))







