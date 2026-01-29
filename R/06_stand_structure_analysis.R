# Stand structure Analysis
# This script summarizes the data into stand structure variables
# Ingrid Farnell & Alana Clason
# June 8, 2023
# October 2025 - revisiting to have a look at the structure between treatments over time
# January 2026, coming back to it to think about describing the stand structure instead of
#focusing exclusively on the "treatment"

library(data.table)
library(ggplot2)
in_dir <- "01_data_inputs"
out_dir <- "02_prepped_values"


# Import data

files_to_source <- list.files("./R/00-utils/", pattern = "Function", 
                              full.names = TRUE)
sapply(files_to_source, source)

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
#--------------------------------------------------------------------
# Prelim figures

# Basal area
ggplot(PlotBAtrees, aes(x=TimeSinceFire, y=BAPH)) +
  geom_point(aes(colour=Planted),size = 3, alpha = 0.7) +
  geom_smooth(method="loess",aes(colour=Planted,fill=Planted))+
  #geom_smooth(method = "loess", se = FALSE) +
  scale_color_manual(values = c("P" = "forestgreen", "NP" = "goldenrod3"),
                     labels = c("Planted", "Natural regen")) +
  labs(x = "Years since fire", y = expression(Basal~area~(m^2~ha^-1)),
       color = "Treatment") +
  theme_minimal(base_size = 14)


# Basal area diameter classes
ggplot(BAdiamClass, aes(x = DBH_bin, y = BAPH, fill = Species)) +
  geom_col(position = position_dodge(width = 0.8), colour = "grey25", width = 0.7, alpha = 0.9) +
  facet_grid(rows = vars(Planted),
             labeller = labeller(Planted = c("P" = "Planted", "NP" = "Not Planted"))) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    x = "Diameter class (cm)",
    y = expression(Basal~area~(m^2~ha^-1)),
    fill = "Species"
  )

#BA diam class no species (not working the way I want it to):
BA_diams <- BAdiamClass[, .(BAHA = sum(BAPH)), 
                        by = .(PlotID, Planted,TimeSinceFire,TSF,DBH_bin)]

ggplot(BA_diams, aes(x = DBH_bin, y = BAHA, fill = Planted)) +
  geom_col(position = position_dodge(),
           colour = "grey25", width = 0.7, alpha = 0.9) +
  scale_fill_manual(values = c("P" = "#2C7BB6", "NP" = "#D7191C"),
                    labels = c("Planted", "Not Planted")) +
  facet_grid(rows = vars(Planted),cols = vars(TSF))+
  labs(
    x = "Diameter class (cm)",
    y = expression(Basal~area~(m^2~ha^-1)),
    fill = ""
  ) +
  theme_bw(base_size = 12)

ggplot(BA_diams, aes(x = DBH_bin, y = BAHA, fill = Planted)) +
  geom_col(position = position_dodge(),
           colour = "grey25", width = 0.7, alpha = 0.9) +
  scale_fill_manual(values = c("P" = "#2C7BB6", "NP" = "#D7191C"),
                    labels = c("Planted", "Not Planted")) +
  facet_grid(rows = vars(Planted),cols = vars(TSF))+
  labs(
    x = "Diameter class (cm)",
    y = expression(Basal~area~(m^2~ha^-1)),
    fill = ""
  ) +
  theme_bw(base_size = 12)


ggplot(BA_diams, aes(x = factor(DBH_bin), y = BAHA, fill = Planted)) +
  geom_boxplot(colour = "grey25", alpha = 0.8, width = 0.7, outlier.size = 1.5) +
  scale_fill_manual(values = c("P" = "#2C7BB6", "NP" = "#D7191C"),
                    labels = c("Planted", "Not Planted")) +
  facet_grid(rows = vars(Planted), cols = vars(TSF),
             labeller = labeller(Planted = c("P" = "Planted", "NP" = "Not Planted"))) +
  labs(
    x = "Diameter class (cm)",
    y = expression(Basal~area~(m^2~ha^-1)),
    fill = ""
  ) +
  theme_bw(base_size = 12)


#what about canopy closure?
ggplot(canopy, 
       aes(x = TimeSinceFire, y = can.close, color = Planted)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_smooth(method = "loess", se = FALSE) +
  scale_color_manual(values = c("P" = "forestgreen", "NP" = "goldenrod3"),
                     labels = c("Planted", "Natural regen")) +
  labs(x = "Years since fire", y = "Canopy closure (%)",
       color = "Treatment") +
  theme_minimal(base_size = 14)


#basal area of all trees 
ggplot(merge(BAtrees,FR_treatments),aes(x = TimeSinceFire, y = BAPH, group = Planted))+
  geom_point(aes(x = TimeSinceFire, y = BAPH, colour = Planted))+
  geom_smooth(aes(x = TimeSinceFire, y = BAPH, colour = Planted),
              method = "loess", se = FALSE)+
  scale_colour_manual(values = c("P" = "#2C7BB6", "NP" = "#D7191C"),
                    labels = c("Planted", "Not Planted"))+
  labs(
    x = "Years since fire",
    y = expression(Basal~area~(m^2~ha^-1)),
    fill = ""
  ) +
  theme_bw(base_size = 12)

ggplot(merge(BAtrees,FR_treatments),aes(x = TimeSinceFire, y = DBH, group = Planted))+
  geom_point(aes(x = TimeSinceFire, y = DBH, colour = Planted))+
  geom_smooth(aes(x = TimeSinceFire, y = DBH, colour = Planted),
              method = "loess", se = FALSE)+
  scale_colour_manual(values = c("P" = "#2C7BB6", "NP" = "#D7191C"),
                      labels = c("Planted", "Not Planted"))+
  labs(
    x = "Years since fire",
    y = "DBH (cm)",
    fill = ""
  ) +
  theme_bw(base_size = 12)


#how many size class 3 (<7.5cm DBH) & 4 (<1.3m) trees
#sum all stems
class3_trees <- PlotTree[DBH_bin < 8, .(class_3 = sum(SPH)),
                         by = .(PlotID, Planted, TimeSinceFire)]
ggplot(class3_trees[class_3 <30000], 
       aes(x = TimeSinceFire, y = class_3, color = Planted)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_smooth(method = "loess", se = FALSE) +
  scale_color_manual(values = c("P" = "forestgreen", "NP" = "goldenrod3"),
                     labels = c("Planted", "Natural regen")) +
  labs(x = "Years since fire", y = "Sapling density (stems/ha)",
       color = "Treatment") +
  theme_minimal(base_size = 14)

class4_trees <- RegenTree[`Live/Dead`=="L", .(class_4 = sum(Tally)),
                      by = .(PlotID, Planted, TimeSinceFire)]

ggplot(class4_trees[class_4 < 1000], 
       aes(x = TimeSinceFire, y = class_4, color = Planted)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_smooth(method = "loess", se = FALSE) +
  scale_color_manual(values = c("P" = "darkblue", "NP" = "turquoise"),
                     labels = c("Planted", "Natural regen")) +
  labs(x = "Years since fire", y = "Regeneration density (stems/ha)",
       color = "Treatment") +
  theme_minimal(base_size = 14)


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


#the other component I wanted to 

