#code to start some multivariate looks

# A. Clason

PlotTree_tr

#add CWD things;
tr_cwd <- merge(PlotTree_tr, PlotCWD[,.(PlotID, diamMed, diamMax, DCevenness, HQI, VolHa)],
                by = "PlotID")

tr_cwd_ba <- merge(tr_cwd, PlotBAtrees[,.(PlotID, BAPH)], by = "PlotID",
                   all.x = TRUE)
cols_to_replace <- c("BAPH")
tr_cwd_ba[, (cols_to_replace) := lapply(.SD, function(x) ifelse(is.na(x), 0, x)), 
            .SDcols = cols_to_replace]
tr_cwd_ba[, totSPH := sum(`5`,`10`,`15`,`20`,`25`,
                          `30`,`35`,`40`), by = seq_len(nrow(tr_cwd_ba))]

# Perform NMDS
set.seed(123)  # For reproducibility
nmds_result <- vegan::metaMDS(tr_cwd_ba[,.(`5`,`10`,`15`,`20`,`25`,
                                           `30`,`35`,`40`,diamMed, diamMax, 
                                            HQI, VolHa,BAPH, totSPH)],
                              k = 2, distance = "bray")

nmds_data <- as.data.table(vegan::scores(nmds_result, display = "sites"))
species_scores <- vegan::scores(nmds_result, display = "species")
species_df <- as.data.frame(species_scores)
species_df$Species <- rownames(species_df)

# Combine NMDS scores with the original data
data_with_nmds <- cbind(tr_cwd_ba, nmds_data)

# Plot the NMDS ordination with color and shape by groups
ggplot() +
  geom_point(data = data_with_nmds, shape = 1, 
             aes(x = NMDS1, y = NMDS2, size = TimeSinceFire, colour = Planted))+
  #geom_point(data = species_df, aes(color = Species,x = NMDS1, y = NMDS2))+
  geom_text(data = species_df, aes(x = NMDS1, y = NMDS2,
                                   label = Species, hjust = 1.2), size = 3)
#pca:
pca_pl <- prcomp(tr_cwd_ba[,.(`5`,`10`,`15`,`20`,`25`,
                              `30`,`35`,`40`,diamMed, diamMax, 
                              HQI, VolHa,BAPH, totSPH)], scale. = TRUE)
pca_pl_dt <- data.table(cbind(tr_cwd_ba[,.(PlotID, Planted,TimeSinceFire)],
                               pca_pl$x))
pca_load <- as.data.table(pca_pl$rotation)
pca_load <- cbind(c("5","10","15","20","25","30","35","40","diamMed", "diamMax", 
                    "HQI", "VolHa","BAPH", "totSPH"), pca_load)
setnames(pca_load, "V1", "Variable")

ggplot() +
  geom_jitter(data = pca_pl_dt, aes(x = PC1, y = PC2, colour = Planted,
                                    size = TimeSinceFire),
              shape = 1)+
  #geom_hline(pca_load, yintercept = 0, linetype = "dashed", colour = "gray")+
  #geom_vline(pca_load, yintercept = 0, linetype = "dashed", colour = "gray")+
  geom_text(data = pca_load, aes(x = PC1*10, y = PC2*10, label = Variable), size = 3)+
  theme_minimal()
  geom_segment(data = pca_load, aes(x = PC1, y = PC2, xend = 0, yend = 0), 
               arrow = arrow(type = "closed", length = unit(0.2, "inches")), size = 0.7) 
ggsave(plot = last_plot(), "./Outputs/pca_multivalues.jpg")
  
  #biplot(pca_pl)


## Example with stand structure (dbh class by species) data ---------------------------------
# could look at the size class by species in multivariate space:
plot_dbh_class <- dcast(PlotTree[,.(PlotID,Species,DBH_bin,SPH)], 
                        PlotID + Species ~ DBH_bin,
                        value.var = "SPH", fun = sum)
#get zeroes:
# Sample data with two columns
gr <- data.table(expand.grid(PlotID = unique(plot_dbh_class$PlotID),
                   Species = unique(plot_dbh_class$Species)))


# Merge with the original data to fill in missing values with 0s
cols_to_replace <- c("5","10","15","20","25","30","35","40")
plot_dbh_cl_wide <- merge(gr, plot_dbh_class, by = c("PlotID", "Species"), all.x = TRUE)
plot_dbh_cl_wide[, (cols_to_replace) := lapply(.SD, function(x) ifelse(is.na(x), 0, x)), 
        .SDcols = cols_to_replace]

dbh_dat <- merge(plot_dbh_cl_wide, 
                 FR_treatments[,.(PlotID, Planted, TimeSinceFire)], by = "PlotID",
                 all.y = TRUE)

dbh_dat[, (cols_to_replace) := lapply(.SD, function(x) ifelse(is.na(x), 0, x)), 
   .SDcols = cols_to_replace]
dbh_dat[, Planted := as.factor(Planted)]


pca_dbh <- prcomp(dbh_dat[,.(`5`,`10`,`15`,`20`,`25`,`30`,`35`,`40`)], scale. = TRUE)
pca_dbh_dt <- data.table(cbind(dbh_dat[,.(PlotID, Species,Planted,TimeSinceFire)],
                               pca_dbh$x))

ggplot(pca_dbh_dt, aes(x = PC2, y = PC3, colour = Planted)) +
  geom_jitter(shape = 1, aes(size = TimeSinceFire))+ 
  labs(x = "Principal Component 1", y = "Principal Component 2") +
  scale_colour_discrete()
  scale_shape_ordinal()
  ggtitle("PCA Ordination")

biplot(pca_dbh)
  
 
ggplot2::autoplot(pca_dbh, data = dbh_dat)
  
  # Perform NMDS
  set.seed(123)  # For reproducibility
  nmds_result <- vegan::metaMDS(dbh_dat[,.(`5`,`10`,`15`,`20`,`25`,`30`,`35`,`40`)],
                         k = 2, distance = "bray")
  
  nmds_data <- as.data.table(vegan::scores(nmds_result))
  
  # Combine NMDS scores with the original data
  data_with_nmds <- cbind(dbh_dat, nmds_data)
  
  # Plot the NMDS ordination with color and shape by groups
  ggplot(data_with_nmds, aes(x = sites.NMDS1, y = sites.NMDS2, color = Species)) +
    geom_point(shape = 1, aes(size = TimeSinceFire) )
    labs(x = "NMDS Axis 1", y = "NMDS Axis 2") +
    scale_color_manual(values = c("red", "blue", "green")) +  # Customize colors
    scale_shape_manual(values = c(1, 2, 3)) +  # Customize shapes
    ggtitle("NMDS Ordination with Color and Shape by Group")
  
  
  #MRPP
    mrpp_result <- vegan::mrpp(dbh_dat$Planted, 
                        dbh_dat[,.(`5`,`10`,`15`,`20`,`25`,`30`,`35`,`40`)])
vegdist(dbh_dat[,.(`5`,`10`,`15`,`20`,`25`,`30`,`35`,`40`)])    
    
    
# 1. CWD habitat quality -- using index from van Galen et al. 2019
PlotHQI <- cwdHQI(cwd) #values between 0-100; 0=bad 100=best
PlotMarten <- merge(FR_treatments[,.(PlotID,Planted,TimeSinceFire)], PlotHQI, by = "PlotID")

# 2. CWD clumps/piles
PlotPiles <- cwdPiles(cwd)
PlotPiles <- PlotPiles[,.(PileCount = sum(PileCount)), by = PlotID]
PlotMarten <- merge(PlotMarten, PlotPiles, all.x=TRUE)

# 3. Crown closure >20% and not 100% deciduous (we didn't sample plots that were 100% decid)
# mean of densiometer readings and multiply by 1.04 to get canopy openness
PlotCrown <- densiometer[,.(SubPlotOpen=mean(Densiometer)), by = c("PlotID", "SubPlot")]
PlotCrown <- PlotCrown[,.(CrownOpen=mean(SubPlotOpen)*1.04), by = PlotID]
# convert openness to canopy closure
PlotCrown[, CrownClos:= 100-CrownOpen]
PlotMarten <- merge(PlotMarten, PlotCrown[,.(PlotID,CrownClos)], all.x = TRUE)

# 4. Presence of snags >20cm dbh
PlotSnags <- SnagDensity(A1trees, B1trees)
# only care about presence >20cm, not species, so sum SPH per plot
PlotMarten <- merge(PlotMarten, PlotSnags[DBH_bin >= 20, .(snagSPH_20=sum(snagSPH)), by=PlotID], all.x=TRUE)
PlotMarten[is.na(snagSPH_20), snagSPH_20 := 0]

# 5. Shrub cover (B1= <2m and B2=2-10m shrub heights), B1 > 10%
columnstoadd <- c("PlotID", "ShrubsB1")
PlotMarten[shrubCover, (columnstoadd) := mget(columnstoadd), on = "PlotID"]

for (i in names(PlotMarten))PlotMarten[is.na(get(i)), (i):=0]


#-- FISHER
# fisher require 1) CWD quality similar to marten 2) large diseased and decaying 
# aspen and cottonwood (>30 cm dbh) 3) Ac leading or secondary or tertiary or Sx as 
# only species 4) QMD >=19.6 5) Crown closure >=30%  6) shrub cover > 20%

# 1. CWD quality similar to marten 
# already calculated this for marten
PlotFisher <- merge(FR_treatments[,.(PlotID,Planted,TimeSinceFire)], PlotHQI)

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
PlotFisher <- merge(PlotFisher, QMD, all.x=TRUE)
PlotFisher[is.na(QMD), QMD := 0]

# 5. Crown closure >=30%
# already calculated for marten
PlotFisher<- merge(PlotFisher, PlotCrown[,.(PlotID,CrownClos)], all.x=TRUE)
PlotFisher[is.na(CrownClos), CrownClos := 0]

# 6. shrub cover(0-2m) > 20%
# already calculated for marten (just filter for >=20%)
columnstoadd <- c("PlotID", "ShrubsB1")
PlotFisher[shrubCover, (columnstoadd) := mget(columnstoadd), on = "PlotID"]
PlotFisher[is.na(ShrubsB1), ShrubsB1 := 0]




#-- GOSHAWK
# goshawk require 1) snag and live tree retention, large diameter snags >30cm dbh
# 2) large CWD, >30cm diameter & >7m long 3) clumped/piled CWD

# 1. large snag >30cm and live tree retention post fire
# only care about presence, not species, so sum SPH per plot
PlotGoshawk <- merge(FR_treatments[,.(PlotID, Planted,TimeSinceFire)], 
                     PlotSnags[DBH_bin >= 30, .(snagSPH_30=sum(snagSPH)), by="PlotID"], all.x=TRUE)
PlotGoshawk[is.na(snagSPH_30), snagSPH_30 := 0]

# 2. large CWD >30cm diam / quality CWD
# already calculated for marten & fisher
PlotGoshawk <- merge(PlotGoshawk, PlotHQI)

# 3. CWD piles
PlotGoshawk <- merge(PlotGoshawk, PlotPiles, all.x=TRUE)

#-- SNOWSHOE HARE
# Snowshoe hare require 1) dense canopy cover and 2) shrub cover >30%, 3) tree density >3000 stems/ha, 
# 4) abundant CWD, and 5) browse (Joc)

# 1. Dense canopy cover, minimum 30%
PlotHare <- merge(FR_treatments[,.(PlotID, Planted,TimeSinceFire)], 
                  PlotCrown[,.(PlotID,CrownClos)], all.x = TRUE)
PlotHare[is.na(CrownClos), CrownClos := 0]

# 2. Shrub cover (B1= <2m and B2=2-10m shrub heights), B1 >= 30%
columnstoadd <- c("PlotID", "ShrubsB1")
PlotHare[shrubCover, (columnstoadd) := mget(columnstoadd), on = "PlotID"]
PlotHare[is.na(ShrubsB1), ShrubsB1 := 0]

# 3. Tree density >3000 stems/ha - probably should be larger than a certain dbh?
PlotHare <- merge(PlotHare, PlotTree[,. (SPH_all = sum(SPH)), by=c("PlotID")])
#PlotHare <- PlotHare[SPH <= 3000, SPH := 0]

# 4. Abundant CWD
PlotCWD <- cwdVol(cwd, line)
PlotHare <- merge(PlotHare, PlotCWD[,. (CWDvol=sum(VolHa)), by=c("PlotID")])

# 5. Browse info *Jocelyn*


#-- RED SQUIRRELS
# Red squirrels require 1) tree canopy cover >30%, 2) large conifers >30cm dbh, 
# 3) large snags >40cm, 4) tree composition 50% conifer, 5) 60% spruce preferred, and
# 6) abundant cwd >100m3 

# 1. Tree canopy cover >30%
PlotSquirrel <- merge(FR_treatments[,.(PlotID,Planted,TimeSinceFire)],
                      PlotCrown[,.(PlotID, CrownClos)], all.x = TRUE)
PlotSquirrel[is.na(CrownClos), CrownClos := 0]

# 2. Large conifers >30cm dbh
PlotSquirrel <- merge(PlotSquirrel, PlotTree[Species %in% c("Sx", "Pl", "Bl") & DBH_bin>=30, 
                                             .(lgConSPH=sum(SPH)), by=c("PlotID")], all.x=TRUE)
PlotSquirrel[is.na(lgConSPH), lgConSPH := 0]

# 3. Large snags >40cm dbh
PlotSquirrel <- merge(PlotSquirrel, PlotSnags[DBH_bin>=40, .(snagSPH_40=sum(snagSPH)), by=PlotID], all.x=TRUE)
PlotSquirrel[is.na(snagSPH_40), snagSPH_40:=0]

# 4. Tree composition at least 50% conifer
treeComp <- PlotTree[DBH_bin >=10, totalSPH := sum(SPH), by=PlotID]
treeComp <- treeComp[DBH_bin >=10, .(spComp=sum(SPH)/totalSPH), by=c("PlotID", "Species")]
treeComp <- unique(treeComp)
PlotSquirrel <- merge(PlotSquirrel, treeComp[Species %in% c("Sx", "Pl", "Bl"), 
                                             .(conComp_10 = sum(spComp)), by=PlotID], all.x=TRUE)
PlotSquirrel[is.na(conComp_10), conComp_10 := 0]

# 5. Sx composition at least 60% - intended to have a 10cm DBH cutoff?
PlotSquirrel <- merge(PlotSquirrel, treeComp[Species=="Sx", .(SxComp_10 = sum(spComp)), 
                                             by=PlotID], all.x=TRUE)
PlotSquirrel[is.na(SxComp_10), SxComp_10 := 0]

# 6. CWD >100m3
PlotSquirrel <- merge(PlotSquirrel, PlotCWD[,.(CWDvol=sum(VolHa)), by=PlotID], all.x = TRUE)
#PlotSquirrel[CWDvol < 100, CWDvol := 0]


#-- SMALL MAMMALS (deer mice, southern red backed voles)
# Small mammals require 1) abundant (>100m3/ha) decayed (class 4&5) CWD, 2) high live
# tree basal area


#PlotMarten <- PlotMarten[,.(PlotID, Planted, TimeSinceFire, HQI, CrownOpen, 
#                           CrownClos, snagSPH, ShrubsB1)]
#PlotHare <- PlotHare[,.(PlotID, Planted, TimeSinceFire,CrownOpen, CrownClos, 
#                        ShrubsB1, SPH, CWDvol)]
#PlotFisher <- PlotFisher[,.(PlotID, Planted, TimeSinceFire, HQI, lgAtSPH, decidSx, 
#                            QMD, CrownOpen, CrownClos, ShrubsB1)]
#PlotGoshawk <- PlotGoshawk[,.(PlotID, Planted, TimeSinceFire,snagSPH, HQI, PileCount)]
#PlotSquirrel <- PlotSquirrel[,.(PlotID, Planted, TimeSinceFire, CrownOpen, CrownClos, 
#                               lgConSPH, snagSPH, conComp, SxComp, CWDvol)]

# Get unique column names
#unique_columns <- unique(c(names(PlotMarten), names(PlotHare), names(PlotFisher),
# names(PlotGoshawk), names(PlotSquirrel)))

# Merge data.tables and select unique columns

# all together -------------------------------------------------------------------------
value_dat1 <- merge(PlotMarten, PlotHare, 
                    by = c("PlotID", "Planted", "TimeSinceFire"),
                    all = TRUE, suffixes = c("",""))[, c(unique(c(names(PlotMarten),names(PlotHare)))), 
                                                     with = FALSE]
value_dat2 <- merge(value_dat1, PlotFisher, by = c("PlotID", "Planted", "TimeSinceFire"), 
                    all.y = TRUE, suffixes = c("",""))[, c(unique(c(names(value_dat1),names(PlotFisher)))), 
                                                       with = FALSE]
value_dat3 <- merge(value_dat2, PlotGoshawk, by = c("PlotID", "Planted", "TimeSinceFire"), 
                    all.y = TRUE, suffixes = c("",""))[, c(unique(c(names(value_dat2),names(PlotGoshawk)))), 
                                                       with = FALSE]
value_dat4 <- merge(value_dat3, PlotSquirrel, by = c("PlotID", "Planted", "TimeSinceFire"), 
                    all.y = TRUE, suffixes = c("",""))[, c(unique(c(names(value_dat3),names(PlotSquirrel)))), 
                                                       with = FALSE]
for (i in names(value_dat4))value_dat4[is.na(get(i)), (i):=0]



hab_vars <- names(value_dat4)[!names(value_dat4) %in% c("PlotID","Planted","TimeSinceFire")]

#there's a bunch of NAs to deal with - check codes
value_dat4[, (hab_vars) := lapply(.SD, scale), .SDcols = hab_vars]

pca_hab <- prcomp(value_dat4[, c(hab_vars), with = FALSE], scale. = TRUE)

pca_pl_dt <- data.table(cbind(value_dat4[,.(PlotID, Planted,TimeSinceFire)],
                              pca_hab$x))
pca_load <- as.data.table(pca_hab$rotation)
pca_load <- cbind(hab_vars, pca_load)
#setnames(pca_load, "V1", "Variable")

ggplot() +
  geom_jitter(data = pca_pl_dt, aes(x = PC1, y = PC2, colour = Planted,
                                    size = TimeSinceFire),
              shape = 1)+
  geom_hline(pca_load, yintercept = 0, linetype = "dashed", colour = "gray")+
  #geom_vline(pca_load, yintercept = 0, linetype = "dashed", colour = "gray")+
  geom_text(data = pca_load, aes(x = PC1*10, y = PC2*10, label = hab_vars), size = 3)+
  theme_minimal()
geom_segment(data = pca_load, aes(x = PC1, y = PC2, xend = 0, yend = 0), 
             arrow = arrow(type = "closed", length = unit(0.2, "inches")), size = 0.7) 

#could get PC1 for each habitat variable:
# Fisher -------------------------------------------------------------------------

fisher_vars <- c("HQI", "lgAtSPH", "decidSx","QMD", "CrownClos", "ShrubsB1")
pca_hab <- prcomp(value_dat4[, c(fisher_vars), with = FALSE], scale. = TRUE)

pca_pl_dt <- data.table(cbind(value_dat4[,.(PlotID, Planted,TimeSinceFire)],
                              pca_hab$x))
pca_load <- as.data.table(pca_hab$rotation)
pca_load <- cbind(fisher_vars, pca_load)

ggplot() +
  geom_jitter(data = pca_pl_dt, aes(x = PC1, y = PC2, colour = Planted,
                                    size = TimeSinceFire),
              shape = 1)+
  #geom_hline(pca_load, yintercept = 0, linetype = "dashed", colour = "gray")+
  #geom_vline(pca_load, yintercept = 0, linetype = "dashed", colour = "gray")+
  geom_text(data = pca_load, aes(x = PC1*10, y = PC2*10, label = fisher_vars), size = 3)+
  theme_minimal()

pc1_2 <-  pca_pl_dt[,.(PlotID, Planted, TimeSinceFire, PC1, PC2)]
setnames(pc1_2, c("PC1","PC2"), c("fisher_pc1","fisher_pc2"))
# Marten -------------------------------------------------------------------------

marten_vars <- c("HQI", "PileCount", "snagSPH_20", "CrownClos", "ShrubsB1")
pca_hab <- prcomp(value_dat4[, c(marten_vars), with = FALSE], scale. = TRUE)

pca_pl_dt <- data.table(cbind(value_dat4[,.(PlotID, Planted,TimeSinceFire)],
                              pca_hab$x))
pca_load <- as.data.table(pca_hab$rotation)
pca_load <- cbind(marten_vars, pca_load)

ggplot() +
  geom_jitter(data = pca_pl_dt, aes(x = PC1, y = PC2, colour = Planted,
                                    size = TimeSinceFire),
              shape = 1)+
  #geom_hline(pca_load, yintercept = 0, linetype = "dashed", colour = "gray")+
  #geom_vline(pca_load, yintercept = 0, linetype = "dashed", colour = "gray")+
  geom_text(data = pca_load, aes(x = PC1*10, y = PC2*10, label = marten_vars), size = 3)+
  theme_minimal()

pc1_2 <- cbind(pc1_2,pca_pl_dt[,.(PC1, PC2)])
setnames(pc1_2, c("PC1","PC2"), c("marten_pc1","marten_pc2"))

# Goshawk -------------------------------------------------------------------------

goshawk_vars <- c("HQI", "PileCount", "snagSPH_30")
pca_hab <- prcomp(value_dat4[, c(goshawk_vars), with = FALSE], scale. = TRUE)

pca_pl_dt <- data.table(cbind(value_dat4[,.(PlotID, Planted,TimeSinceFire)],
                              pca_hab$x))
pca_load <- as.data.table(pca_hab$rotation)
pca_load <- cbind(goshawk_vars, pca_load)

ggplot() +
  geom_jitter(data = pca_pl_dt, aes(x = PC1, y = PC2, colour = Planted,
                                    size = TimeSinceFire),
              shape = 1)+
  #geom_hline(pca_load, yintercept = 0, linetype = "dashed", colour = "gray")+
  #geom_vline(pca_load, yintercept = 0, linetype = "dashed", colour = "gray")+
  geom_text(data = pca_load, aes(x = PC1*10, y = PC2*10, label = goshawk_vars), size = 3)+
  theme_minimal()

pc1_2 <- cbind(pc1_2,pca_pl_dt[,.(PC1, PC2)])
setnames(pc1_2, c("PC1","PC2"), c("goshawk_pc1","goshawk_pc2"))

# Hare -------------------------------------------------------------------------

hare_vars <- c("CrownClos", "ShrubsB1", "SPH_all", "CWDvol")
pca_hab <- prcomp(value_dat4[, c(hare_vars), with = FALSE], scale. = TRUE)

pca_pl_dt <- data.table(cbind(value_dat4[,.(PlotID, Planted,TimeSinceFire)],
                              pca_hab$x))
pca_load <- as.data.table(pca_hab$rotation)
pca_load <- cbind(hare_vars, pca_load)

ggplot() +
  geom_jitter(data = pca_pl_dt, aes(x = PC1, y = PC2, colour = Planted,
                                    size = TimeSinceFire),
              shape = 1)+
  #geom_hline(pca_load, yintercept = 0, linetype = "dashed", colour = "gray")+
  #geom_vline(pca_load, yintercept = 0, linetype = "dashed", colour = "gray")+
  geom_text(data = pca_load, aes(x = PC1*10, y = PC2*10, label = hare_vars), size = 3)+
  theme_minimal()

pc1_2 <- cbind(pc1_2,pca_pl_dt[,.(PC1, PC2)])
setnames(pc1_2, c("PC1","PC2"), c("hare_pc1","hare_pc2"))


# Squirrel -------------------------------------------------------------------------

squirrel_vars <- c("CrownClos", "lgConSPH", "snagSPH_40", "conComp_10","SxComp_10","CWDvol")
pca_hab <- prcomp(value_dat4[, c(squirrel_vars), with = FALSE], scale. = TRUE)

pca_pl_dt <- data.table(cbind(value_dat4[,.(PlotID, Planted,TimeSinceFire)],
                              pca_hab$x))
pca_load <- as.data.table(pca_hab$rotation)
pca_load <- cbind(squirrel_vars, pca_load)

ggplot() +
  geom_jitter(data = pca_pl_dt, aes(x = PC1, y = PC2, colour = Planted,
                                    size = TimeSinceFire),
              shape = 1)+
  #geom_hline(pca_load, yintercept = 0, linetype = "dashed", colour = "gray")+
  #geom_vline(pca_load, yintercept = 0, linetype = "dashed", colour = "gray")+
  geom_text(data = pca_load, aes(x = PC1*10, y = PC2*10, label = squirrel_vars), size = 3)+
  theme_minimal()

pc1_2 <- cbind(pc1_2,pca_pl_dt[,.(PC1, PC2)])
setnames(pc1_2, c("PC1","PC2"), c("squirrel_pc1","squirrel_pc2"))

ggplot(pc1_2) +
  geom_point(aes(x = TimeSinceFire, y = fisher_pc1, colour = Planted))+
  geom_smooth(aes(x = TimeSinceFire, y = fisher_pc1, colour = Planted))+
  geom_point(aes(x = TimeSinceFire, y = fisher_pc2, colour = Planted))+
  geom_smooth(aes(x = TimeSinceFire, y = fisher_pc2, colour = Planted))

ggplot(pc1_2) +
  geom_point(aes(x = TimeSinceFire, y = marten_pc1, colour = Planted))+
  geom_smooth(aes(x = TimeSinceFire, y = marten_pc1, colour = Planted))

ggplot(pc1_2) +
  geom_point(aes(x = TimeSinceFire, y = goshawk_pc1, colour = Planted))+
  geom_smooth(aes(x = TimeSinceFire, y = goshawk_pc1, colour = Planted))

ggplot(pc1_2) +
  geom_point(aes(x = TimeSinceFire, y = hare_pc1, colour = Planted))+
  geom_smooth(aes(x = TimeSinceFire, y = hare_pc1, colour = Planted))

ggplot(pc1_2) +
  geom_point(aes(x = TimeSinceFire, y = squirrel_pc1, colour = Planted))+
  geom_smooth(aes(x = TimeSinceFire, y = squirrel_pc1, colour = Planted))

    
    
  