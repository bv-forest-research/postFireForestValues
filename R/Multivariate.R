



#get live tree size classes across species
PlotTree_allSp <- PlotTree[, .(SPH = sum(SPH)), by = c("PlotID","DBH_bin")]
PlotTree_allSp_d <- dcast(PlotTree_allSp[,.(PlotID, DBH_bin, SPH)], 
                        PlotID ~ DBH_bin,
                        value.var = "SPH", fun = sum)
#merge with treatment data
PlotTree_tr <- merge(PlotTree_allSp_d, 
                     FR_treatments[,.(PlotID, Planted, TimeSinceFire)], by = "PlotID",
                     all.y = TRUE)
PlotTree_tr[, Planted := as.factor(Planted)]

cols_to_replace <- c("5","10","15","20","25","30","35","40")
PlotTree_tr[, (cols_to_replace) := lapply(.SD, function(x) ifelse(is.na(x), 0, x)), 
                 .SDcols = cols_to_replace]

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
    
    
    
    
    
  