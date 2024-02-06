# integrate across values
library(data.table)
library(ggplot2)

in_dir <- "02-prepped_values"

#Schwenk et al 2012
#1. overall goal: manage forests for multiple values
#2. set objectives: objectives: 1. store carbon, 2. produce timber/wood products, 3. sustain biodiversity
#3. assign weights
#4. choose management prescriptions
#5. calculate partial utilities
#6. calculate total utilities



#1a. partial utilities carbon and timber

#The partial utility for C storage, UC,i,j, was calculated as the mean annual C stored during the
# 100-year simulation (at site i for prescription j ), divided by the mean annual C for the 
# site-prescription combination with the maximum C storage (considering all four prescriptions).

# mean annual C store not planted/ (max c (planted or not planted)) 

#1b. partial utlities biodiversity

#average occupandy for each bird species
#rescaled occurpancy estimates so species with greater occupancy did not have a disproportionate influence
#then summed across all 51 species

# sum( mean occupancy not planted species 1/ max occupancy for not planted species 1) / max species occupancy
# across all prescriptions (planted or not planted)

#forest_ind_list <- list.files(in_dir, "indi", full.names = TRUE)
#fi <- purrr::map(forest_ind_list, fread)
#fi_dt <- do.call(function(x)merge(x, by = "PlotID"), fi)
#fi_dt <- data.table::rbindlist(fi, use.names = TRUE, fill = TRUE, idcol = "source")

hb <- fread(list.files(in_dir, "hab", full.names = TRUE))
cr <- fread(list.files(in_dir, "carbon", full.names = TRUE))
fl <- fread(list.files(in_dir, "fuels", full.names = TRUE))
tb <- fread(list.files(in_dir, "vol", full.names = TRUE))

hb_cr <- merge(hb, cr, by ="PlotID")
hb_cr_fl <- merge(hb_cr, fl, by ="PlotID")
hb_cr_fl_tb <- merge(hb_cr_fl, tb, by ="PlotID")

FR_treatments <- fread(file.path("01-data_inputs","FR_Treatments.csv"))
FR_treatments[ID == "FR41"|ID == "FR48"|ID == "FR50"|ID == "FR50"|ID == "FR60", 
              under_plant := "Y"][is.na(under_plant), under_plant := "N"]

#Plot treatment cleaning
FR_treatments[,`:=`(PlotID = as.factor(ID), Planted = as.factor(Planted))]
FR_treatments[, TimeSinceFire := 2020 - FIRE_YEAR]
#for this paper, we don't need all the columns:
plot_treatments <- FR_treatments[,.(PlotID, Planted, TimeSinceFire, under_plant)]

hb_cr_fl_tb <- merge(plot_treatments, hb_cr_fl_tb, by = "PlotID")

ind_names <- colnames(hb_cr_fl_tb)[!colnames(hb_cr_fl_tb) %in% c("PlotID",
                                                                 "Planted",
                                                                 "TimeSinceFire",
                                                                 "under_plant")]

ind_table <- melt(hb_cr_fl_tb, id.vars = c("PlotID","Planted","TimeSinceFire","under_plant"),
                  measure.vars = ind_names)

contrasting_colors <- c("#E41A1C", "#377EB8", "#4DAF4A", "#FF7F00", "#FF33CC", "#A65628", 
                        "#984EA3", "#FFD92F", "#66C2A5", "#FC8D62", "#8E7BAA", "#F8766D")

contrasting_colors <- c("#00AFBB", "#E7B800","#0072B2", "#FC4E07","#4DAF4A","#A65628",
                        "#CC79A7","#FF33CC","#8E7BAA","#999999","#E41A1C","#000000")


ggplot(ind_table)+
  geom_point(aes(x = TimeSinceFire, y = value, colour = variable))+
  geom_smooth(aes(x = TimeSinceFire, y = value, colour = variable), alpha = 0.2, method = "gam")+
  labs(color = "Wildlife species")+
  scale_color_manual(labels = c("Marten", "Fisher", "Goshawk", "Hare", "Squirrel",
                                "Small mammal", "Grouse","Total carbon", "Dead carbon",
                                "Live carbon","prob of crown", "merch vol"), 
                     values = contrasting_colors)+
  xlab("Time since fire")+
  ylab("Ecosystem Service Value")+
  facet_wrap(~Planted)

ggplot(ind_table[Planted == "P" & TimeSinceFire < 20])+
  geom_point(aes(x = TimeSinceFire, y = value, colour = variable))+
  geom_smooth(aes(x = TimeSinceFire, y = value, colour = variable), alpha = 0, method = "lm")+
  labs(color = "Wildlife species")+
  scale_color_manual(labels = c("Marten", "Fisher", "Goshawk", "Hare", "Squirrel",
                                "Small mammal", "Grouse","Total carbon", "Dead carbon",
                                "Live carbon","prob of crown", "merch vol"), 
                     values = contrasting_colors)+
  xlab("Time since fire")+
  ylab("Ecosystem Service Value")+
  facet_wrap(~under_plant)

set.seed(123)  # For reproducibility
nmds_result <- vegan::metaMDS(hb_cr_fl_tb[,..ind_names],
                              k = 2, distance = "bray")

nmds_data <- as.data.table(vegan::scores(nmds_result, display = "sites"))
species_scores <- vegan::scores(nmds_result, display = "species")
species_df <- as.data.frame(species_scores)
species_df$Species <- rownames(species_df)

# Combine NMDS scores with the original data
data_with_nmds <- cbind(hb_cr_fl_tb, nmds_data)

# Plot the NMDS ordination with color and shape by groups
ggplot() +
  geom_point(data = data_with_nmds,
             aes(x = NMDS1, y = NMDS2, size = TimeSinceFire, 
                 colour = Planted, shape = factor(under_plant)))+
  #geom_point(data = species_df, aes(color = Species,x = NMDS1, y = NMDS2))+
  geom_text(data = species_df, aes(x = NMDS1, y = NMDS2,
                                   label = Species, hjust = 1.2), size = 3)


library(mgcv)


form <- m_ccp ~ Planted + s(TimeSinceFire)

set.seed(32639)
gm <- mgcv::gam(formula = as.formula(form),
                data = hb_cr_fl_tb,
                family = betar(link = "logit"),
                method = "REML")

# check the appropriateness of Ks
gam.check(gm)
plot(gm, pages = 1, rug = TRUE, shade = TRUE)
summary(gm)


form <- m_ccp ~ Planted + TimeSinceFire
gl <- glm(formula = as.formula(form),
                       data = hb_cr_fl_tb)
summary(gl)





library(betareg)
form <- GrouseHabitat ~ Planted + TimeSinceFire
gl <- betareg::betareg(formula = as.formula(form),
                data = hb_cr_fl_tb)

model <- betareg::betareg(GoshawkHabitat ~ TimeSinceFire, data = hb_cr_fl_tb)
saveRDS(gm, file = file.path(out_dir, "gam_out.rds"))



gamMod <- mgcv::gam(dNBRReSamp~s(PlantAge, k=25), data = dat_csv_dt[FireID=="Chutanli"], 
                    method = "REML", family = "scat")
summary(gamMod) 
gam.check(gamMod)
gamMod <- mgcv::gam(dNBRReSamp~s(PlantAge, k=25), data = dat_csv_dt[FireID=="Island"], 
                    method = "REML", family = "scat")
summary(gamMod)
gam.check(gamMod)
gamMod <- mgcv::gam(dNBRReSamp~s(PlantAge, k=25), data = dat_csv_dt[FireID=="Nadina"], 
                    method = "REML", family = "scat")
gamMod <- mgcv::gam(dNBRCAT~s(PlantAge), data = dat_csv_dt[FireID=="Nadina"], 
                    method = "REML", family = ocat(R=4))
predict(gamMod,dat_csv_dt[FireID=="Nadina",.(dNBRCAT,PlantAge)],type="response",se=TRUE)
summary(gamMod)
gam.check(gamMod)

gamMod <- mgcv::gam(dNBRReSamp~s(PlantAge), data = dat_csv_dt[FireID=="Shovel"], method = "REML")
summary(gamMod)
gamMod <- mgcv::gam(dNBRReSamp~s(PlantAge), data = dat_csv_dt[FireID=="Tezzeron"], method = "REML")
summary(gamMod)
gamMod <- mgcv::gam(dNBRReSamp~s(PlantAge), data = dat_csv_dt[FireID=="Verdun"], method = "REML")
summary(gamMod)









hab_ind <- melt(HabitatIndicies, id.vars = c("PlotID","Planted","TimeSinceFire"),
                measure.vars = c("MartenHabitat", "FisherHabitat", "GoshawkHabitat", 
                                 "HareHabitat", "SquirrelHabitat",
                                 "SmMammalHabitat", "GrouseHabitat"))
setnames(hab_ind, c("variable","value"), c("species","habitat_index"))

#Scale the indices between 0 and 1
scale_fn <- function(var){(var - min(var)) / (max(var) - min(var))}

hab_ind[,hab_ind_sc := scale_fn(habitat_index),by = .(species)]
hab_ind_d <- dcast(hab_ind, PlotID + Planted + TimeSinceFire ~ species,
                   value.var = "hab_ind_sc")
hab_ind_d <- hab_ind_d[,.(PlotID, MartenHabitat, FisherHabitat, GoshawkHabitat, HareHabitat,
                          SquirrelHabitat,SmMammalHabitat, GrouseHabitat)]
write.csv(hab_ind_d, "./02-prepped_values/hab_indi.csv", row.names = F)


custom_color_scale <- c("#1f78b4", "#33a02c", "#e31a1c", "#ff7f00", "#6a3d9a", "#a6cee3", "#b15928")


ggplot(data= hab_ind)+
  geom_point(aes(x = TimeSinceFire, y = hab_ind_sc, colour = species))+
  geom_smooth(aes(x = TimeSinceFire, y = hab_ind_sc, colour = species), alpha = 0,
              method = "gam")+
  labs(color = "Wildlife species")+
  scale_color_manual(labels = c("Marten", "Fisher", "Goshawk", "Hare", "Squirrel",
                                "Small mammal", "Grouse"), 
                     values = custom_color_scale)+
  xlab("Time since fire")+
  ylab("Habitat index")+
  facet_wrap(~Planted)


ggplot(data= hab_ind)+
  geom_boxplot(aes(x = Planted, y = hab_ind_sc, fill = species))






hab_ind <- hab_ind[,.(PlotID, Planted, TimeSinceFire,MartenHabitat, FisherHabitat, GoshawkHabitat,
                      HareHabitat, SquirrelHabitat, SmMammalHabitat)]

hab_vars <- names(hab_ind)[!names(hab_ind) %in% c("PlotID","Planted","TimeSinceFire")]

scale_values <- function(x){(x-min(x))/(max(x)-min(x))}

hab_ind[, (hab_vars) := lapply(.SD, scale_values), .SDcols = hab_vars]

#x = habitat score
part_utils <- function(x){sum(mean(x-np)/ max(x-p))/max(x)}

#fisher:
fisher_np <- (hab_ind[Planted == "NP",
                      mean(FisherHabitat)]/hab_ind[Planted == "P",
                              max(FisherHabitat)])/hab_ind[,max(FisherHabitat)]
fisher_p <- (hab_ind[Planted == "P",
                      mean(FisherHabitat)]/hab_ind[Planted == "NP", 
                            max(FisherHabitat)])/hab_ind[,max(FisherHabitat)]

#marten:
marten_np <- (hab_ind[Planted == "NP",
                      mean(MartenHabitat)]/hab_ind[Planted == "P",
                              max(MartenHabitat)])/hab_ind[,max(MartenHabitat)]
merten_p <- (hab_ind[Planted == "P",
                     mean(MartenHabitat)]/hab_ind[Planted == "NP",
                            max(MartenHabitat)])/hab_ind[,max(MartenHabitat)]

#marten:
marten_np <- (hab_ind[Planted == "NP",
                      mean(MartenHabitat)]/hab_ind[Planted == "P",
                             max(MartenHabitat)])/hab_ind[,max(MartenHabitat)]
merten_p <- (hab_ind[Planted == "P",
                     mean(MartenHabitat)]/hab_ind[Planted == "NP",
                             max(MartenHabitat)])/hab_ind[,max(MartenHabitat)]

#2. Total utility
#weights equal, and then weights unbalanced

# for each site (71) sum(weightC * CpartUtil + weightT * TpartUtil + weightB * BpartUtil)/ number of sites

#we don't need part 3 because we didn't ahve different site types

# maximum utility under each weighting scenario.





#another framework is multifunctionality, but this seems to be an optimization approach - 
# define the ideal and anti-deal for an idicator and than optimize management decision




