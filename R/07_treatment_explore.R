#stand structure with treatment

library(data.table)
library(tidyr)
library(ggplot2)
library(ggrepel)
library(scico)
in_dir <- "01_data_inputs"
out_dir <- "02_prepped_values"


# Import data

files_to_source <- list.files("./R/00-utils/", pattern = "Function", 
                              full.names = TRUE)
sapply(files_to_source, source)
has <- function(x, pattern) {
  x_chr <- as.character(x)
  x_chr[is.na(x_chr)] <- ""
  grepl(pattern, x_chr, ignore.case = TRUE)
}
# Treatments ---------------------------------------------------------------
treat_hist <- fread(file.path(in_dir,"treatment_history.csv"), na.strings = "n/a")

#dcast(treat_hist[,.(PlotID, pre_post)])
# Treatments ---------------------------------------------------------------
# January 31 - there's still treatment errors to resolve - how can NM == PFO?
FR_treatments <- fread(file.path(in_dir,"FR_Treatments.csv"))

#Plot treatment cleaning
FR_treatments[,`:=`(PlotID = as.factor(ID), Planted = as.factor(Planted))]
FR_treatments[, TimeSinceFire := 2020 - FIRE_YEAR]
#for this paper, we don't need all the columns:
plot_treatments <- FR_treatments[,.(PlotID, Planted, TimeSinceFire)]
plot_treatments[,TSF := ifelse(TimeSinceFire <= 10, "<10",
                             ifelse(TimeSinceFire <= 20, "10-20",
                                    ifelse(TimeSinceFire <= 40, "20-40",
                                           "40-60+")))]
plot_treatments[, Planted := factor(Planted, levels = c("P", "NP"))]

#treatments <- merge(FR_treatments, treat_hist, by.x = "ID", by.y ="PlotID")

jb_treatments <- fread(file.path(in_dir,"jb_treatments.csv"), na.strings = "n/a")

jb_treatments[, `:=`(
  Treatment = factor(Treatment),
  TSF = as.integer(`Time since fire`),
  dNBR = factor(`dNBR severity`,
                levels = c("unburned", "Low", "moderate", "High"),
                ordered = TRUE),
  field_severity = factor(`Field severity`,
                          levels = c("Low", "low-moderate", "Moderate",
                                     "moderate-high", "High"),
                          ordered = TRUE)
)]
jb_treatments[, `:=`(
  pre_harvested   = has(`Pre-fire forest management`, "harvest"),
  pre_planted     = has(`Pre-fire forest management`, "plant"),
  pre_site_prep   = has(`Pre-fire forest management`, "site prep|mechanical|burned"),
  pre_thinned     = has(`Pre-fire forest management`, "thin"),
  pre_brushed     = has(`Pre-fire forest management`, "brush|chemical")
)]

jb_treatments[, `:=`(
  post_harvested  = has(`Post-fire forest management`, "harvest|salvage"),
  post_planted    = has(`Post-fire forest management`, "plant|underplant"),
  post_site_prep  = has(`Post-fire forest management`, "site prep|mechanical|burned"),
  post_thinned    = has(`Post-fire forest management`, "thin"),
  post_piled      = has(`Field comments`, "pile"),
  post_burned     = has(`Field comments`, "burn")
)]
jb_treatments[, `:=`(
  any_pre_management  = pre_harvested | pre_planted | pre_site_prep | pre_thinned,
  any_post_management = post_harvested | post_planted | post_site_prep | post_thinned,
  
  post_disturbance_intensity = fifelse(
    post_harvested | post_site_prep, "High",
    fifelse(post_planted, "Moderate", "Low")
  )
)]
jb_treatments[, post_disturbance_intensity :=
          factor(post_disturbance_intensity,
                 levels = c("Low", "Moderate", "High"),
                 ordered = TRUE)]
jb_treatments[, management_class :=
          fcase(
            any_post_management & !any_pre_management, "Post-fire only",
            any_pre_management & any_post_management,  "Pre + Post fire",
            any_pre_management & !any_post_management, "Pre-fire only",
            default = "No management"
          )
]

jb_treatments[, management_class := 
                factor(ifelse(management_class == "No management","NM",
                              ifelse(management_class == "Post-fire only","PFO",
                                     "SM")))]
setnames(jb_treatments, "Site ID", "PlotID")
fwrite(jb_treatments, file.path(out_dir, "jb_treatments.csv"))

#treat3 <- fread(file.path(in_dir,"treatment_history_simp.csv"), na.strings = "n/a")
# List of harvest types that count as wood product harvests
#wood_harvest_types <- c("CLEAR", "CCRES")

# Function to check if any harvest occurred after fire
#treat3[, post_fire_harvest := FALSE]

# Loop over each harvest column set
#for (i in 1:4) {
  #yr_col   <- paste0("harvest", i, "_yr")
  #type_col <- paste0("harvest", i, "_type")
  
  # Update post_fire_harvest if harvest type matches and year > fire year
  #treat3[!is.na(get(yr_col)) & 
   #    get(type_col) %in% wood_harvest_types & 
  #     get(yr_col) > fire_yr, 
 #    post_fire_harvest := TRUE]
#}

# Quick check
#treat3[, .(PlotID, fire_yr, harvest1_yr, harvest1_type, harvest2_yr, harvest2_type, post_fire_harvest)]

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
setnames(canopy, "site.id", "PlotID")
#canopy <- merge(FR_treatments, canopy, by.y = "site.id", by.x = "PlotID")
canopy <- canopy[, can.close := 100-can.open]


# scale function
scale_fn <- function(var){(var - min(var)) / (max(var) - min(var))}


# Variables ---------------------------------------------------------------------------
# Basal area per hectare (BAPH)
BAtrees <- BAPHlive(A1trees, B1trees) #use for all individual metrics
PlotTree <- TreeDensity(A1trees, B1trees, ClassSize = 2)  #use for live tree density by class
PlotSnags <- SnagDensity(A1trees, B1trees, ClassSize = 2) #use for dead tree density by class
regen_trees <- RegenDensity(Regen) # takes the mean of regen sub-plots
setnames(regen_trees, c("mnSPH","sdSPH"), c("regen_mnSPH","regen_sdSPH"))
PlotHQI <- cwdQI(cwd)
live_trees <- rbindlist(list(A1trees, B1trees),use.names = TRUE, fill = TRUE)
live_trees <- live_trees[Tree_class < 3]

#anything using live-trees needs to combine sub-plots before plots!
live_trees[ , `:=`(BA = pi * (DBH / 200)^2, 
             Crown_length = Height - Crown_base_height,
             LCR = fifelse(!is.na(Height) & !is.na(Crown_base_height) & Height > 0,
                           (Height - Crown_base_height) / Height,
                           NA_real_))]

#define dominant trees (not sure this will work for young stands):
#none of this is per hectare, so i think i just ignore plots?
live_trees[, DBH_q75 := quantile(DBH, 0.75, na.rm = TRUE), by = PlotID]
live_trees[, canopy_class := fifelse(DBH >= DBH_q75, "dominant", "sub_canopy")]

diam_metrics <- live_trees[!is.na(DBH),
                           .(n_trees_dbh = .N,
                             dbh_cv = sd(DBH, na.rm = TRUE) / mean(DBH, na.rm = TRUE),
                             dom_dbh = quantile(DBH, 0.75, na.rm = TRUE)  # dominant height proxy
),
by = PlotID]

#describing the stand development stage from data:
height_metrics <- live_trees[!is.na(Height),.(n_trees_hgt = .N,
    height_cv = sd(Height, na.rm = TRUE) / mean(Height, na.rm = TRUE),
    dom_height = quantile(Height, 0.75, na.rm = TRUE)  # dominant height proxy
  ),
  by = PlotID]

lcr_metrics <- live_trees[!is.na(LCR),.(prop_low_LCR = mean(LCR < 0.4, na.rm = TRUE)
                                       ),by = PlotID]
cbh_sep <- live_trees[
  !is.na(DBH) & !is.na(Crown_base_height),
  {
    q25 <- quantile(DBH, 0.25, na.rm = TRUE)
    q75 <- quantile(DBH, 0.75, na.rm = TRUE)
    
    cbh_top <- mean(Crown_base_height[DBH >= q75], na.rm = TRUE)
    cbh_bot <- mean(Crown_base_height[DBH <= q25], na.rm = TRUE)
    
    .(cbh_sep = cbh_top - cbh_bot)
  },
  by = PlotID
]

devel_metrics <- Reduce(
  function(x, y) merge(x, y, by = "PlotID", all = TRUE),
  list(height_metrics, diam_metrics, lcr_metrics, cbh_sep)
)
# Thresholds
dom_height_thresh <- 8        # meters, dominant height for canopy-only
height_cv_thresh <- 0.25      # low variability cutoff
prop_low_LCR_thresh <- 0.1    # minimal sub-canopy
cbh_sep_thresh <- 1.5         # vertical separation to indicate stratification
n_criteria_stratified <- 2    # number of criteria required for emerging/stratified

devel_metrics[, `:=`(
  crit_height_var   = height_cv > height_cv_thresh,
  crit_suppression  = prop_low_LCR >= 0.2,       # original stratification threshold
  crit_cbh_sep      = cbh_sep > cbh_sep_thresh,
  crit_dom_height   = dom_height >= dom_height_thresh
)]
devel_metrics[, n_criteria_met := crit_height_var + 
                crit_suppression + 
                crit_cbh_sep + 
                crit_dom_height]

devel_metrics[, structure_stage := fcase(
  n_criteria_met >= n_criteria_stratified, "Emerging_or_stratified",
  dom_height >= dom_height_thresh & 
    prop_low_LCR < prop_low_LCR_thresh & 
    height_cv < height_cv_thresh,
  "Canopy_only_single_layer",
  default = "Undifferentiated_single_layer"
)]



#who is in this canopy cohort?
#live_trees <- rbindlist(list(A1trees, B1trees),use.names = TRUE, fill = TRUE)
#live_trees <- live_trees[Tree_class < 3]
live_trees <- assign_PHF(live_trees)

live_trees[, DBH_q75 := quantile(DBH, 0.75, na.rm = TRUE), by = PlotID]
live_trees[, dbh_class := fifelse(DBH >= DBH_q75, "dominant", "sub_dom")]
live_trees[, HGT_q75 := quantile(Height, 0.75, na.rm = TRUE), by = PlotID]
live_trees[, HGT_q25 := quantile(Height, 0.25, na.rm = TRUE), by = PlotID]
live_trees[, hgt_class := ifelse(Height >= HGT_q75, "dominant",
                                 ifelse(Height <= HGT_q25, "suppressed",
                                        "sub-dom"))]
live_trees[, BAPH := (pi * (DBH / 200)^2)*PHF]
live_trees_metrics <- Reduce(
  function(x, y) merge(x, y, by = "PlotID", all = TRUE),
  list(live_trees,plot_treatments)
)
dbh_counts <- live_trees_metrics[, .(SPH = sum(PHF),
                                     BA = sum(BAPH)),
                                 by = .(PlotID, dbh_class, TSF, Planted)]
hgt_counts <- live_trees_metrics[, .(SPH = sum(PHF),
                                     BA = sum(BAPH)),
                                 by = .(PlotID, hgt_class, TSF, Planted)]
# Layer 1 (Mature): Trees => 12.5cm dbh.
# Layer 2 (Pole): Trees => 7.5 cm and <12.5 cm dbh
# Layer 3 (Sapling): Trees >1.3m height and <7.5 cm dbh
#Layer 4 (Regeneration): Trees < = 1.3 m heigh

#density and ba by layer

live_trees[, Layer := fifelse(DBH >= 12.5, "Layer1_Mat",
                              fifelse(DBH >= 7.5 & DBH < 12.5, "Layer2_Pole",
                                      fifelse(Height >= 1.3 & DBH < 7.5, "Layer3_Sapl",
                                              "Layer4_Regen")))]
layer_summary_sub <- live_trees[, .(
  SPH_sub = sum(PHF),           # stems per hectare scaling
  BA_sub  = sum(BAPH)           # basal area per hectare
), by = .(PlotID, `Sub-plot`, Species, Layer)]

layer_summary_plot <- layer_summary_sub[, .(
  SPH = mean(SPH_sub),
  BA  = mean(BA_sub)
), by = .(PlotID, Species, Layer)]
layer_summary_plot[, sp_layer := paste(Species, Layer, sep = "_")]

# get layer 4s:
live_regen <- Regen[`Live/Dead`=="L"]
live_regen[,PHF:= 
            ifelse(`Sub-Plot`=="A1",
                   100,
                   200)][,SPH := Tally*PHF][,SdlHgt:= 
                                              ifelse(`Height_class(cm)` =="0-30",
                                                     1,2)]
subplot_regen <- live_regen[, .(sumSPH = sum(SPH)),
                           by = c("PlotID","Sub-Plot", "Species")]
live_regen_plot <- subplot_regen[,.(SPH = mean(sumSPH)),
                           by=c("PlotID", "Species")]

live_regen_plot[, sp_layer := paste(Species, "Layer4_Regen", sep = "_")]

regen_sph_wide <- dcast(
  live_regen_plot,
  PlotID ~ sp_layer,
  value.var = "SPH",
  fill = 0
)


sph_wide <- dcast(
  layer_summary_plot,
  PlotID ~ sp_layer,
  value.var = "SPH",
  fill = 0
)
ba_wide <- dcast(
  layer_summary_plot,
  PlotID ~ sp_layer,
  value.var = "BA",
  fill = 0
)
setnames(
  sph_wide,
  old = names(sph_wide)[-1],
  new = paste0("SPH_", names(sph_wide)[-1])
)

setnames(
  ba_wide,
  old = names(ba_wide)[-1],
  new = paste0("BA_", names(ba_wide)[-1])
)
str_layers_wide <- merge(
  sph_wide,
  ba_wide,
  by = "PlotID",
  all = TRUE
)

tree_regen_layers <- merge(str_layers_wide, 
                           regen_sph_wide, by = "PlotID",
                           all.y = TRUE)
tree_regen_layers <- tree_regen_layers[
  ,lapply(.SD, function(x) fifelse(is.na(x), 0, x)),
  .SDcols = colnames(tree_regen_layers)[colnames(tree_regen_layers) !="PlotID"]
]
#fwrite(str_layers_wide, file.path(out_dir, "str_layers_wide.csv"))

#single layer = Tree crowns occupy largely the same vertical space
#   Height differences exist but do not translate into persistent crown stratification
#   Suppression is weak, transient, or absent

#emerging_or_stratified = A stand where:
# Vertical differentiation is functionally meaningful
# Crowns of dominant trees are separating upward
# A suppressed or subordinate cohort is forming and persists

#A stand where:A dominant canopy is fully developed Crown closure is high,
# There is little to no persistent sub-canopy,
# Vertical structure has collapsed into one dominant layer

#just to look at this a bit more
site_devel <- merge(jb_treatments[,.(PlotID, Treatment, TSF, field_severity,
                       management_class)], 
                    devel_metrics[,.(PlotID, n_trees_hgt, n_trees_dbh, height_cv,
                       dom_height, prop_low_LCR,structure_stage)],
                    by = "PlotID", all.x = TRUE)

#check whether sites with Na have no trees:
live_trees[PlotID %in% site_devel[is.na(structure_stage)]$PlotID] #true
unique(Regen[PlotID %in% site_devel[is.na(structure_stage)]$PlotID]$PlotID)
unique(site_devel[is.na(structure_stage)]$PlotID)
#these are the initiating stands
site_devel[is.na(structure_stage), structure_stage := "initiating"]








#stand metrics (Lilles et al):
# large trees (live stems ha-1 >50 cm DBH)
# snags (dead stems ha-1 >10 cm DBH)
# regenerating trees (live stems ha-1 <1.3 m DBH)

# horizontal heterogeneity:
# standard deviation (SD) of canopy openness and basal area


#SD of DBH represented size variability of individual trees, 
#typically higher in older forests (Spies and Franklin 1991)
#and was calculated from all trees >10 cm DBH per stand.

#the number of tree species with DBH >10 cm.

# complexity index, that combined several attributes into one metric: 
# the top-height of each stands, multiplied by the live basal area ha-1, stems ha-1
# and species richness (Newman and Starlinger 2001).

live_ba <- BAtrees[, .(live_ba = sum(BAPH)), by = PlotID]
live_ba_sp <- BAtrees[, .(live_ba = sum(BAPH)), by = .(PlotID, Species)]
live_ba_pl <- live_ba_sp[Species == "Pl",.(live_ba_pl = sum(live_ba)), by = PlotID]
live_ba_sx <- live_ba_sp[Species == "Sx",.(live_ba_bl = sum(live_ba)), by = PlotID]
live_ba_bl <- live_ba_sp[Species == "Bl",.(live_ba_sx = sum(live_ba)), by = PlotID]
live_ba_at <- live_ba_sp[Species == "At",.(live_ba_at = sum(live_ba)), by = PlotID]
stems_ha <- BAtrees[, .(stems_ha = sum(PHF)), by = PlotID]
large_trees <- BAtrees[DBH > 30, .(large_trees_ha = sum(PHF)), by = PlotID]
all_snags <- PlotSnags[, .(snags_ha = sum(snagSPH)), by = PlotID]
large_snags <- PlotSnags[DBH_bin >= 10, .(large_snags_ha = sum(snagSPH)),
                         by = PlotID]
tree_sp_richness <- BAtrees[,.(tree_sp_richness = uniqueN(Species)), 
                            by = PlotID]
regen_sp_richness <- Regen[Tally > 0,.(regen_sp_richness = uniqueN(Species)),
                           by = PlotID]
ba_sd <- BAtrees[,.(sd_ba = sd(BAPH, na.rm = TRUE)), by = PlotID]
dbh_sd <- BAtrees[, .(sd_dbh = sd(DBH, na.rm = TRUE)), by = PlotID]
dbh_qmd <- BAtrees[, .(qmd_dbh = sqrt(sum(DBH^2)/.N)),
                   by = PlotID]
top_height <- BAtrees[,.(top_height = max(Height, na.rm = TRUE)),
                      by = PlotID]
sd_height <- BAtrees[,.(sd_hgt = sd(Height, na.rm = TRUE)),
                     by = PlotID]
mn_lcr <- live_trees[,.(mn_lcr = mean(LCR, na.rm = TRUE)), by = PlotID]
mn_lcr_sp <- live_trees[,.(mn_lcr = mean(LCR, na.rm = TRUE)), by = .(PlotID,Species)]
sd_lcr <- live_trees[,.(sd_lcr = sd(LCR, na.rm = TRUE)), by = PlotID]

#what about something that could be managed for like stand density (how)
# do people describe this? over a certain dbh? and this relates to how quickly
# stands reach self-thinning

#what about live crown ratio

#do we see evidence of vertical stratification? i.e. different height classes?
#old growth would have stems of all sizes - presumably in height class as well as dbh

#odum - species richness and evenness should increase over time as pioneers lose dominance


plot_structure <- Reduce(
  function(x, y) merge(x, y, by = "PlotID", all = TRUE),
  list(
    large_trees,
    stems_ha,
    all_snags,
    large_snags,
    regen_trees,
    ba_sd,
    dbh_sd,
    dbh_qmd,
    tree_sp_richness,
    regen_sp_richness,
    live_ba,
    top_height,
    PlotHQI,
    canopy,
    sd_height,
    mn_lcr,
    sd_lcr,
    live_ba_bl,
    live_ba_pl,
    live_ba_sx, 
    live_ba_at,
    Cover,
    site_devel[,.(PlotID, management_class,TSF,
                  Treatment,
                  structure_stage,
                  field_severity)]#,
    #plot_treatments
  )
)
plot_structure
live_trees[PlotID %in% plot_structure[is.na(stems_ha)]$PlotID] #no trees

plot_structure[is.na(stems_ha), stems_ha := 0]
plot_structure[is.na(snags_ha), snags_ha := 0]
plot_structure[is.na(large_snags_ha), large_snags_ha := 0]
plot_structure[is.na(live_ba), live_ba := 0]
plot_structure[is.na(tree_sp_richness), tree_sp_richness := 0]
plot_structure[is.na(regen_sp_richness), regen_sp_richness := 0]
plot_structure[is.na(live_ba_pl), live_ba_pl := 0]
plot_structure[is.na(live_ba_bl), live_ba_bl := 0]
plot_structure[is.na(live_ba_sx), live_ba_sx := 0]
plot_structure[is.na(live_ba_at), live_ba_at := 0]
#fwrite(plot_structure, file.path(out_dir, "plot_structure.csv"))

lcr_treatment <- Reduce(
  function(x, y) merge(x, y, by = "PlotID", all = TRUE),
  list(
    mn_lcr_sp,
    plot_treatments))
lcr_treatment[TimeSinceFire > 50,.(mean(mn_lcr, na.rm = TRUE),
              sd(mn_lcr, na.rm = TRUE)),
              by = .(Species, Planted)]   

#fwrite(plot_structure, file.path(out_dir, "plot_structure.csv"))


struct_vars <- c(
  "large_trees_ha",
  "stems_ha",
  "snags_ha",
  "large_snags_ha",
  "regen_mnSPH",
  "regen_sdSPH",
  "sd_ba",
  "sd_dbh",
  "qmd_dbh",
  "tree_sp_richness",
  "regen_sp_richness",
  "live_ba",
  "top_height",
  "sd_hgt",
  "CQI",
  "can.open",
  "mn_lcr",
  "sd_lcr"
)
scale_dt <- plot_structure[
  ,
  lapply(.SD, function(x) fifelse(is.na(x), 0, x)),
  .SDcols = struct_vars
]
scale_m <- scale(scale_dt, center = TRUE, scale = TRUE)
scale_dt <- as.data.table(scale_m)
scale_dt[, PlotID := plot_structure$PlotID]
plot_str_sc <- Reduce(
  function(x, y) merge(x, y, by = "PlotID", all = TRUE),
  list(
    scale_dt,
    site_devel[,.(PlotID, management_class,
                  structure_stage,
                  field_severity)],
    plot_treatments
  )
)
plot_long <- melt(plot_str_sc, 
                  id.vars = c("PlotID", "TSF", "Planted", "TimeSinceFire",
                              "management_class", "structure_stage", "field_severity"), 
                  measure.vars = struct_vars,
                  variable.name = "StructureVar",
                  value.name = "Value")



pca <- prcomp(scale_m, center = FALSE, scale. = FALSE)
scores <- as.data.table(pca$x[, 1:2])
scores[, PlotID := plot_structure$PlotID]
scores <- scores[
  plot_structure,
  on = "PlotID"
]

loadings <- as.data.table(pca$rotation[, 1:2], keep.rownames = "variable")
pretty_names <- c(
  large_trees_ha      = "Large trees (>30 cm DBH)",
  stems_ha            = "Live stems per ha",
  snags_ha      = "Snags per ha",
  large_snags_ha = "Snags (>10 cm DBH) per ha",
  regen_mnSPH = "Regenerating trees (<1.3 m)" ,
  regen_sdSPH = "Regenerating trees (<1.3 m) variability (SD)",
  sd_ba    = "Basal area (SD)", 
  sd_dbh   = "DBH variability (SD)",
  qmd_dbh    = "Quadratic mean diameter",
  tree_sp_richness = "Tree species richness",
  regen_sp_richness = "Seedling species richness",
  live_ba = "Live basal area",
  top_height = "Top height",
  CQI = "Coarse woody debris index",
  can.open = "Canopy openness"
)

loadings[, variable := pretty_names[variable]]


#library(vegan)
#sim_res <- simper(sim_data, group = plot_structure$management_class)





ggplot(scores, aes(PC1, PC2)) +
  
  ## 1) Filled ellipses for TSF (background layer)
  stat_ellipse(
    aes(fill = TSF, group = TSF),
    geom = "polygon",
    alpha = 0.18,
    colour = NA,
    level = 0.68
  ) +
  
  ## 2) Outline ellipses for Planted (foreground structure)
  stat_ellipse(
   aes(linetype = Treatment, group = Treatment),
    colour = "black",
    linewidth = 0.9,
    level = 0.68
  ) +
  
  ## 2) Outline ellipses for Planted (foreground structure)
 # stat_ellipse(
  #  aes(linetype = management_class, group = management_class),
  #  colour = "grey",
 #   linewidth = 0.9,
 #   level = 0.68
 # ) +
  
  ## 2) Outline ellipses for Planted (foreground structure)
#  stat_ellipse(
 #   aes(linetype = structure_stage, group = structure_stage),
#    colour = "black",
#    linewidth = 0.9,
#    level = 0.68
#  ) +
  
  ## 3) Site scores
  geom_point(
    aes(colour = TSF, shape = Treatment),
    size = 3,
    alpha = 0.85
  ) +
  

  ## 4) Faded PCA loading vectors
  geom_segment(
    data = loadings,
    aes(x = 0, y = 0, xend = PC1 * 3, yend = PC2 * 3),
    arrow = arrow(length = unit(0.15, "cm")),
    colour = "grey30",
    alpha = 0.55,
    linewidth = 0.8
  ) +
  
  geom_text_repel(
    data = loadings,
    aes(x = PC1 * 3.3, y = PC2 * 3.3, label = variable),
    size = 4,
    colour = "grey20",
    box.padding = 0.15,
    point.padding = 0.2,
    segment.color = "grey50",
    segment.size = 0.4,
    max.overlaps = Inf
  ) +
  ## Linetype control for planting
  scale_linetype_manual(
    values = c("P" = "dashed", "NR" = "solid")
 ) +
  
# scale_linetype_manual(
#    values = c("Pre + Post fire" = "dashed",
#               "Post-fire only" = "solid",
 #              "No management" = "dotted")
#  ) +
  scale_fill_scico_d(palette = "berlin")+
  
  scale_colour_scico_d(palette = "berlin")+
  
 # scale_fill_manual(
#    values = wes_palette("Darjeeling2", n = length(unique(scores$TSF)))
#  ) +
 # scale_colour_manual(
  #  values = wes_palette("Darjeeling2", n = length(unique(scores$TSF)))
  #)+
  
  theme_minimal(base_size = 15) +
  labs(
    x = paste0("PC1 (", round(100 * summary(pca)$importance[2,1], 1), "% variance)"),
    y = paste0("PC2 (", round(100 * summary(pca)$importance[2,2], 1), "% variance)")
  )+
  theme(
    axis.title = element_text(size = 16),
    axis.text  = element_text(size = 13),
    legend.title = element_text(size = 14),
    legend.text  = element_text(size = 12)
  )



stage_counts <- plot_structure[management_class != "SM",
                               .N,by = .(management_class, TSF, structure_stage)]
stage_counts[, propN := N / sum(N), by = .(management_class, TSF)]
stage_counts[, structure_stage := factor(structure_stage,
                                         levels = c("initiating",
                                                    "Undifferentiated_single_layer",
                                                    "Emerging_or_stratified",
                                                    "Canopy_only_single_layer"))]

 

ggplot(stage_counts, aes(x = TSF, y = propN, fill = structure_stage)) +
  
  # 1) Bars
  geom_col(alpha = 0.7) +
  
  # 2) Fill palette 
  scale_fill_scico_d(palette = "berlin") +
  
  # 3) Labels
  labs(
    x = "Time Since Fire (years)",
    y = "proportion of plots",
    fill = "Structural stage",
    title = "Structural stage distribution by TSF and whether it had post-fire management"
  ) +
  
  # 4) Facet by Planted/Not Planted
  facet_wrap(~Planted, labeller = labeller(Planted = c("P" = "Planted",
                                                       "NP" = "Not Planted"))) +
  
  #facet_wrap(~management_class, labeller = labeller(Planted = c("NM" = "No Management",
   #                                                    "PFO" = "Post-fire Management"))) +
  
  # 5) Theme
  theme_minimal(base_size = 15) +
  theme(
    axis.title = element_text(size = 16),
    axis.text.x = element_text(size = 12, angle = 0, hjust = 0.5),
    axis.text.y = element_text(size = 13),
    legend.title = element_text(size = 14),
    legend.text  = element_text(size = 12),
    legend.position = "bottom"  # move legend to bottom
  )

ggplot(plot_long, 
       aes(x = TimeSinceFire, y = Value, colour = Planted)) +
  
  # Points
  geom_point(alpha = 0.7, size = 2) +
  
  # Optional: add trend line by Treatment
  geom_smooth(aes(group = Planted, linetype = Planted), 
              method = "loess", se = FALSE, linewidth = 0.8) +
  
  # Facet by structure variable
  facet_wrap(~StructureVar, scales = "free_y", ncol = 4) +
  
  # Colors & linetypes
  scale_colour_scico_d(palette = "berlin") +
  scale_linetype_manual(values = c("P" = "dashed", "NP" = "solid")) +
  
  # Labels
  labs(
    x = "Time Since Fire (years)",
    y = "Value",
    colour = "Treatment",
    linetype = "Treatment",
    title = "Structural variables vs TSF by Treatment"
  ) +
  
  # Theme
  theme_minimal(base_size = 14) +
  theme(
    axis.title = element_text(size = 15),
    axis.text = element_text(size = 12),
    legend.position = "bottom",
    strip.text = element_text(size = 12)
  )


#could use slope to say that live ba, canopy closure etc. is faster in P



ggplot(plot_structure) +
  
  # 1) Points colored by structural stage
  geom_point(aes(x = TimeSinceFire, y = can.close, 
                 colour = structure_stage, shape = management_class), 
             size = 3, alpha = 0.85) +
  
  # 2) Smooth trend lines grouped by Planted
  geom_smooth(aes(x = TimeSinceFire, y = can.close, 
                  linetype = management_class, group = management_class), 
              method = "lm", se = FALSE, linewidth = 1,
              colour = "black") +
  
  # 3) Colors and linetypes
  scale_colour_scico_d(palette = "berlin") +
  #scale_linetype_manual(values = c("NM" = "dashed", "PFO" = "solid")) +
  scale_linetype_manual(values = c("NM" = "dashed", "PFO" = "solid",
                                   "SM" = "dotted")) +
  # 4) Labels
  labs(
    x = "Time Since Fire (years)",
    y = "live ba",
    colour = "Structural stage",
    # linetype = "Management class",
    title = "Canopy closure vs TSF by structural stage and management"
  ) +
  
  # 5) Theme to match your PCA style
  theme_minimal(base_size = 15) +
  theme(
    axis.title = element_text(size = 16),
    axis.text  = element_text(size = 13),
    legend.title = element_text(size = 14),
    legend.text  = element_text(size = 12)
  )





#plot_structure[, CI := top_height * stems_ha * tree_sp_richness * dbh_qmd,
 #              by = "PlotID"]
#dataset just has very few large trees
#CI := top_height * live_ba_ha * stems_ha * species_richness * QMD

#Metrics were normalized by dividing each stand’s value by the 
# maximum value among stands for presentation on one figure.

plot_long_sp <- melt(plot_structure, 
                  id.vars = c("PlotID", "TSF", "Planted", "TimeSinceFire",
                              "management_class", "structure_stage", "field_severity"), 
                  measure.vars = c("live_ba_pl","live_ba_bl","live_ba_sx","live_ba_at"),
                  variable.name = "StructureVar",
                  value.name = "Value")

ggplot(plot_long_sp, 
       aes(x = TimeSinceFire, y = Value, colour = Planted)) +
  
  # Points
  geom_point(alpha = 0.7, size = 2) +
  
  # Optional: add trend line by Treatment
  geom_smooth(aes(group = Planted, linetype = Planted), 
              method = "lm", se = FALSE, linewidth = 0.8) +
  
  # Colors & linetypes
  scale_colour_scico_d(palette = "berlin") +
  scale_linetype_manual(values = c("P" = "dashed", "NP" = "solid")) +
  #scale_linetype_manual(values = c("NM" = "dashed", "PFO" = "solid",
                                #   "SM" = "dotted")) +
  facet_wrap(~StructureVar)+
  # Labels
  labs(
    x = "Time Since Fire (years)",
    y = "live_ba",
    colour = "Treatment",
    linetype = "Treatment",
    title = "live ba vs TSF by Treatment"
  ) +
  
  # Theme
  theme_minimal(base_size = 14) +
  theme(
    axis.title = element_text(size = 15),
    axis.text = element_text(size = 12),
    legend.position = "bottom",
    strip.text = element_text(size = 12)
  )
  

#other cover:

plot_long_cov <- melt(plot_structure, 
                  id.vars = c("PlotID", "TSF", "Planted", "TimeSinceFire",
                              "management_class", "structure_stage", "field_severity"), 
                  measure.vars = c("Graminoids", "Ferns",
                  "CHAMANG", "Other_Herbs", "Total_Herbs", "ShrubsB2", "ShrubsB1"),
                  variable.name = "StructureVar",
                  value.name = "Value")
ggplot(plot_long_cov, 
       aes(x = TimeSinceFire, y = Value, colour = Planted)) +
  
  # Points
  geom_point(alpha = 0.7, size = 2) +
  
  # Optional: add trend line by Treatment
  geom_smooth(aes(group = Planted, linetype = Planted), 
              method = "loess", se = FALSE, linewidth = 0.8) +
  
  # Colors & linetypes
  scale_colour_scico_d(palette = "berlin") +
  scale_linetype_manual(values = c("P" = "dashed", "NP" = "solid")) +
  #scale_linetype_manual(values = c("NM" = "dashed", "PFO" = "solid",
   #  "SM" = "dotted")) +
  facet_wrap(~StructureVar)+
  # Labels
  labs(
    x = "Time Since Fire (years)",
    y = "percent cover",
    colour = "Treatment",
    linetype = "Treatment"  ) +
  
  # Theme
  theme_minimal(base_size = 14) +
  theme(
    axis.title = element_text(size = 15),
    axis.text = element_text(size = 12),
    legend.position = "bottom",
    strip.text = element_text(size = 12)
  )

t.test(plot_structure[TSF == "20-40" & Planted == "P"]$ShrubsB2,
       plot_structure[TSF == "20-40" & Planted == "NP"]$ShrubsB2)
t.test(plot_structure[Planted == "P"]$ShrubsB2,
       plot_structure[Planted == "NP"]$ShrubsB2)



devel_metrics_tr <- Reduce(
  function(x, y) merge(x, y, by = "PlotID", all = TRUE),
  list(devel_metrics, plot_treatments,site_devel[,.(PlotID, management_class,
                                                    field_severity)])
)
scale_devel <- devel_metrics[
  ,
  lapply(.SD, function(x) fifelse(is.na(x), 0, x)),
  .SDcols = c("height_cv", "dom_height",
              "prop_low_LCR", "cbh_sep",
              "dbh_cv", "dom_dbh")
]
scale_devel_m <- scale(scale_devel, center = TRUE, scale = TRUE)
scale_devel <- as.data.table(scale_devel_m)
scale_devel[, PlotID := devel_metrics$PlotID]
plot_scale_devel <- Reduce(
  function(x, y) merge(x, y, by = "PlotID", all = TRUE),
  list(
    scale_devel,
    devel_metrics[,.(PlotID, n_trees_dbh, n_trees_hgt)],
    site_devel[,.(PlotID, management_class,structure_stage,
                  field_severity)],
    plot_treatments
  )
)
plot_scale_devel[,sum(n_trees_dbh, na.rm = TRUE), by = "Planted"]
plot_scale_devel[,sum(n_trees_hgt, na.rm = TRUE), by = "Planted"]
devel_long_ <- melt(plot_scale_devel, 
                      id.vars = c("PlotID", "TSF", "Planted", "TimeSinceFire",
                                  "management_class", "structure_stage", "field_severity"), 
                      measure.vars = c("height_cv", "dom_height",
                                       "prop_low_LCR", "cbh_sep",
                                       "dbh_cv", "dom_dbh"),
                      variable.name = "StructureVar",
                      value.name = "Value")

ggplot(devel_long_, 
       aes(x = TimeSinceFire, y = Value, colour = Planted)) +
  
  # Points
  geom_point(alpha = 0.7, size = 2) +
  
  # Optional: add trend line by Treatment
  geom_smooth(aes(group = Planted, linetype = Planted), 
              method = "lm", se = FALSE, linewidth = 0.8) +
  
  # Colors & linetypes
  scale_colour_scico_d(palette = "berlin") +
  scale_linetype_manual(values = c("P" = "dashed", "NP" = "solid")) +
  #scale_linetype_manual(values = c("NM" = "dashed", "PFO" = "solid",
  #  "SM" = "dotted")) +
  facet_wrap(~StructureVar)+
  # Labels
  labs(
    x = "Time Since Fire (years)",
    y = "live_ba",
    colour = "Treatment",
    linetype = "Treatment"
  ) +
  
  # Theme
  theme_minimal(base_size = 14) +
  theme(
    axis.title = element_text(size = 15),
    axis.text = element_text(size = 12),
    legend.position = "bottom",
    strip.text = element_text(size = 12)
  )




ggplot(hgt_counts, aes(x = TSF, y = SPH, fill = hgt_class)) +
  geom_col(alpha = 0.7) +
 scale_fill_scico_d(palette = "berlin") +
  labs(
    x = "Time Since Fire (years)",
    y = "SPH in each cohort",
    fill = "Structural stage",
    #itle = "Structural stage distribution by TSF and whether it had post-fire management"
  ) +
  
  # 4) Facet by Planted/Not Planted
  facet_wrap(~Planted, labeller = labeller(Planted = c("P" = "Planted",
                                                       "NP" = "Not Planted"))) +
  
  #facet_wrap(~management_class, labeller = labeller(Planted = c("NM" = "No Management",
  #                                                    "PFO" = "Post-fire Management"))) +
  
  # 5) Theme
  theme_minimal(base_size = 15) +
  theme(
    axis.title = element_text(size = 16),
    axis.text.x = element_text(size = 12, angle = 0, hjust = 0.5),
    axis.text.y = element_text(size = 13),
    legend.title = element_text(size = 14),
    legend.text  = element_text(size = 12),
    legend.position = "bottom"  # move legend to bottom
  )




# ladder fuels ------------------------------------------------
#this isn't quite right yet, but a first pass. Seems counter intuitive that
# there is a lot of ladder fuel in planted treatment

live_trees[, `:=`(
  crown_top = Height,
  crown_bot = Crown_base_height
)]
overlap_dt <- live_trees[
  ,
  {
    can_cbh <- mean(crown_bot[hgt_class == "dominant"], na.rm = TRUE)
    sub_top <- mean(crown_top[hgt_class == "sub-dom"], na.rm = TRUE)
    sub_cbh <- mean(crown_bot[hgt_class == "sub-dom"], na.rm = TRUE)
    supp_top <- mean(crown_top[hgt_class == "suppressed"], na.rm = TRUE)
    
    can_sub_overlap <- sub_top - can_cbh
    sub_sup_overlap <- supp_top - sub_cbh
    
    .(can_sub_overlap_depth = can_sub_overlap,
      sub_sup_overlap_depth = sub_sup_overlap)
  },
  by = PlotID
]
connect_dt <- live_trees[
  ,
  {
    can_cbh <- mean(crown_bot[hgt_class == "dominant"], na.rm = TRUE)
    sub_cbh <- mean(crown_bot[hgt_class == "sub-dom"], na.rm = TRUE)
    
    # ---- PHF-weighted proportions (what you already had) ----
    can_sub_prop_connect <- weighted.mean(
      crown_top[hgt_class == "sub-dom"] > can_cbh,
      w = PHF[hgt_class == "sub-dom"],
      na.rm = TRUE
    )
    
    sub_sup_prop_connect <- weighted.mean(
      crown_top[hgt_class == "suppressed"] > sub_cbh,
      w = PHF[hgt_class == "suppressed"],
      na.rm = TRUE
    )
    
    # ---- NEW: stems per hectare forming ladder connections ----
    can_sub_stems_ha <- sum(
      PHF[hgt_class == "sub-dom" & crown_top > can_cbh],
      na.rm = TRUE
    )
    
    sub_sup_stems_ha <- sum(
      PHF[hgt_class == "suppressed" & crown_top > sub_cbh],
      na.rm = TRUE
    )
    
    .(
      can_sub_prop_connect,
      sub_sup_prop_connect,
      can_sub_stems_ha,
      sub_sup_stems_ha
    )
  },
  by = PlotID
]
max_h <- live_trees[, max(crown_top, na.rm = TRUE)]
bins <- seq(0, max_h + 1, by = 1)

fill_dt <- live_trees[
  ,
  {
    occ <- rep(FALSE, length(bins) - 1)
    
    for(i in seq_len(.N)){
      hit <- bins[-length(bins)] < crown_top[i] &
        bins[-1] > crown_bot[i]
      occ <- occ | hit
    }
    
    .(vertical_fill = mean(occ))
  },
  by = PlotID
]
ladder_dt <- Reduce(
  function(x, y) merge(x, y, by = "PlotID", all = TRUE),
  list(overlap_dt, connect_dt, fill_dt)
)
ladder_dt[, `:=`(
  can_sub_overlap_scaled =
    pmax(pmin(can_sub_overlap_depth / 5, 1), 0),
  
  sub_sup_overlap_scaled =
    pmax(pmin(sub_sup_overlap_depth / 5, 1), 0)
)]
ladder_dt[, `:=`(
  can_sub_stems_scaled =
    pmin(can_sub_stems_ha / 500, 1),
  
  sub_sup_stems_scaled =
    pmin(sub_sup_stems_ha / 500, 1)
)]
ladder_dt[, overlap_scaled :=
            rowMeans(
              .SD,
              na.rm = TRUE
            ),
          .SDcols = c(
            "can_sub_overlap_scaled",
            "sub_sup_overlap_scaled"
          )
]
ladder_dt[, prop_connect :=
            rowMeans(
              .SD,
              na.rm = TRUE
            ),
          .SDcols = c(
            "can_sub_prop_connect",
            "sub_sup_prop_connect",
            "can_sub_stems_scaled",
            "sub_sup_stems_scaled"
          )
]
ladder_dt[, ladder_fuel_index :=
            rowMeans(
              .SD,
              na.rm = TRUE
            ),
          .SDcols = c(
            "overlap_scaled",
            "prop_connect",
            "vertical_fill"
          )
]
ladder_dt <- merge(
  ladder_dt,
  plot_structure[, .(PlotID, TSF, TimeSinceFire, Planted)],
  by = "PlotID",
  all.x = TRUE
)

ggplot(
  ladder_dt,
  aes(x = TimeSinceFire,
      y = ladder_fuel_index,
      colour = Planted)
) +
  geom_point(alpha = 0.6) +
  geom_smooth(aes(group = Planted, linetype = Planted), 
              method = "loess", se = FALSE, linewidth = 0.8) +
  
  # Colors & linetypes
  scale_colour_scico_d(palette = "berlin") +
  scale_linetype_manual(values = c("P" = "dashed", "NP" = "solid")) +
  labs(
    x = "Time since fire (years)",
    y = "Ladder fuel index",
    colour = "Treatment",
    title = "Development of ladder fuels through time"
  )+
  theme_minimal(base_size = 15) +
  theme(
    axis.title = element_text(size = 16),
    axis.text.x = element_text(size = 12, angle = 0, hjust = 0.5),
    axis.text.y = element_text(size = 13),
    legend.title = element_text(size = 14),
    legend.text  = element_text(size = 12),
    legend.position = "bottom"  # move legend to bottom
  )










