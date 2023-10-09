#This script calculates Forest carbon.

# citation A.Clason, I. Farnell, and E. Lilles, 2022. Carbon 5 - 60 years after fire: Planting
#         trees does not compensate for losses in dead wood stores


calc_tree_carbon <- function(A1trees = A1trees,
                             B1trees = B1trees) {

    ################################## Tree Carbon ##################################
    # A1 plot = 5.64m radius = 100m2, B1 = 11.28m radius= 400 m2, A2 = 3.99m radius = 50m2
    # convert to Mg/ha: Kg/m2 x 10:
    # (Kg C in the tree)/plot are(m2) x (1Mg/1000kg) x (10000m2/ha) == (Kg C in the tree)/plot are(m2) x 10
    # also here, we account for the sub-sampling of some plots
    A1trees[, Species := as.factor(Species)]
    B1trees[, Species := as.factor(Species)]
  
    #A1 trees biomass
    A1trees[, AreaSearchM2 := ifelse(`Sub-plot` == "A1", 100, ifelse(`Sub-plot` ==
                                                                       "A2", 50, 400))]
    A1trees[Notes == "only 1/4 of A1", AreaSearchM2 := 100 / 4]
    A1trees[Notes == "only 1/2 of A1", AreaSearchM2 := 100 / 2]
    A1trees[Notes == "only 1/4 of A2", AreaSearchM2 := 50 / 4]
    A1trees[, MgPerHa_fac := 10 / AreaSearchM2] #calculating the factor to multiply to get Mg/Ha
    
    A1trees[, CarbonPerHa := treeCalcs::calc_tree_c(Species = Species,
                                                    DBH = DBH,
                                                    HT = Height,
                                                    Tree_class = Tree_class) * MgPerHa_fac,
            by = seq_len(nrow(A1trees))]
    
    #A1trees[is.na(CarbonPerHa)] #No A1 trees, biomass==0
    #A1trees[is.na(CarbonPerHa), CarbonPerHa := 0]
    #A1trees[CarbonPerHa == 0]
    
    #B1 trees biomass
    B1trees[, AreaSearchM2 := ifelse(`Sub-plot` == "A1", 100, 
                                ifelse(`Sub-plot` == "A2", 50, 400))]
    B1trees[, MgPerHa_fac := 10 / AreaSearchM2] #calculating the factor to mulitply to get Mg/Ha
    B1trees[, CarbonPerHa := treeCalcs::calc_tree_c(Species = Species,
                                                    DBH = DBH,
                                                    HT = Height,
                                                    Tree_class = Tree_class) * MgPerHa_fac,
            by = seq_len(nrow(B1trees))]
    B1trees[is.na(CarbonPerHa), CarbonPerHa := 0]
    
    #merge A1 and B1 together
    A1B1trees <- rbind(A1trees[, .(PlotID,`Sub-plot`, Species, DBH, Height,
                                   Tree_class, AreaSearchM2,CarbonPerHa)],
                       B1trees[, .(PlotID,`Sub-plot`, Species, DBH, Height,
                                   Tree_class, AreaSearchM2, CarbonPerHa)])
    A1B1trees[, PHF := 10000 / AreaSearchM2] #accounting for smaller search areas

    live_plot_trees <- A1B1trees[Tree_class < 3, .(LiveMgHa = sum(CarbonPerHa)), by = c("PlotID")]
    dead_plot_trees <- A1B1trees[Tree_class >= 3, .(DeadMgHa = sum(CarbonPerHa)), by = c("PlotID")]
    plot_trees <- merge(live_plot_trees, dead_plot_trees, by = "PlotID", all = TRUE)
    colst <- c("LiveMgHa","DeadMgHa")
    plot_trees[, (colst) := lapply(.SD, function(x) ifelse(is.na(x), 0, x)), 
              .SDcols = colst]
    return(plot_trees)
}



calc_regen_c <- function(Regen = Regen){
  
  ################################## Regeneration carbon ##################################
  Regen[, Height_class := `Height_class(cm)`][, `Height_class(cm)` := NULL]
  Regen$Diam_est <- 1 # for 31-130 cm height class
  Regen$Diam_est[Regen$Height_class == "0-30"] <- 0.1
  h <- vector()
  for (i in 1:nrow(Regen)) {
    h[i] <- treeCalcs::calc_sm_tree_c_Ung(Species = Regen[i, Species],
                                         Diam_est = 0.1,
                                         Height_class = Regen[i, Height_class],
                                         Health = Regen[i, `Live/Dead`])
  }
  Regen[, AreaSearchM2 := ifelse(`Sub-Plot` == "A1", 100, 50)]
  Regen[, MgPerHa_fac := 10 / AreaSearchM2]
  Regen[, CarbonPerHa := h * Tally * MgPerHa_fac]
  Regen[CarbonPerHa == 0]
  Regen[is.na(CarbonPerHa)]
  PlotRegen <- Regen[, .(Regen_MGHa = sum(CarbonPerHa)), by = c("PlotID", "Live/Dead")]
  PlotRegen <- dcast(PlotRegen, PlotID ~ `Live/Dead`, value.var = "Regen_MGHa")
  setnames(PlotRegen, c("L", "D"), c("Regen_L_MGHa", "Regen_D_MGHa"))
  
  return(PlotRegen)
}

calc_regen_biomass <- function(Regen = Regen){
  
  ################################## Regeneration carbon ##################################
  Regen[, Height_class := `Height_class(cm)`][, `Height_class(cm)` := NULL]
  Regen$Diam_est <- 1 # for 31-130 cm height class
  Regen$Diam_est[Regen$Height_class == "0-30"] <- 0.1
  h <- vector()
  for (i in 1:nrow(Regen)) {
    h[i] <- treeCalcs::calc_sm_tree_c_Ung(Species = Regen[i, Species],
                                          Diam_est = 0.1,
                                          Height_class = Regen[i, Height_class],
                                          Health = Regen[i, `Live/Dead`])
  }
  b <- h * 2 #should be in kg
  Regen[, AreaSearchM2 := ifelse(`Sub-Plot` == "A1", 100, 50)]
  Regen[, AreaHa := AreaSearchM2/10000, by = seq_len(nrow(Regen))]
  #Regen[, MgPerHa_fac := 10 / AreaSearchM2]
  Regen[, BiomassHa := (b * Tally)/AreaHa]
  PlotRegen <- Regen[, .(Regen_Kg = sum(BiomassHa)), by = c("PlotID", "Live/Dead")]
  Regen[CarbonPerHa == 0]
  Regen[is.na(CarbonPerHa)]
  PlotRegen <- Regen[, .(Regen_MGHa = sum(CarbonPerHa)), by = c("PlotID", "Live/Dead")]
  PlotRegen <- dcast(PlotRegen, PlotID ~ `Live/Dead`, value.var = "Regen_MGHa")
  setnames(PlotRegen, c("L", "D"), c("Regen_L_MGHa", "Regen_D_MGHa"))
  
  return(PlotRegen)
}




calc_soil_carbon <- function(soil = soil,
                             plots = plot_treatments[,.(PlotID)]) {
    ################################## Mineral soil carbon ##################################
    Soils[, C_pro := MinSoil_C_PC / 100]
    
    #calculating bulk density
    Soils[, TotalSample_wgt_kg := (
      MinSoilAll_DryWgt - MinSoil_TinWgt + #this includes the coarse fragments
        Root_wgt_inMin + Black_C_wgt_inMin +
        Litter_Wgt_inMin
    ) / 1000]
    Soils[, CoarseFrag_wgt_kg := (Coarse_frag_weight_W_tin - MinSoil_TinWgt) /
            1000]
    #Bulk density calculations:
    Soils[, OrgFrag_wgt_kg := (Root_wgt_inMin + Black_C_wgt_inMin + Litter_Wgt_inMin) /
            1000]
    Soils[, FineFract_wgt_kg := (MinSoilAll_DryWgt - Coarse_frag_weight_W_tin) /
            1000 - OrgFrag_wgt_kg]
    Soils[, CoarseAndOrgFragM3M3 := (CoarseFrag_wgt_kg / 2650 + OrgFrag_wgt_kg /
                                       500) / (BulkDensity_ml / 1000000)]
    ## Fine fraction BulkDensity ####
    #fine fraction bulk density defined in Bulmer and Simpson 2010 (Soil Compaction Reduced the Growth of Lodgepole Pine and Douglas-fi r Seedlings in Raised Beds after Two Growing Seasons) Fine fraction
    Soils[, BDcalc_finefract := FineFract_wgt_kg / ((BulkDensity_ml / 1000000) -
                                                      (CoarseFrag_wgt_kg / 2650) -
                                                      (OrgFrag_wgt_kg / 500))]#kg/m3
    FR_minSOC_finefract_kg_m2 <-
      treeCalcs::calc_min_soil_c(
        Soc = Soils[, C_pro],
        BD = Soils[, BDcalc_finefract],
        depth = Soils[, BDDepth],
        CoarseFrags = Soils[, CoarseAndOrgFragM3M3]
      )
    ## Soil Organic Carbon ####
    #using 0.5 biomass to C conversion for litter and roots
    #using a 0.75 char to C mass conversion from Donato et al. 2009 (Quantifying char in postfire woody detritus inventories)
    Soils[, FR_BlackC_kg := (Black_C_wgt_inMin * 0.75) / 1000] #removed roots, and added litter to litter
    Soils[, FR_BlackC_kg_m2 := FR_BlackC_kg / (BulkDensity_ml / 1000000) * BDDepth]
    Soils[, FR_SOM := C_pro / 0.47]
    Soils[, BDcalc_finefract_g_cm3 := BDcalc_finefract * 1000 / 1e+06] #calculate finefraction
    
    #Test the predicted (Perie and Ouimet) BD vs observed BD
    Soils[, BDest_finefract_g_cm3 := -1.977 + 4.105 * FR_SOM - 1.229 * log(FR_SOM) - 0.103 *
            log(FR_SOM) ^ 2] #estimate fine fraction
    
    ## Estimating missing data in plot 63
    #use estimated fine fraction from Perie and Ouimet
    Soils[ID == "FR63", BDcalc_finefract := BDest_finefract_g_cm3 * 1000]
    #back calculate likely volume of hole for 63 to estimate other C volume
    Soils[ID == "FR63", BulkDensity_ml := (FineFract_wgt_kg / (
      BDcalc_finefract + (CoarseFrag_wgt_kg / 2650) -
        (OrgFrag_wgt_kg /
           500)
    )) * 1000000]
    Soils[ID == "FR63", FR_BlackC_kg_m2 := FR_BlackC_kg / (BulkDensity_ml /
                                                             1000000) * 0.2] #calculate black C Kg/m2
    Soils[ID == "FR63", CoarseAndOrgFragM3M3 := (CoarseFrag_wgt_kg / 2650 + OrgFrag_wgt_kg /
                                                   500) / (BulkDensity_ml / 1000000)]
    FR_minSOC_finefract_kg_m2[63] <-
      treeCalcs::calc_min_soil_c(
        Soc = Soils[ID == "FR63", C_pro],
        BD = Soils[ID == "FR63", BDcalc_finefract],
        depth = 0.2,
        CoarseFrags = Soils[ID == "FR63", CoarseAndOrgFragM3M3]
      )
    FR_minSOC_kg_m2 <-
      FR_minSOC_finefract_kg_m2 + Soils[, FR_BlackC_kg_m2] #fine fraction SOC + BlackCarbon C
    FR_minSOC_Mg_ha <-
      (FR_minSOC_kg_m2 / 1000) * 10000 #convert to Mg per hectare
    
    
    plots[, MinSoilC_Mgha := FR_minSOC_Mg_ha]
    

    ################################## Litter carbon ##################################
    
    plots[, Litter_C_g := Soils[, (Litter_DryWgt * (Litter_C_PC /
                                                              100)) + Litter_Wgt_inMin * 0.5]]
    plots[, Litter_C_MgHa := Litter_C_g / 16]
    
    ################################## Forest floor carbon ##################################
    Soils[, ForestFloor_C_g := sum((ForFloor_DryWgt * ForFloor_C_PC / 100),
                                   Wood_chunks_inFF * 0.5,
                                   Black_C_wgt_inFF * 0.75,
                                   na.rm = TRUE
    ), by = "ID"]
    #tiny amt of FF in FR03 didn't make it to the lab, so just use 0.5
    Soils[ID == "FR03", ForestFloor_C_g := ForFloor_DryWgt * 0.5 + Wood_chunks_inFF *
            0.5 + Black_C_wgt_inFF * 0.75]
    #the only true NA (missing sample) is FR02
    Soils[ID == "FR02", ForestFloor_C_g := NA]
    plots[, ForestFloor_C_g := Soils[, ForestFloor_C_g]]
    plots[, ForestFl_MgHa := ForestFloor_C_g / 16] #converting to Mg/Ha
    
    return(plots)
    
}


calc_wd_carbon <- function(cwdDat, fwdDat, lineDat){
  # cwdCARBON (T/ha) = volume(m3/ha)
  #                     x structural reduction factor # decay class specific (Fraver et al. 2013)
  #                     x Absolute density(g/cm3) # species and decay class specific (Harmon et al. 2008)
  #                     x CarbonConcentration # species and decay class specific (Harmon et al. 2013)
  
  cwd.line.plot <- cwdVol(cwdDat, lineDat)
  
  
  
  t <- vector()
  for(i in 1:nrow(cwd.line.plot)){
    t[i] <- treeCalcs::calc_cwd_c(volume_ha = cwd.line.plot[i,VolHa], 
                                  Decay_class = cwd.line.plot[i,Decay_class], 
                                  Species = cwd.line.plot[i,Species],
                                  BECzone = "SBS")
  }
  
  cwd.line.plot[,carbon := as.numeric(t)]
  cwd.line.plot$carbon[is.na(cwd.line.plot$carbon)] <- 0
  cwd.line.plot$VolHa[is.na(cwd.line.plot$VolHa)] <- 0
  
  # Sum total carbon in each plot
  cwd.carbon.plot <- cwd.line.plot[,.(cwd_VolHa = sum(VolHa),
                                      cwd_MgHa = sum(carbon)),
                                   by = "PlotID"]
  
  ################################## Fine woody debris carbon ##################################
  # fwdCARBON (T/ha) = volume(m3/ha)
  #                     x Live wood density(g/cm3) # use unknown species (Harmon et al. 2008)
  #                     x Decay reduction factor for each size class (Harmon and Fasth website)
  #                     x CarbonConcentration # use 50% (Harmon and Fasth website)
  
  fwd.line.plot <- fwdVol(fwdDat, lineDat)
  
  # Assign carbon to new column
  c <- vector()
  for(i in 1:nrow(fwd.line.plot)){
    c[i] <- treeCalcs::calc_fwd_c(volume = fwd.line.plot[i,"volume_ha"], 
                        Diam_class = fwd.line.plot[i, "Diam_class"])
  }
  fwd.line.plot[, carbon := as.numeric(c)]
  
  # Sum total carbon in each plot
  fwd.carbon.plot <- fwd.line.plot[,.(fwd_VolHa = sum(volume_ha),
                                      fwd_MgHa = sum(carbon)),
                                   by = "PlotID"]
  
   cfwd_carbon <- merge(cwd.carbon.plot, fwd.carbon.plot, by = "PlotID")
 
   return(cfwd_carbon)
    
  }
