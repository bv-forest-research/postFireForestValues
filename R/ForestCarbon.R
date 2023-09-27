## Load libraries
library(sf)
library(data.table)
source("./R/CarbonFunctions.R")

## set pathways 
DatPath <- "./Inputs/"

## Import data
#Plot
FR_treatments <- fread(paste0(DatPath,"FR_Treatments.csv")) # has field plot assessed treatments and fire year
#decided to change FR08 to NP because while it was a plantation, it was burned at low severity and not planted after the fire.
#Tree data:
A1trees <- fread(paste0(DatPath,"A1trees.csv"))
B1trees <- fread(paste0(DatPath,"B1trees.csv"))
Regen <- fread(paste0(DatPath,"Regen.csv"))

#Soils data:
Soils <- fread(paste0(DatPath,"Soils.csv"))

#Woody debris:
cwd <- read.csv(paste0(DatPath,"FireRehabData_CWD.csv"),header = T,stringsAsFactors = T)
fwd <- read.csv(paste0(DatPath,"FireRehabData_FWD.csv"),header = T,stringsAsFactors = T)
line <- read.csv(paste0(DatPath,"FireRehabData_TransectDistance.csv"),header = T,stringsAsFactors = T) 

#Plot treatment cleaning
FR_treatments[,ID:=as.factor(ID)][,Planted:=as.factor(Planted)][,CumBurnSev:= as.factor(CumBurnSev)]
FR_treatments$CumBurnSev <- factor(FR_treatments$CumBurnSev, levels=c("L","LM","M","MH","H"))
FR_treatments[,FIRE_YR_cat:=ifelse(FIRE_YEAR<=1979,"1960-1979",
                                   ifelse(FIRE_YEAR>1979 & FIRE_YEAR<=1989,"1980-1989",
                                          ifelse(FIRE_YEAR>1989 & FIRE_YEAR<=1999,"1990-1999",
                                                 ifelse(FIRE_YEAR>1999 & FIRE_YEAR<=2010,"2000-2009",
                                                        "2010+"))))]
FR_treatments[,TimeSinceFire := 2020 - FIRE_YEAR]

#Climate
AnnClim <- fread(paste0(DatPath,"FR_plots_Clim_Normal_1961_1990Y.csv"))
FR_treatments <- merge(FR_treatments, AnnClim[,.(ID1,MAT, MWMT, MCMT, TD, MAP, MSP, AHM, SHM, DD_0, DD5, DD_18,
                                                 DD18, NFFD, bFFP, eFFP, FFP, PAS, EMT, EXT, MAR, Eref, CMD, RH,
                                                 CMI, DD1040)],by.x="ID",by.y="ID1")

hli_rast <- raster("./Inputs/Rasters/DEMhli.tif")
slope_rast <- raster("./Inputs/Rasters/DEMslope.tif")
Study_plots <- read_sf("./Inputs/Shapefiles/FRplots.shp")
Can_dNBR <- raster("./Inputs/Rasters/CanLaBS_dNBR1000_v0.tif")



###################################################################################################### 
############################################ Data Prep ###############################################
######################################################################################################

################################## Tree Carbon ################################## 
# A1 plot = 5.64m radius = 100m2, B1 = 11.28m radius= 400 m2, A2 = 3.99m radius = 50m2
# convert to Mg/ha: Kg/m2 x 10: 
# (Kg C in the tree)/plot are(m2) x (1Mg/1000kg) x (10000m2/ha) == (Kg C in the tree)/plot are(m2) x 10
# also here, we account for the sub-sampling of some plots
A1trees[,Species := as.factor(Species)]
B1trees[,Species := as.factor(Species)]
#A1 trees biomass
t <- vector()
for(i in 1:nrow(A1trees)){
  t[i] <- TreeCarbonFN(Species = A1trees[i,Species], DBH= A1trees[i,DBH], 
                       HT= A1trees[i,Height], Tree_class=A1trees[i,Tree_class])
}
A1trees[,AreaSearchM2 := ifelse(`Sub-plot`=="A1",100, ifelse(`Sub-plot`=="A2",50,400))]
A1trees[Notes=="only 1/4 of A1",AreaSearchM2:=100/4]
A1trees[Notes=="only 1/2 of A1",AreaSearchM2:=100/2]
A1trees[Notes=="only 1/4 of A2",AreaSearchM2:=50/4]
A1trees[,MgPerHa_fac:=10/AreaSearchM2] #calculating the factor to mulitply to get Mg/Ha
A1trees[,CarbonPerHa:=t*MgPerHa_fac]
A1trees[is.na(CarbonPerHa)]#No A1 trees, biomass==0
A1trees[is.na(CarbonPerHa), CarbonPerHa:=0]
A1trees[CarbonPerHa ==0]
#B1 trees biomass
r <- vector()
for(i in 1:nrow(B1trees)){
  r[i] <- TreeCarbonFN(Species = B1trees[i,Species], DBH= B1trees[i,DBH], 
                       HT= B1trees[i,Height], Tree_class=B1trees[i,Tree_class])
}
B1trees[,AreaSearchM2 := ifelse(`Sub-plot`=="A1",100, ifelse(`Sub-plot`=="A2",50,400))]
B1trees[,MgPerHa_fac:=10/AreaSearchM2] #calculating the factor to mulitply to get Mg/Ha
B1trees[,CarbonPerHa:=r*MgPerHa_fac]
B1trees[is.na(CarbonPerHa), CarbonPerHa:=0]

#merge A1 and B1 together
A1B1trees <- rbind(A1trees[,.(PlotID,`Sub-plot`,Species,DBH,Height,Tree_class,AreaSearchM2,CarbonPerHa)], 
                   B1trees[,.(PlotID,`Sub-plot`,Species,DBH,Height,Tree_class,AreaSearchM2,CarbonPerHa)])
A1B1trees[,PHF:=10000/AreaSearchM2] #accounting for smaller search areas
#Live trees by plot
LiveTree_plots <- merge(FR_treatments[,.(ID)],A1B1trees[Tree_class<3, sum(CarbonPerHa),by="PlotID"],
                        by.x="ID", by.y="PlotID", all=TRUE)
LiveTree_plots$V1[is.na(LiveTree_plots$V1)] <-0
setnames(LiveTree_plots,"V1","LiveTreeCperHa")

#Dead trees by plot
DeadTree_plots <- merge(FR_treatments[,.(ID)],A1B1trees[Tree_class>=3, sum(CarbonPerHa),by="PlotID"],
                        by.x="ID", by.y="PlotID", all=TRUE)
DeadTree_plots$V1[is.na(DeadTree_plots$V1)] <-0
setnames(DeadTree_plots,"V1","DeadTreeCperHa")
live_dead_CperHa <- merge(LiveTree_plots,DeadTree_plots,by="ID")

FR_treatments <- merge(FR_treatments,live_dead_CperHa,by="ID")

################################## Regeneration carbon ##################################
Regen[,Height_class:=`Height_class(cm)`][,`Height_class(cm)`:=NULL]
Regen$Diam_est <- 1 # for 31-130 cm height class
Regen$Diam_est[Regen$Height_class == "0-30"] <- 0.1
h <- vector()
for(i in 1:nrow(Regen)){
  h[i] <- RegenCarbonFN_Ung(Species = Regen[i,Species], Diam_est = 0.1, 
                            Height_class = Regen[i,Height_class], Health = Regen[i,`Live/Dead`] )
}
Regen[,AreaSearchM2:=ifelse(`Sub-Plot`=="A1",100,50)]
Regen[,MgPerHa_fac:=10/AreaSearchM2]
Regen[,CarbonPerHa := h*Tally*MgPerHa_fac] 
Regen[CarbonPerHa==0]
Regen[is.na(CarbonPerHa)]
PlotRegen <- Regen[,.(Regen_MGHa = sum(CarbonPerHa)),by=c("PlotID","Live/Dead")]
PlotRegen <- dcast(PlotRegen, PlotID ~ `Live/Dead`, value.var="Regen_MGHa")
setnames(PlotRegen,c("L","D"),c("Regen_L_MGHa","Regen_D_MGHa"))

FR_treatments <- merge(FR_treatments,PlotRegen,by.x="ID",by.y="PlotID")


################################## Root carbon ##################################
FR_treatments[,LiveRootC:=((LiveTreeCperHa*0.222)+(Regen_L_MGHa*0.222)),by="ID"] #root =  0.222 x live biomass
FR_treatments[,DeadRootC:=((DeadTreeCperHa*exp(-0.1044*TimeSinceFire))+ #decaying the tree roots by time since fire
                             (Regen_D_MGHa*0.222)),by="ID"] #root =  0.222 x  dead biomass


################################## Mineral soil carbon ##################################
Soils[,C_pro := MinSoil_C_PC/100]

#calculating bulk density
Soils[,TotalSample_wgt_kg := (MinSoilAll_DryWgt-MinSoil_TinWgt+ #this includes the coarse fragments
                                Root_wgt_inMin+Black_C_wgt_inMin+
                                Litter_Wgt_inMin)/1000]
Soils[,CoarseFrag_wgt_kg := (Coarse_frag_weight_W_tin - MinSoil_TinWgt)/1000]
#Bulk density calculations:
Soils[,OrgFrag_wgt_kg := (Root_wgt_inMin + Black_C_wgt_inMin + Litter_Wgt_inMin)/1000]
Soils[,FineFract_wgt_kg := (MinSoilAll_DryWgt - Coarse_frag_weight_W_tin)/1000 - OrgFrag_wgt_kg]
Soils[,CoarseAndOrgFragM3M3 := (CoarseFrag_wgt_kg/2650 + OrgFrag_wgt_kg/500)/ (BulkDensity_ml/1000000) ]
## Fine fraction BulkDensity ####
#fine fraction bulk density defined in Bulmer and Simpson 2010 (Soil Compaction Reduced the Growth of Lodgepole Pine and Douglas-fi r Seedlings in Raised Beds after Two Growing Seasons) Fine fraction
Soils[,BDcalc_finefract := FineFract_wgt_kg/((BulkDensity_ml/1000000)-
                                               (CoarseFrag_wgt_kg/2650)-(OrgFrag_wgt_kg/500))]#kg/m3
FR_minSOC_finefract_kg_m2 <- Min_SOC(Soc = Soils[,C_pro], BD = Soils[,BDcalc_finefract],
                                     depth = Soils[,BDDepth], CoarseFrags = Soils[,CoarseAndOrgFragM3M3])
## Soil Organic Carbon ####
#using 0.5 biomass to C conversion for litter and roots
#using a 0.75 char to C mass conversion from Donato et al. 2009 (Quantifying char in postfire woody detritus inventories)
Soils[,FR_BlackC_kg := (Black_C_wgt_inMin*0.75)/1000] #removed roots, and added litter to litter
Soils[,FR_BlackC_kg_m2:= FR_BlackC_kg/(BulkDensity_ml/1000000) * BDDepth]
Soils[,FR_SOM:= C_pro/0.47]
Soils[,BDcalc_finefract_g_cm3 := BDcalc_finefract*1000/1e+06] #calculate finefraction

#Test the predicted (Perie and Ouimet) BD vs observed BD
Soils[,BDest_finefract_g_cm3 := -1.977+4.105*FR_SOM - 1.229*log(FR_SOM) - 0.103*log(FR_SOM)^2] #estimate fine fraction

## Estimating missing data in plot 63 
#use estimated fine fraction from Perie and Ouimet
Soils[ID=="FR63",BDcalc_finefract := BDest_finefract_g_cm3*1000]
#back calculate likely volume of hole for 63 to estimate other C volume
Soils[ID=="FR63", BulkDensity_ml := (FineFract_wgt_kg/(BDcalc_finefract + (CoarseFrag_wgt_kg/2650) -
                                                         (OrgFrag_wgt_kg/500)))*1000000]
Soils[ID=="FR63",FR_BlackC_kg_m2 := FR_BlackC_kg/(BulkDensity_ml/1000000) *0.2] #calculate black C Kg/m2
Soils[ID=="FR63",CoarseAndOrgFragM3M3 := (CoarseFrag_wgt_kg/2650 + OrgFrag_wgt_kg/500)/ (BulkDensity_ml/1000000) ]
FR_minSOC_finefract_kg_m2[63] <- Min_SOC(Soc = Soils[ID=="FR63",C_pro], 
                                         BD = Soils[ID=="FR63",BDcalc_finefract],
                                         depth = 0.2, 
                                         CoarseFrags = Soils[ID=="FR63",CoarseAndOrgFragM3M3])
FR_minSOC_kg_m2 <-FR_minSOC_finefract_kg_m2 + Soils[,FR_BlackC_kg_m2] #fine fraction SOC + BlackCarbon C
FR_minSOC_Mg_ha <- (FR_minSOC_kg_m2/1000)*10000 #convert to Mg per hectare
FR_treatments[,MinSoilC_Mgha := FR_minSOC_Mg_ha]

# Add textures
Soils[,CLAY := sum(MinSoil_Clay_PC_hyd,MinSoil_Clay_PC_h202_hyd,na.rm = TRUE), by="ID"]
Soils[,SILT := sum(MinSoil_Silt_PC_hyd, MinSoil_Silt_PC_h202_hyd,na.rm = TRUE), by="ID"]
Soils[,SAND := sum(MinSoil_Sand_PC_hyd,MinSoil_Sand_PC_h202_hyd,na.rm = TRUE), by="ID"]
Soils[,TotalTexture := CLAY + SILT + SAND]

### Add C:N ratio
Soils[,CN := MinSoil_C_PC/MinSoil_N_PC]

#combine this into the plot treatments
FR_treatments <- merge(FR_treatments, Soils[,.(ID,CN,CLAY)],by="ID")
FR_treatments[,CoarseFrag_wgt_kg := Soils[,CoarseFrag_wgt_kg]]
FR_treatments[,CN:=scale(CN, center=TRUE,scale=TRUE)]
FR_treatments[,CoarseFrag_wgt_kg:=scale(CoarseFrag_wgt_kg, center=TRUE,scale=TRUE)]
FR_treatments[,CLAY:=scale(CLAY, center=TRUE,scale=TRUE)]

################################## Litter carbon ##################################

FR_treatments[,Litter_C_g := Soils[,(Litter_DryWgt*(Litter_C_PC/100)) + Litter_Wgt_inMin*0.5]]
FR_treatments[,Litter_C_MgHa := Litter_C_g/16]

################################## Forest floor carbon ##################################
Soils[,ForestFloor_C_g := sum((ForFloor_DryWgt*ForFloor_C_PC/100), Wood_chunks_inFF*0.5,
                              Black_C_wgt_inFF*0.75,na.rm=TRUE), by="ID"]
#tiny amt of FF in FR03 didn't make it to the lab, so just use 0.5
Soils[ID=="FR03",ForestFloor_C_g := ForFloor_DryWgt*0.5+Wood_chunks_inFF*0.5+Black_C_wgt_inFF*0.75 ]
#the only true NA (missing sample) is FR02
Soils[ID=="FR02",ForestFloor_C_g := NA]
FR_treatments[,ForestFloor_C_g := Soils[,ForestFloor_C_g]]
FR_treatments[,ForestFl_MgHa :=ForestFloor_C_g/16] #converting to Mg/Ha

################################## Coarse woody debris carbon ##################################
source("./R/CWD_carbon.R")
cwd.carbon.plot <- as.data.table(cwd.carbon.plot)
plot.desc<- FR_treatments
# First column imported weird - rename it
names(plot.desc)[names(plot.desc) == "ID"] <- "PlotID"
# Merge dataframes via PlotID
cwd.desc <- merge(cwd.carbon.plot, plot.desc, by = "PlotID")
cwd.desc$FIRE_YR_cat <- as.factor(cwd.desc$FIRE_YR_cat)
FR_treatments[,CWD_C:=cwd.carbon.plot$carbon]

################################## Fine woody debris carbon ##################################
source("./R/FWD_carbon.R")
fwd.carbon.plot <- as.data.table(fwd.carbon.plot)
plot.desc<- FR_treatments
# First column imported weird - rename it
names(plot.desc)[names(plot.desc) == "ID"] <- "PlotID"
# Merge dataframes via PlotID
fwd.desc <- merge(fwd.carbon.plot, plot.desc, by = "PlotID") #not working now
fwd.desc <- as.data.frame(fwd.desc)
fwd.desc$FIRE_YR_cat <- as.factor(fwd.desc$FIRE_YR_cat)
FR_treatments[,FWD_C:=fwd.carbon.plot$carbon]

################################## Carbon pool summaries ##################################
CarbonPlots <- melt(FR_treatments, id.vars = c("ID", "Date", "HarvHist","Planted","CumBurnSev","CumBurnSevCat",
                                               "Aspect","Slope_PC", "SlopePos","FIRE_NUMBE", "FIRE_NAME", 
                                               "FIRE_YEAR", "area","FIRE_YR_cat"),
                    measure.vars = c("LiveTreeCperHa","Regen_L_MGHa","Regen_D_MGHa","DeadTreeCperHa",
                                     "MinSoilC_Mgha","Litter_C_MgHa",
                                     "ForestFl_MgHa","CWD_C","FWD_C","LiveRootC"),
                    variable.name = "CarbonSource",
                    value.name = "CarbonMgHa")

FR_treatments <- merge(FR_treatments,CarbonPlots[,.(TotalCarbon = sum(na.omit(CarbonMgHa))), by="ID"],by="ID")
FR_treatments <- merge(FR_treatments,CarbonPlots[CarbonSource!="MinSoilC_Mgha",
                                                 .(TotalCarbon_NotMin = sum(na.omit(CarbonMgHa))), by="ID"],by="ID")

FR_treatments <- merge(FR_treatments,CarbonPlots[CarbonSource=="DeadTreeCperHa"|
                                                   CarbonSource=="Regen_D_MGHa"|
                                                   CarbonSource=="CWD_C"|
                                                   CarbonSource=="FWD_C",
                                                 .(DeadCarbon = sum(na.omit(CarbonMgHa))),
                                                 by="ID"],by="ID")
FR_treatments <- merge(FR_treatments,CarbonPlots[CarbonSource=="LiveTreeCperHa"|
                                                   CarbonSource=="Regen_L_MGHa"|
                                                   CarbonSource=="LiveRootC",
                                                 .(LiveCarbon = sum(na.omit(CarbonMgHa))),
                                                 by="ID"],by="ID")
FR_treatments <- merge(FR_treatments,CarbonPlots[CarbonSource=="ForestFl_MgHa"|
                                                   CarbonSource=="Litter_C_MgHa",
                                                 .(ForestFloorTotC = sum(na.omit(CarbonMgHa))),
                                                 by="ID"],by="ID")

FR_tables <- melt(FR_treatments,id.vars = c("ID","Planted","TimeSinceFire"),
                  measure.vars = c("LiveTreeCperHa","Regen_L_MGHa","LiveRootC","DeadTreeCperHa","Regen_D_MGHa",
                                   "CWD_C","FWD_C"),
                  variable.name = "CarbonPool",
                  value.name = "CarbonMgHa")
