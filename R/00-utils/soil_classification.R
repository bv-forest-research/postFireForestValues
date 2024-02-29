# soil classification
# Jocelyn Biro, Ingrid Farnell, Alana Clason
# Aug 28, 2023

# This script classifies the plot's percent coarse frags, clay, and sand into soil classifications 
# to be used to determine soil heat transfer for fire risk analysis by Greg Greene

# load libraries
library(data.table)

# load data
soils <- fread("./Inputs/Soils.csv")

#calculate percent coarse fragments by weight
soils[, coarse.frag.kg:= (Coarse_frag_weight_W_tin - MinSoil_TinWgt)/1000]
soils[, min.soil.kg:= (MinSoilAll_DryWgt - MinSoil_TinWgt)/1000] # this includes coarse and fine frags
soils[, per.coarse.wgt:= (coarse.frag.kg/min.soil.kg)*100]

#need percent coarse fragments by volume
#cf(vol%)=(BD.total/BD.cf)*per.coarse.wgt
#where
#BD.total = bulk density total soil (kg/m3)
#BD.cf = bulk density coarse fragments -approximated to be 2650
#per.coarse.vol = volume of coarse fragments as percent of total soil
#per.coarse.wgt = weight of coarse fragments as percent of total soil

soils[, TotalSample_wgt_kg:= (MinSoilAll_DryWgt - MinSoil_TinWgt +
                                Root_wgt_inMin + Black_C_wgt_inMin + 
                                Litter_Wgt_inMin)/1000]

#calculate total soil Bulk Density (BDs)
# confirm with Joc & Alana that this BD calc is correct - it's not the one we used in the paper
soils[, BD.total:= TotalSample_wgt_kg/(BulkDensity_ml/1000000)] #kg/m3
soils[, per.coarse.vol:= (BD.total/2650)*per.coarse.wgt]

# add textures
# Calculate 'clay', 'silt', and 'sand' columns
soils[, `:=` (clay = ifelse(!is.na(MinSoil_Clay_PC_hyd), MinSoil_Clay_PC_hyd, MinSoil_Clay_PC_h202_hyd),
              silt = ifelse(!is.na(MinSoil_Silt_PC_hyd), MinSoil_Silt_PC_hyd, MinSoil_Silt_PC_h202_hyd),
              sand = ifelse(!is.na(MinSoil_Sand_PC_hyd), MinSoil_Sand_PC_hyd, MinSoil_Sand_PC_h202_hyd))]


## Classify ####
##note: split loamy-skeletal into fine-loamy-skeletal and coarse-loamy-skeletal as per E.Lilles recommendation

##classifications are based on the Rooting Zone Particle Size section from LMH25
####fine-loamy-skeletal; FLS (>=35% coarse fragments, >=18% clay)
####coarse-loamy-skeletal; CLS (>=35% coarse fragments, <18% clay)
####fine-silty; FS (<35 coarse fragments, 35%<clay>=18%)
####fine; F (<35 coarse fragments, >=35% clay)
####coarse-silty; CS (<35 coarse fragments, <18% clay, <15% sand)
####coarse-loamy; CL (<35 coarse fragments, <18% clay, >=15% sand))

soils[, soilClass := ifelse(
  per.coarse.vol >= 35 & clay >= 18, "FLS",
  ifelse(per.coarse.vol >= 35 & clay < 18, "CLS",
         ifelse(per.coarse.vol < 35 & clay < 35 & clay >= 18, "FS",
                ifelse(per.coarse.vol < 35 & clay >= 35, "F",
                       ifelse(per.coarse.vol < 35 & clay < 18 & sand < 15, "CS",
                              ifelse(per.coarse.vol < 35 & clay < 18 & sand >= 15, "CL", "NA"))))))]

# create new 'cleaned' output datatable
soil.class <- soils[, .(ID, soilClass)]

# export .csv
write.csv(soil.class, "./Outputs/SoilClassification.csv", row.names = FALSE)
