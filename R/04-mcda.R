# integrate across values
library(data.table)

in_dir <- "02-prepped_values"

#Schwenk et al 2012
#1. overall goal: manage forests for multiple values
#2. set objectives: objectives: 1. store carbon, 2. produce timber/wood products, 3. sustain biodiversity
#3. assign weights
#4. choose management prescriptions
#5. calculate partial utilities
#6. calculate total utilities



#1a. partial utlities carbon and timber

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

hab_ind <- fread(file.path(in_dir,"HabitatIndicies.csv"))

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