# Stand structure 

# This script summarizes the data into stand structure variables
# Ingrid Farnell
# June 8, 2023

# Load libraries
library(data.table)

# Import data
#Plot
FR_treatments <- fread("./Inputs/FR_Treatments.csv") # has field plot assessed treatments and fire year
#decided to change FR08 to NP because while it was a plantation, it was burned at low severity and not planted after the fire.

#Tree data:
A1trees <- fread("./Inputs/A1trees.csv")
B1trees <- fread("./Inputs/B1trees.csv")
Regen <- fread("./Inputs/Regen.csv")

#Woody debris:
cwd <- fread("./Inputs/FireRehabData_CWD.csv")
fwd <- fread("./Inputs/FireRehabData_FWD.csv")
line <- fread("./Inputs/FireRehabData_TransectDistance.csv")


# Variables
# Basal area per hectare (BAPH)
source("./R/BasalAreaFunctions.R")
BAtrees <- BAPH(A1trees, B1trees)
# 7.5+, 12.5+, 17.5+, 22.5+ (size classes from BC VRI )
BAdiamClass <- BAPHsizeCl(A1trees, B1trees)


# Live trees stems per hectare (density)
source("./R/DensityFunctions.R")
PlotTree <- TreeDensity(A1trees, B1trees) # ask Alana why 2cm size bins

# Snags per hectare
PlotSnags <- SnagDensity(A1trees, B1trees)

# Crown closure
