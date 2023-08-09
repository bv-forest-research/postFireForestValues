# CWD Habitat Quality Index
# from Farnell et al. 2020 and van Galen et al. 2019

#####################################
# Habitat Quality Index Criteria
####################################

# Criteria 1: median diameter - cwd size cohort

# Criteria 2: Maximum diameter- largest piece, important for habitat

# Criteria 3: Having an even distribution of decay classes is benificial for mammals and saproxylic insects. 
# Mustileds prefer more sound CWD for providing structure, runways and subnivean access. Small mammals (mice etc.)
# prefer more decayed pieces for tunnelling in the wood. Saproxlyic insects prefer a diverse range of
# decay for the multiple different species requirements. 

# Criteria 3. The number of CWD pieces. 
#############################################################################################
# Habitat Quality Index function
cwdHQI <- function(cwdDat){
  cwd[Notes=="no CWD" | Notes=="no CWD on line", Diam_cm:=0][Notes=="no CWD" | Notes=="no CWD on line", Decay_class:=0]
  # Median and max diameter
  cwdPlot<- cwd[, .(diamMed= mean(Diam_cm), 
                  diamMax=max(Diam_cm)),
              by="PlotID"]
  # Number of cwd pieces/plot
  Ndc <- cwd[, .N, by=.(PlotID, Decay_class)]
  Ndc[, PlotID:=as.factor(PlotID)][, Decay_class:=as.factor(Decay_class)]
  Ndc[, PlotN:=sum(N), by="PlotID"]
  # Decay class evenness
  Ndc[, DCevenness:= abs(N-(PlotN/5)), by=c("PlotID", "Decay_class")]
  PlotNdc <- Ndc[,.(DCevenness=DCevenness/PlotN), by=c("PlotID", "PlotN")][,.(DCevenness=-(sum(DCevenness))), by=c("PlotID", "PlotN")]
  # square root of max diameter and number pieces
  cwdPlot <- merge(cwdPlot, PlotNdc)
  cwdPlot <- cwdPlot[,diamMax:=sqrt(diamMax)][,PlotN:=sqrt(PlotN)][diamMax==0, PlotN:=0][diamMax==0, DCevenness:=0]
  # scaling variables (mean=0, sd=1)
  cols <- colnames(cwdPlot)[-1]
  cwdPlot[, (cols):=lapply(.SD, scale), .SDcols=cols] # right now I have plots with no CWD included before the standardization - should they be removed for this?
  # summing variables and normalizing (0-100) for habitat quality index
  cwdPlot[, HQI:=sum(diamMed, diamMax, PlotN, DCevenness), by="PlotID"]
  cwdPlot[, HQI:=((HQI-min(HQI))/(max(HQI)-min(HQI)))*100]
  PlotHQI <- cwdPlot[,.(PlotID, HQI)]
  return(PlotHQI)
}

# CWD plot variable function
cwdVar <- function(cwdDat){
  cwd[Notes=="no CWD" | Notes=="no CWD on line", Diam_cm:=0][Notes=="no CWD" | Notes=="no CWD on line", Decay_class:=0]
  # Median and max diameter
  cwdPlot<- cwd[, .(diamMed= mean(Diam_cm), 
                    diamMax=max(Diam_cm)),
                by="PlotID"]
  # Number of cwd pieces/plot
  Ndc <- cwd[, .N, by=.(PlotID, Decay_class)]
  Ndc[, PlotID:=as.factor(PlotID)][, Decay_class:=as.factor(Decay_class)]
  Ndc[, PlotN:=sum(N), by="PlotID"]
  # Decay class evenness
  Ndc[, DCevenness:= abs(N-(PlotN/5)), by=c("PlotID", "Decay_class")]
  PlotNdc <- Ndc[,.(DCevenness=DCevenness/PlotN), by=c("PlotID", "PlotN")][,.(DCevenness=-(sum(DCevenness))), by=c("PlotID", "PlotN")]
  # square root of max diameter and number pieces
  cwdPlot <- merge(cwdPlot, PlotNdc)
  return(cwdPlot)
}
