# CWD Quality Index
# from Farnell et al. 2020 and van Galen et al. 2019

#####################################
# CWD Quality Index Criteria
####################################

# Criteria 1: median diameter - cwd size cohort

# Criteria 2: Maximum diameter- largest piece, important for habitat

# Criteria 3: Having an even distribution of decay classes is benificial for mammals and saproxylic insects. 
# Mustileds prefer more sound CWD for providing structure, runways and subnivean access. Small mammals (mice etc.)
# prefer more decayed pieces for tunnelling in the wood. Saproxlyic insects prefer a diverse range of
# decay for the multiple different species requirements. 

# Criteria 3. The number of CWD pieces. 
#############################################################################################

# CWD Quality Index function
cwdQI <- function(cwdDat){
  # Median and max diameter
  cwdPlot<- cwd[, .(diamMed = mean(Diam_cm, na.rm = TRUE),
                    diamMax = max(Diam_cm, na.rm = TRUE)),
              by = c("PlotID", "SubplotID")]
  cwdPlot <- cwdPlot[!is.na(diamMed)] # remove plots without cwd 
  # Number of cwd pieces/plot
  Ndc <- cwd[, .N, by=.(PlotID, SubplotID, Decay_class)]
  Ndc <- Ndc[!is.na(Decay_class)] # remove plots without cwd
  Ndc[, PlotID:=as.factor(PlotID)][, Decay_class:=as.factor(Decay_class)][SubplotID := as.factor(SubplotID)]
  Ndc[, PlotN:=sum(N), by= c("PlotID", "SubplotID")]
  # Decay class evenness
  Ndc[, DCevenness:= abs(N-(PlotN/5)), by=c("PlotID", "SubplotID", "Decay_class")]
  PlotNdc <- Ndc[,.(DCevenness=DCevenness/PlotN), by=c("PlotID", "SubplotID", "PlotN")][,.(DCevenness=-(sum(DCevenness))), 
                                                                           by=c("PlotID", "SubplotID", "PlotN")]
  # square root of max diameter and number pieces
  cwdPlot <- merge(cwdPlot, PlotNdc)
  cwdPlot <- cwdPlot[,diamMax:=sqrt(diamMax)][,PlotN:= sqrt(PlotN)]
  # scaling variables (mean=0, sd=1)
  cols <- colnames(cwdPlot)[-1]
  cwdPlot[, (cols):=lapply(.SD, scale), .SDcols=cols] 
  # summing variables and normalizing (0-100) for habitat quality index
  cwdPlot[, CQI:=sum(diamMed, diamMax, PlotN, DCevenness), by= c("PlotID", "SubplotID")]
  cwdPlot[, CQI:=((CQI-min(CQI))/(max(CQI)-min(CQI)))*100]
  PlotCQI <- cwdPlot[,.(PlotID, SubplotID, CQI)]
  return(PlotCQI)
}
