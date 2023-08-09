# Coarse woody debris volume/ha (line intersect method) by decay class and species
cwdVol <- function(cwdDat, lineDat){
  PlotLine <- line[,.(HorizontalDist=sum(HorizontalDist)), by="PlotID"]
  # Calculate volume using VanWagner volume equation
  PlotCWDvol <- cwd[, .(D2=sum(Diam_cm^2)), by=c("PlotID", "Decay_class", "Species")]
  PlotCWDvol <- merge(PlotCWDvol, PlotLine)
  PlotCWDvol[, VolHa:= pi^2/(8*HorizontalDist)*D2]
  PlotCWDvol[,c("D2", "HorizontalDist"):=NULL]
  return(PlotCWDvol)
}
