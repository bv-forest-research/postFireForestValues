# Coarse woody debris volume/ha (line intersect method) by decay class and species
cwdVol <- function(cwdDat, lineDat){
  PlotLine <- lineDat[,.(HorizontalDist = sum(HorizontalDist)),
                      by="PlotID"]
  # Calculate volume using VanWagner volume equation
  PlotCWDvol <- cwdDat[, .(D2 = sum(Diam_cm^2)), 
                       by=c("PlotID", "Decay_class", "Species")]
  PlotCWDvol <- merge(PlotCWDvol, PlotLine)
  PlotCWDvol[, VolHa:= pi^2/(8*HorizontalDist)*D2]
  PlotCWDvol[,c("D2", "HorizontalDist"):=NULL]
  
  return(PlotCWDvol)
}

fwdVol <- function(fwdDat, lineDat){
  
  PlotLine <- lineDat[,.(HorizontalDist = sum(`Horizontal distance: adj. line length`)),
                      by="Plot"]
  # fwd only measured on 10m of each line (=20m both lines), 
  #so subtract remaing 80 m from total length
  PlotLine[, fwd.dist := HorizontalDist - 80, 
           by = seq_len(nrow(PlotLine))]
  
  fwd.plot <- fwdDat[, .(Tally = sum(Tally)), 
                     by = c("PlotID","Diam_class")]
  # Merge line and fwd data
  fwd.line.plot <- merge(fwd.plot, PlotLine, 
                         by.x =  "PlotID", by.y = "Plot",
                         all.x = TRUE)
  
  #------- VOLUME ------------#
  # Create QMD column (mean of diameter class)
  qmdFN <- function(Diam_class){
    if(is.na(Diam_class)){
      print(paste("Diam_class is not found"))
      Diam <- NA
    } else {
      if (Diam_class == "1.1-2.5"){
        Diam <- (2.5+1.1)/2
      } else if (Diam_class == "2.6-5"){
        Diam <- (5+2.6)/2
      } else if (Diam_class == "5.1-7.5"){
        Diam <- (7.5+5.1)/2
      } else {
        print(paste("Diam_class",Species,"not found"))
        Diam <- NA
      }
    }
    return(Diam)
  }
  
  # Assign QMD to new column
  qmd <- vector()
  for(i in 1:nrow(fwd.line.plot)){
    qmd[i] <- qmdFN(Diam_class = fwd.line.plot[i, "Diam_class"])
  }
  fwd.line.plot$QMD <- qmd
  
  
  # Create volumn calculation function
  fwdVolFN <- function(L, n, QMD){
    fwd_vol <- ((pi^2/(8*L)) * n *(QMD^2))
    return(fwd_vol)
  }
  
  # Calculate fwd volume in each size class
  fwd.line.plot$volume_ha <- fwdVolFN(L = fwd.line.plot$fwd.dist, 
                                      n = fwd.line.plot$Tally, 
                                      QMD = fwd.line.plot$QMD)
  
  fwd.line.plot <- as.data.table(fwd.line.plot)
  fwd.line.plot[,`:=`(HorizontalDist = NULL,fwd.dist = NULL,
                      QMD = NULL,Tally = NULL)]
  
  return(fwd.line.plot)
  
}
