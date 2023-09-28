


# Basal Area Per Hectare function
BAPH <- function(treeDat_a1,treeDat_b1){
  ##Trees
  FR_LiveTrees <- rbind(A1trees[Tree_class < 3, .(PlotID, `Sub-plot`, Species, 
                                                  Tree_class, DBH, Height, Notes)],
                        B1trees[Tree_class < 3, .(PlotID, `Sub-plot`, Species, 
                                                  Tree_class, DBH, Height)], fill = TRUE)
  FR_LiveTrees <- FR_LiveTrees[!is.na(DBH)]
  #deal with the different plot sizes and areas surveyed
  FR_LiveTrees[, AreaSearchM2 := ifelse(`Sub-plot` == "A1", 100, 
                                    ifelse(`Sub-plot` == "A2", 50, 400))]
  FR_LiveTrees[Notes == "only 1/4 of A1", AreaSearchM2 := 100 / 4]
  FR_LiveTrees[Notes == "only 1/2 of A1", AreaSearchM2 := 100 / 2]
  FR_LiveTrees[Notes == "only 1/4 of A2", AreaSearchM2 := 50 / 4]
  FR_LiveTrees[, PHF := 10000 / AreaSearchM2] #accounting for smaller search areas
  
  FR_LiveTrees[, BA := pi * (DBH ^ 2 / 40000)]
  FR_LiveTrees[, BAPH := BA * PHF]
  
  PlotTrees <- FR_LiveTrees[, .(PlotID, Species, DBH, Height, BAPH)]
  return(PlotTrees)
  
}


BAPHsizeCl <- function(treeDat_a1,treeDat_b1){
  ##Trees
  FR_LiveTrees <- rbind(A1trees[Tree_class < 3, .(PlotID, `Sub-plot`, Species, 
                                                  Tree_class, DBH, Height, Notes)],
                        B1trees[Tree_class < 3, .(PlotID, `Sub-plot`, Species, 
                                                  Tree_class, DBH, Height)], fill = TRUE)
  FR_LiveTrees <- FR_LiveTrees[!is.na(DBH)]
  #deal with the different plot sizes and areas surveyed
  FR_LiveTrees[, AreaSearchM2 := ifelse(`Sub-plot` == "A1", 100, ifelse(`Sub-plot` ==
                                                                          "A2", 50, 400))]
  FR_LiveTrees[Notes == "only 1/4 of A1", AreaSearchM2 := 100 / 4]
  FR_LiveTrees[Notes == "only 1/2 of A1", AreaSearchM2 := 100 / 2]
  FR_LiveTrees[Notes == "only 1/4 of A2", AreaSearchM2 := 50 / 4]
  FR_LiveTrees[, PHF := 10000 / AreaSearchM2] #accounting for smaller search areas
  
  FR_LiveTrees[, BA := pi * (DBH ^ 2 / 40000)]
  FR_LiveTrees[, BAPH := BA * PHF]
  
  
  diamClasses <- c(0, 7.5, 12.5, 17.5, 22.5)
  for (j in 1:length(diamClasses)) {
    FR_LiveTrees[DBH >= diamClasses[j], DBH_bin := diamClasses[j]]
  }
  FR_LiveTrees[, PlotID := as.factor(PlotID)][, Species := as.factor(Species)]
  PlotTrees <- FR_LiveTrees[, .(BAPH = sum(BAPH)), 
                            by = c("PlotID", "Species", "DBH_bin")] #only 1 plot, sum up stems in each diam class
  return(PlotTrees)
}
