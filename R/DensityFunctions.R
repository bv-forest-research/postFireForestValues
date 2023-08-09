#Functions to get live trees for regen and trees 
RegenDensity <- function(regenDat){
  #Regen - already accounted for sub-plot sampling, so can just use PHF here
  RegenLive <- Regen[`Live/Dead`=="L"]
  RegenLive[,PHF:=ifelse(`Sub-Plot`=="A1",100,200)][,SPH := Tally*PHF][,SdlHgt:=ifelse(Height_class=="0-30",1,2)]
  PlotRegen <- RegenLive[,.(SPH=mean(SPH)),by=c("PlotID","Species","SdlHgt")] #take the mean of regen sub-plots
  return(PlotRegen)
}

TreeDensity <- function(treeDat_a1,treeDat_b1){
  ##Trees
  #UniSpecies <- unique(c(A1trees[Tree_class<3,Species],B1trees[Tree_class<3,Species],
   #                      Regen[`Live/Dead`=="L",Species]))#Species in plots:
  FR_LiveTrees <- rbind(A1trees[Tree_class<3,.(PlotID, `Sub-plot`, Species, Tree_class, DBH, Height, Notes)],
                        B1trees[Tree_class<3,.(PlotID, `Sub-plot`, Species, Tree_class, DBH,Height)], fill=TRUE)
  FR_LiveTrees <- FR_LiveTrees[!is.na(DBH)]
  #deal with the different plot sizes and areas surveyed
  FR_LiveTrees[,AreaSearchM2 := ifelse(`Sub-plot`=="A1",100, ifelse(`Sub-plot`=="A2",50,400))]
  FR_LiveTrees[Notes=="only 1/4 of A1",AreaSearchM2:=100/4]
  FR_LiveTrees[Notes=="only 1/2 of A1",AreaSearchM2:=100/2]
  FR_LiveTrees[Notes=="only 1/4 of A2",AreaSearchM2:=50/4]
  FR_LiveTrees[,PHF:=10000/AreaSearchM2] #accounting for smaller search areas

  dbhClSize <- 2
  diamClasses <- seq(0,(max(na.omit(FR_LiveTrees[,DBH]))+dbhClSize), by=dbhClSize)
  for(j in 1:length(diamClasses)){
    FR_LiveTrees[DBH <= diamClasses[j] & DBH > diamClasses[j]-dbhClSize, DBH_bin := diamClasses[j]]
  }
  FR_LiveTrees[,PlotID:=as.factor(PlotID)][,Species:=as.factor(Species)]
  PlotTrees <- FR_LiveTrees[,.(SPH=sum(PHF)),by=c("PlotID","Species","DBH_bin","Height")] #only 1 plot, sum up stems in each diam class
  return(PlotTrees)
  }

SnagDensity <- function(treeDat_a1,treeDat_b1){
  ##Trees
  #UniSpecies <- unique(c(A1trees[Tree_class<3,Species],B1trees[Tree_class<3,Species],
  #                      Regen[`Live/Dead`=="L",Species]))#Species in plots:
  FR_LiveTrees <- rbind(A1trees[Tree_class>2,.(PlotID, `Sub-plot`, Species, Tree_class, DBH, Height, Notes)],
                        B1trees[Tree_class>2,.(PlotID, `Sub-plot`, Species, Tree_class, DBH,Height)], fill=TRUE)
  FR_LiveTrees <- FR_LiveTrees[!is.na(DBH)]
  #deal with the different plot sizes and areas surveyed
  FR_LiveTrees[,AreaSearchM2 := ifelse(`Sub-plot`=="A1",100, ifelse(`Sub-plot`=="A2",50,400))]
  FR_LiveTrees[Notes=="only 1/4 of A1",AreaSearchM2:=100/4]
  FR_LiveTrees[Notes=="only 1/2 of A1",AreaSearchM2:=100/2]
  FR_LiveTrees[Notes=="only 1/4 of A2",AreaSearchM2:=50/4]
  FR_LiveTrees[,PHF:=10000/AreaSearchM2] #accounting for smaller search areas
  
  dbhClSize <- 2
  diamClasses <- seq(0,(max(na.omit(FR_LiveTrees[,DBH]))+dbhClSize), by=dbhClSize)
  for(j in 1:length(diamClasses)){
    FR_LiveTrees[DBH <= diamClasses[j] & DBH > diamClasses[j]-dbhClSize, DBH_bin := diamClasses[j]]
  }
  FR_LiveTrees[,PlotID:=as.factor(PlotID)][,Species:=as.factor(Species)]
  PlotTrees <- FR_LiveTrees[,.(SPH=sum(PHF)),by=c("PlotID","Species","DBH_bin","Height")] #only 1 plot, sum up stems in each diam class
  return(PlotTrees)
}
