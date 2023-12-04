# CWD piles
# this code detects whether >3 pieces of cwd overlap (a pile) and counts the number of piles

cwdPiles <- function(cwdDat){
  # Prep the data
  cwdDat$Transect <- as.factor(cwdDat$Transect)
  cwdDat[, Diam_m := Diam_cm*0.01] # make sure location and diameter are the same units
  cwdDat <- cwdDat[order(PlotID, Transect, Location_m)] # make sure location data is ordered properly - lowest to highest


  # Function to check overlap
  check_overlap <- function(cwd1, cwd2) {
    overlap_condition <- abs(cwd1$Location_m - cwd2$Location_m) <= (cwd1$Diam_m/2 + cwd2$Diam_m/2)
    return(overlap_condition)
  }

  # Initialize a count for piles per transect per plot
  piles <- data.table(PlotID = integer(), Transect = integer(), PileCount = integer())

  # Loop through each combination of PlotID and Transect
  for (plot_id in unique(cwdDat$PlotID)) {
    for (transect in unique(cwdDat$Transect)) {
      # Subset data for the specific PlotID and Transect
      subset_data <- cwdDat[PlotID == plot_id & Transect == transect, ]
    
      # Check if the subset is not empty
      if (nrow(subset_data) > 1) {  # Ensure at least 2 pieces are available for a potential pile
        # Create a vector of TRUE/FALSE indicating whether each pair of CWD pieces overlaps
        overlap_vector <- sapply(1:(nrow(subset_data)-1), function(i) {
          check_overlap(subset_data[i, ], subset_data[i + 1, ])
        })
      
        # Identify groups of consecutive TRUE values
        consecutive_groups <- rle(overlap_vector)
      
        # Count the number of groups with at least 2 TRUEs
        pile_count <- sum(consecutive_groups$values & consecutive_groups$length >= 2)
      
        # Append the result to the piles data.table
        piles <- rbind(piles, data.table(PlotID = plot_id, Transect = transect, PileCount = pile_count))
      }
    }
  }
  # Return the result
  return(piles)
}