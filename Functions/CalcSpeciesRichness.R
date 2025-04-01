# --------------------------------------
# FUNCTION calc_species_richness
# required packages: none
# description:
# inputs:
# outputs:
########################################
calc_species_richness <- function(data){
  # Get species from data frame
  species <- data$scientificName
  
  # Get count of unique species
  species_richness = length(unique(species))
  
  return(species_richness)
  
} # end of function calc_species_richness
# --------------------------------------
# calc_species_richness()
