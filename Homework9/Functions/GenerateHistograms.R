# --------------------------------------
# FUNCTION generate_histograms
# required packages: none
# description:
# inputs:
# outputs:
########################################
generate_histograms <- function(data){
  library(ggplot2)
  # Create histogram for Species Richness
  species_richness_histogram <- ggplot(data, aes(x = species_richness)) +
    geom_histogram(binwidth = 5, fill = "skyblue", color = "black", alpha = 0.7) +
    labs(title = "Histogram of Species Richness (S)", x = "Species Richness (width = 5)", y = "Frequency") +
    theme_minimal() + 
    scale_x_continuous(breaks = seq(0, max(species_richness)+100, by = 5))
  
  # Create histogram for Abundance
  abundance_histogram <- ggplot(data, aes(x = abundance)) +
    geom_histogram(binwidth = 50, fill = "lightgreen", color = "black", alpha = 0.7) +
    labs(title = "Histogram of Abundance", x = "Abundance (width = 50)", y = "Frequency") +
    theme_minimal() + 
    scale_x_continuous(breaks = seq(0, max(abundance)+500, by = 50))
  
  
  return(list(species_richness_histogram = species_richness_histogram, 
              abundance_histogram = abundance_histogram))

} # end of function generate_histograms
# --------------------------------------
# generate_histograms()
