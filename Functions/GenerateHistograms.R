# --------------------------------------
# FUNCTION generate_histograms
# required packages: none
# description:
# inputs:
# outputs:
########################################
generate_histograms <- function(data){
  # Create histogram for Species Richness
  species_richness_histogram <- ggplot(data, aes(x = species_richness)) +
    geom_histogram(binwidth = 5, fill = "skyblue", color = "black", alpha = 0.7) +
    labs(title = "Histogram of Species Richness (S)", x = "Species Richness (S)", y = "Frequency") +
    theme_minimal()
  
  # Create histogram for Abundance
  abundance_histogram <- ggplot(data, aes(x = abundance)) +
    geom_histogram(binwidth = 50, fill = "lightgreen", color = "black", alpha = 0.7) +
    labs(title = "Histogram of Abundance", x = "Abundance", y = "Frequency") +
    theme_minimal()
  
  return(list(species_richness_histogram = species_richness_histogram, 
              abundance_histogram = abundance_histogram))

} # end of function generate_histograms
# --------------------------------------
# generate_histograms()
