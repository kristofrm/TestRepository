library(devtools)
library(upscaler)


count <- 1
year <- 2015
setwd("/Users/kristofrm/TestRepository/NEON_count-landbird")

file_paths <- list()

folders <- list.files()

# For loop to get all countdata file paths (for read.csv)
for (folder in folders) {
  # Get folder pat
  folder_path <- file.path(getwd(), folder)
  
  # Get file path and file name for each countdata csv file
  file_paths[count] <- list.files(folder_path, pattern = "countdata", full.names = TRUE)
  count = count+1
}

build_function("clean_data")
build_function("extract_year")
build_function("calculate_abundance")
build_function("calc_species_richness")
build_function("regression_model")
build_function("generate_histograms")

# Loading the above newly made functions
r_files <- list.files("~/TestRepository/Functions", pattern = "\\.R$", full.names = TRUE)

for (file in r_files) {
  source(file)
}

# Create an initial empty data frame to hold the above summary statistics-you should have columns for the file name, one for abundance, one for species richness, one for year, and the regression model summary statistics.

bird_data <- data.frame(
  file_name = character(),
  abundance = numeric(),
  species_richness = numeric(),
  year = character()
)

# Using a for loop, run your created functions as a batch process for each folder, changing the working directory as necessary to read in the correct files, calculating summary statistics with your created functions, and then writing them out into your summary statistics data frame.


for (i in 1:length(file_paths)) {
  data <- read.csv(file_paths[[i]])
  file_name <- basename(file_paths[[i]])
  
  data <- clean_data(data)
  year <- extract_year(file_name)
  abundance <- calculate_abundance(data)
  species_richness <- calc_species_richness(data)
  
  bird_data <- rbind(bird_data, data.frame(
    file_name = file_name,
    abundance = abundance,
    species_richness = species_richness,
    year = year
  ))
}

# Regression model and histogram for bird_data species richness and abundance
regression_model <- regression_model(bird_data)
histograms <- generate_histograms(bird_data)

