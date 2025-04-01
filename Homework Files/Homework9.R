library(devtools)
library(upscaler)


count <- 1
year <- 2015

bird_df <- data.frame()
file_names <- list()
file_paths <- list()

folders <- list.files()

# For loop to get all countdata file paths (for read.csv)
for (folder in folders) {
  # Get folder path
  folder_path <- file.path(getwd(), folder)
  
  # Get file path and file name for each countdata csv file
  file_paths[count] <- list.files(folder_path, pattern = "countdata", full.names = TRUE)
  file_names[count] <- list.files(folder_path, pattern = "countdata", full.names = FALSE)
  count = count+1
}

build_function("clean_data")
build_function("extract_year")
build_function("calculate_abundance")
build_function("calc_species_richness")
build_function("regression_model")
build_function("generate_histograms")

# Create an initial empty data frame to hold the above summary statistics-you should have columns for the file name, one for abundance, one for species richness, one for year, and the regression model summary statistics.

bird_data <- data.frame(
  file_name = character(),
  abundance = numeric(),
  species_richness = numeric(),
  year = character(),
)

# Using a for loop, run your created functions as a batch process for each folder, changing the working directory as necessary to read in the correct files, calculating summary statistics with your created functions, and then writing them out into your summary statistics data frame.








