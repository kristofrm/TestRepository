# --------------------------------------
# FUNCTION extract_year
# required packages: stringr
# description: function to extract the year from a given file name
# inputs: file_name: the file name to extract the year from
# outputs: the year of the file name
########################################
extract_year <- function(file_name){
  library(stringr)
  # Create pattern regex for possible years
  years <- as.character(2015:2023)
  pattern <- paste(years, collapse = "|")
  
  # Extract the year from the filename
  year <- str_extract(file_name, pattern)
  
  return(year)

} # end of function extract_year
# --------------------------------------
# extract_year()
