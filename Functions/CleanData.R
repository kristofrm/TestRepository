# --------------------------------------
# FUNCTION clean_data
# required packages: none
# description: function to remove rows with NA values from data set
# inputs: data: a data set object (data frame, matrix, list, etc.)
# outputs: data_cleaned: data set object excluding rows with NA values
########################################
clean_data <- function(data){

  # Get rows without NA values
  complete_rows <- complete.cases(data)
  
  # Keep only rows without NAs
  data_cleaned <- data[complete_rows, ]
  
  return(data_cleaned)

} # end of function clean_data
# --------------------------------------
# clean_data()
