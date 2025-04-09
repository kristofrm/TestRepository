# --------------------------------------
# FUNCTION clean_data
# required packages: none
# description: function to remove rows with NA values from data set
# inputs: data: a data set object (data frame, matrix, list, etc.)
# outputs: data_cleaned: data set object excluding rows with NA values
########################################
clean_data <- function(data){
  
  # Get boolean matrix where NAs are set to true and values are set to false
  bool_NAs <- is.na(data)
  # Get the number of NAs in a column by summing the 1s (NA values)
  num_NAs <- colSums(bool_NAs)
  # Check if the number of NAs in a column = the number of rows in the dataset
  na_columns <- num_NAs == nrow(data)
  
  # Subset data to be all the rows that ARE NOT strictly NAs
  data <- data[, !na_columns]
  
  # Return data without rows that have NA values
  return(na.omit(data))

} # end of function clean_data
# --------------------------------------
# clean_data()
