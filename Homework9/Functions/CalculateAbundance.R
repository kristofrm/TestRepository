# --------------------------------------
# FUNCTION calculate_abundance
# required packages: none
# description: function to return the number of individuals found in a data set
# inputs: data: the data set
# outputs: abundance: the abundance for the data set
########################################
calculate_abundance <- function(data){
  # Assuming no repeat rows
  abundance <- nrow(data)
  
  return(abundance)

} # end of function calculate_abundance
# --------------------------------------
# calculate_abundance()
