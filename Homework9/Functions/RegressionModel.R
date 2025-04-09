# --------------------------------------
# FUNCTION regression_model
# required packages: none
# description:
# inputs:
# outputs:
########################################
regression_model <- function(data){
  
  model <- lm(species_richness ~ abundance, data = data)
  
  return(model)

} # end of function regression_model
# --------------------------------------
# regression_model()
