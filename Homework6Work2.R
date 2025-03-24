# Homework 6 Creating Fake Data Sets
# 2/19/25
# KRM

# weight variable: n = 490, mean = 202, std dev = 45.7, min = 106, max = 369
# systolic blood pressure variable: n = 496, mean = 140, std dev = 20, min = 87, max = 214

##################################################
# FUNCTION: fit_linear2       
# fits simple regression line
# inputs: numeric vector of predictor (x) and response (y)
# outputs: slope and p-value
#------------------------------------------------- 
fit_linear2 <- function(p=NULL, x_cols=NULL, y_cols=NULL) {
  # Extract data from data frame
  x_data <- p[, x_cols]  # All the rows in column x_cols
  y_data <- p[, y_cols]  # All the rows in column y_cols
  
  # Fit the model with y as a function of x
  my_mod <- lm(y_data ~ x_data)
  
  # Extract slope and intercept
  slope <- summary(my_mod)$coefficients[2, 1]
  intercept <- summary(my_mod)$coefficients[1, 1]
  pvalue <- summary(my_mod)$coefficients[2, 4]
  
  # Format output as slope and p value
  my_out <- c(slope=slope, pvalue=pvalue)
  # Plot output
  plot(x=x_data, y=y_data, xlab="Weight (lbs)", ylab="Systolic BP (mmHg)", main="Weight vs. Systolic BP Regression")
  
  # Add regression line
  abline(a=intercept, b=slope, col="blue", lwd=2)
  
  return(data.frame(slope=slope, pvalue=pvalue))
}

##################################################

adjusting_parameters <- function(n = 500, mean = 100, stddev = 50) {
  variable <- rtruncnorm(n, a=0, mean, stddev)
  return(variable)
}

results_sample_size <- data.frame(mean = numeric(), slope = numeric(), pvalue = numeric())

# Adjusting sample size
for (n in seq(100, 1000, by=100)) {
  weight <- adjusting_parameters(n, 202, 45.7)
  sys_bp <- adjusting_parameters(n, 140, 20)
  data <- data.frame(weight, sys_bp)
  
  # Run analysis
  output <- fit_linear2(data, x_cols=1, y_cols=2)
  slope <- output[1]
  pvalue <- output[2]
  
  results_sample_size <- rbind(results_sample_size, data.frame(mean = mean, slope = slope, pvalue = pvalue))
}

results_stddev <- data.frame(mean = numeric(), slope = numeric(), pvalue = numeric())
# Adjusting stddev
for (stddev in seq(10, 100, by=10)) {
  weight <- adjusting_parameters(500, 202, stddev+25)
  sys_bp <- adjusting_parameters(500, 140, stddev)
  data <- data.frame(weight, sys_bp)
  
  # Run analysis
  output <- fit_linear2(data, x_cols=1, y_cols=2)
  slope <- output[1]
  pvalue <- output[2]
  
  results_stddev <- rbind(results_stddev, data.frame(mean = mean, slope = slope, pvalue = pvalue))
}

results_mean <- data.frame(mean = numeric(), slope = numeric(), pvalue = numeric())
# Adjusting mean
for (mean in seq(50, 400, by=25)) {
  weight <- adjusting_parameters(500, mean+50, 45.7)
  sys_bp <- adjusting_parameters(500, mean, 20)
  data <- data.frame(weight, sys_bp)
  
  # Run analysis
  output <- fit_linear2(data, x_cols=1, y_cols=2)
  slope <- output[1]
  pvalue <- output[2]
  
  results_mean <- rbind(results_mean, data.frame(mean = mean, slope = slope, pvalue = pvalue))
}

print(results_sample_size)
print(results_stddev)
print(results_mean)









