# Homework 6 Creating Fake Data Sets
# 2/19/25
# KRM

library(truncnorm)

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
  my_out <- data.frame(slope=slope, pvalue=pvalue)
  # Plot output
  plot(x=x_data, y=y_data, xlab="Weight (lbs)", ylab="Systolic BP (mmHg)", main="Weight vs. Systolic BP Regression")
  
  # Add regression line
  abline(a=intercept, b=slope, col="blue", lwd=2)
  
  return(my_out)
}

##################################################

adjusting_parameters <- function(n = 500, mean = 100, stddev = 50) {
  return(rtruncnorm(n, a=0, mean=mean, sd=stddev))
}

# Initialize data frame for varied sample sizes
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
  
  results_sample_size <- rbind(results_sample_size, data.frame(sample_size = n, slope = slope, pvalue = pvalue))
}

# Initialize data frame for varied std dev
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
  
  results_stddev <- rbind(results_stddev, data.frame(stddev = stddev, slope = slope, pvalue = pvalue))
}

# Initialize data frame for varied mean
results_mean <- data.frame(mean = numeric(), slope = numeric(), pvalue = numeric())

# Adjusting mean
for (mean_value in seq(50, 400, by=25)) {
  weight <- adjusting_parameters(500, mean_value+50, 45.7)
  sys_bp <- adjusting_parameters(500, mean_value, 20)
  data <- data.frame(weight, sys_bp)
  
  # Run analysis
  output <- fit_linear2(data, x_cols=1, y_cols=2)
  slope <- output[1]
  pvalue <- output[2]
  
  results_mean <- rbind(results_mean, data.frame(mean = mean_value, slope = slope, pvalue = pvalue))
}

print(results_sample_size)
print(results_stddev)
print(results_mean)

# Determining smallest difference between groups to still get significant pattern
results_effect_size <- data.frame(mean_diff = numeric(), avg_pvalue = numeric())

for (mean_diff in seq(1, 20, by=1)) {  # Test effect sizes from 1 to 20 mmHg
  pvalues <- numeric() # Initialize storage for p values
  
  for (i in 1:100) {  # Run each effect size 100 times to account for random variation
    weight <- adjusting_parameters(500, 202, 45.7)
    sys_bp_original <- adjusting_parameters(500, 140, 20) # Control sys bp
    sys_bp_altered <- adjusting_parameters(500, 140 + mean_diff, 20) # Adjusted sys bp
    
    # Combine data frames holding weight, sys_bp
    data_original <- data.frame(weight = weight, sys_bp = sys_bp_original, group = "Original")
    data_altered <- data.frame(weight = weight, sys_bp = sys_bp_altered, group = "Altered")
    data_combined <- rbind(data_original, data_altered)
    
    # Run ANOVA test on sys_bp based on original and altered sys_bp
    anova_result <- aov(sys_bp ~ group, data=data_combined)
    pvalues[i] <- summary(anova_result)[[1]][["Pr(>F)"]][1]  # Notation to get p value
  }
  
  # Get average p-value across 100 runs
  avg_pvalue <- mean(pvalues)
  
  # Store results
  results_effect_size <- rbind(results_effect_size, data.frame(mean_diff = mean_diff, avg_pvalue = avg_pvalue))
  
  # Exit loop if significant result is reached
  if (avg_pvalue <= 0.05) {
    # Print smallest effect size
    print(paste("Smallest effect size for significant pattern:", mean_diff))
    break
  }
}

print(results_effect_size)

# Plot showing smallest effect size with significance threshold
plot(results_effect_size$mean_diff, results_effect_size$avg_pvalue, type="b", col="blue", xlab="Effect Size (Mean Difference)", ylab="Average P-Value", main="Effect Size vs. P-Value")
# Add significance threshold
abline(h=0.05, col="red", lty=2)


# Determining smallest sample size to still get significant pattern
results_sample_size <- data.frame(sample_size = numeric(), avg_pvalue = numeric())

# Minimum effect size from above = 4
mean_diff <- 4

# Vary the sample size (from 100 to 1000, in steps of 100)
for (sample_size in seq(10, 1000, by=100)) {
  
  pvalues <- numeric()  # Store p-values for multiple runs
  
  for (i in 1:100) {  # Run 100 simulations for each sample size
    weight <- adjusting_parameters(sample_size, 202, 45.7)
    sys_bp_original <- adjusting_parameters(sample_size, 140, 20)  
    sys_bp_altered <- adjusting_parameters(sample_size, 140 + mean_diff, 20)
    
    # Combine data frames holding weight, sys_bp
    data_original <- data.frame(weight = weight, sys_bp = sys_bp_original, group = "Original")
    data_altered <- data.frame(weight = weight, sys_bp = sys_bp_altered, group = "Altered")
    data_combined <- rbind(data_original, data_altered)
    
    # Run ANOVA test on sys_bp based on original and altered sys_bp
    anova_result <- aov(sys_bp ~ group, data=data_combined)
    pvalues[i] <- summary(anova_result)[[1]][["Pr(>F)"]][1]  # Notation to get p value
  }
  
  # Get average p-value across 100 runs
  avg_pvalue <- mean(pvalues)
  
  # Store results
  results_sample_size <- rbind(results_sample_size, data.frame(sample_size = sample_size, avg_pvalue = avg_pvalue))
  
  # Exit loop if significant result is reached
  if (avg_pvalue <= 0.05) {
    # Print smallest effect size
    print(paste("Minimum sample size for significant pattern:", sample_size))
    break
  }
}

print(results_sample_size)

# Plot showing smallest sample size for significant result
plot(results_sample_size$sample_size, results_sample_size$avg_pvalue, type="b", col="blue", xlab="Sample Size", ylab="Average P-Value", main="Sample Size vs. P-Value (Effect Size = 5 mmHg)")
# Add significance threshold
abline(h=0.05, col="red", lty=2)







