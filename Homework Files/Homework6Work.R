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
  x_data <- p[, x_cols]  # Independent variable (e.g., weight)
  y_data <- p[, y_cols]  # Dependent variable (e.g., sys_bp)
  
  # Fit the model with y as a function of x
  my_mod <- lm(y_data ~ x_data)
  
  # Extract slope and intercept
  slope <- summary(my_mod)$coefficients[2, 1]
  intercept <- summary(my_mod)$coefficients[1, 1]
  
  # Format output as slope and p value
  my_out <- c(slope=slope, p_value=summary(my_mod)$coefficients[2, 4])
  # Plot output
  plot(x=x_data, y=y_data, xlab="Weight (lbs)", ylab="Systolic BP (mmHg)", main="Weight vs. Systolic BP Regression")
  
  # Add regression line
  abline(a=intercept, b=slope, col="blue", lwd=2)
  
  return(my_out)
}

##################################################

std_dev_effect <- function(stddev=50) {
  pvalue <- 1
  count <- 1
  while (pvalue > 0.05) {
    # Randomize data
    weight <- rnorm(500, 202, stddev)
    sys_bp <- rnorm(500, 140, stddev)
    data <- data.frame(weight, sys_bp)
    
    # Rerun analysis
    output <- fit_linear2(data, x_cols=1, y_cols=2)
    slope <- output[1]
    pvalue <- output[2]
    
    count <- count + 1
  }
  print("p-value is significant")
  print(paste("Slope:", slope))
  print(paste("p-value:", pvalue))
  print(paste("count:", count))
  out <- c(count = count, pvalue = pvalue, slope=slope)
  return(out)
}

sample_size_effect <- function(n=500) {
  pvalue <- 1
  count <- 1
  while (pvalue > 0.05) {
    # Randomize data
    weight <- rnorm(n, 202, 45.7)
    sys_bp <- rnorm(n, 140, 20)
    data <- data.frame(weight, sys_bp)
    
    # Rerun analysis
    output <- fit_linear2(data, x_cols=1, y_cols=2)
    slope <- output[1]
    pvalue <- output[2]
    
    count <- count + 1
  }
  print("p-value is significant")
  print(paste("Slope:", slope))
  print(paste("p-value:", pvalue))
  print(paste("count:", count))
  out <- c(count = count, pvalue = pvalue, slope=slope)
  return(out)
}

n <- 100
count_total <- 0
upper_lim <- 1000
increment <- 100
repetitions <- 50
i <- 1
average_counts <- data.frame(sample_size = numeric(), avg_count = numeric())

# Main loop
while (n <= upper_lim) {
  for (number in seq(1, repetitions)) {
    print(paste("Sample size:", n))
    # Get the output from sample_size_effect(n)
    final_output <- sample_size_effect(n)
    count_total <- count_total + final_output[1]
    print("")  # Empty line for readability
  }
  
  # Calculate and store the average count
  count_average <- count_total / repetitions
  # Add a new row to average_counts (dframe = average_counts, new_data = new data frame with sample size and avg count)
  new_data <- list(sample_size = n, avg_count = count_average)
  average_counts <- rbind(average_counts, new_data)
  
  # Increment n and reset count_total
  n <- n + increment
  count_total <- 0
  count_average <- 0
  i <- i + 1
}

print(average_counts)

# Testing standard deviation effects
n <- 100
stddev <- 0
count_total <- 0
upper_lim <- 100
increment <- 10
repetitions <- 10
i <- 1
average_counts <- data.frame(stddev = numeric(), avg_count = numeric())

# Main loop
while (stddev <= upper_lim) {
  for (number in seq(1, repetitions)) {
    print(paste("Std dev:", stddev))
    # Get the output from sample_size_effect(n)
    final_output <- std_dev_effect(stddev)
    count_total <- count_total + final_output[1]
    print("")  # Empty line for readability
  }
  
  # Calculate and store the average count
  count_average <- count_total / repetitions
  # Add a new row to average_counts (dframe = average_counts, new_data = new data frame with sample size and avg count)
  new_data <- list(std_dev = stddev, avg_count = count_average)
  average_counts <- rbind(average_counts, new_data)
  
  # Increment n and reset count_total
  stddev <- stddev + increment
  count_total <- 0
  count_average <- 0
  i <- i + 1
}

print(average_counts)




