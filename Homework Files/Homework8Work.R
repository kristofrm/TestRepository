# Homework 8 work

library(ggplot2) # for graphics
library(MASS) # for maximum likelihood estimation

# My data
z <- read.csv("/Users/kristofrm/Downloads/obesity_data.csv")
colnames(z)[colnames(z) == "Weight"] <- "myVar"
str(z)
summary(z)

# Plotting histogram
real_data <- ggplot(data=z, aes(x=myVar, y=..density..)) +
  geom_histogram(color="grey60",fill="cornsilk",size=0.2) 
print(real_data)

# Add empirical density curve
real_data <-  real_data +  geom_density(linetype="dotted",size=0.75)
print(real_data)

# Get parameters for normal
normPars <- fitdistr(z$myVar,"normal")
print(normPars)
str(normPars)
normPars$estimate["mean"] # note structure of getting a named attribute

# Plot normal probability density
meanML <- normPars$estimate["mean"]
sdML <- normPars$estimate["sd"]

xval <- seq(0,max(z$myVar),len=length(z$myVar))

stat <- stat_function(aes(x = xval, y = ..y..), fun = dnorm, colour="red", n = length(z$myVar), args = list(mean = meanML, sd = sdML))
real_data + stat

# Plot exponential probability density
expoPars <- fitdistr(z$myVar,"exponential")
rateML <- expoPars$estimate["rate"]

stat2 <- stat_function(aes(x = xval, y = ..y..), fun = dexp, colour="blue", n = length(z$myVar), args = list(rate=rateML))
real_data + stat + stat2

# Plot uniform probability density
stat3 <- stat_function(aes(x = xval, y = ..y..), fun = dunif, colour="darkgreen", n = length(z$myVar), args = list(min=min(z$myVar), max=max(z$myVar)))
real_data + stat + stat2 + stat3

# Plot gamma probability density
gammaPars <- fitdistr(z$myVar,"gamma")
shapeML <- gammaPars$estimate["shape"]
rateML <- gammaPars$estimate["rate"]

gamma_distribution_line <- stat_function(aes(x = xval, y = ..y..), fun = dgamma, colour="brown", n = length(z$myVar), args = list(shape=shapeML, rate=rateML))
real_data + gamma_distribution_line

# Plot beta probability density
pSpecial <- ggplot(data=z, aes(x=myVar/(max(myVar + 0.1)), y=..density..)) +
  geom_histogram(color="grey60",fill="cornsilk",size=0.2) + 
  xlim(c(0,1)) +
  geom_density(size=0.75,linetype="dotted")

betaPars <- fitdistr(x=z$myVar/max(z$myVar + 0.1),start=list(shape1=1,shape2=2),"beta")
shape1ML <- betaPars$estimate["shape1"]
shape2ML <- betaPars$estimate["shape2"]

statSpecial <- stat_function(aes(x = xval, y = ..y..), fun = dbeta, colour="orchid", n = length(z$myVar), args = list(shape1=shape1ML,shape2=shape2ML))
pSpecial + statSpecial

#-----------------------------------------------------------------

# Plotting simulated data set based on gamma distribution parameters
gammaPars <- fitdistr(z$myVar,"gamma")
shapeML <- gammaPars$estimate["shape"]
rateML <- gammaPars$estimate["rate"]
length_data <- gammaPars$n

# Making gamma distribution using above parameters
simulated_gamma <- rgamma(n=length_data,shape=shapeML,rate=rateML)

# Plotting histogram
df <- data.frame(x=simulated_gamma)
estimated_data <- ggplot(data=df, aes(x=x, y=..density..)) +
  geom_histogram(color="black",fill="goldenrod",size=0.2) 

# Getting distribution line
stat4 <- stat_function(aes(x = xval, y = ..y..), fun = dgamma, colour="brown", n = length(z$myVar), args = list(shape=shapeML, rate=rateML))

estimated_data + stat4

# Real data again
real_data + gamma_distribution_line


# How do the two histogram profiles compare? Do you think the model is doing a good job of simulating realistic data that match your original measurements? Why or why not?
