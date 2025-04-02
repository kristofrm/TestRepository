# Homework 8 work

library(ggplot2) # for graphics
library(MASS) # for maximum likelihood estimation

# My data
z <- read.csv("/Users/kristofrm/Downloads/obesity_data.csv")
colnames(z)[colnames(z) == "Weight"] <- "myVar"
str(z)
summary(z)

# Plotting histogram
p1 <- ggplot(data=z, aes(x=myVar, y=..density..)) +
  geom_histogram(color="grey60",fill="cornsilk",size=0.2) 
print(p1)

# Add empirical density curve
p1 <-  p1 +  geom_density(linetype="dotted",size=0.75)
print(p1)

# Get parameters for normal
normPars <- fitdistr(z$myVar,"normal")
print(normPars)
str(normPars)x
normPars$estimate["mean"] # note structure of getting a named attribute

# Plot normal probability density
meanML <- normPars$estimate["mean"]
sdML <- normPars$estimate["sd"]

xval <- seq(0,max(z$myVar),len=length(z$myVar))

stat <- stat_function(aes(x = xval, y = ..y..), fun = dnorm, colour="red", n = length(z$myVar), args = list(mean = meanML, sd = sdML))
p1 + stat

# Plot exponential probability density
expoPars <- fitdistr(z$myVar,"exponential")
rateML <- expoPars$estimate["rate"]

stat2 <- stat_function(aes(x = xval, y = ..y..), fun = dexp, colour="blue", n = length(z$myVar), args = list(rate=rateML))
p1 + stat + stat2

# Plot uniform probability density
stat3 <- stat_function(aes(x = xval, y = ..y..), fun = dunif, colour="darkgreen", n = length(z$myVar), args = list(min=min(z$myVar), max=max(z$myVar)))
p1 + stat + stat2 + stat3

# Plot gamma probability density
gammaPars <- fitdistr(z$myVar,"gamma")
shapeML <- gammaPars$estimate["shape"]
rateML <- gammaPars$estimate["rate"]

stat4 <- stat_function(aes(x = xval, y = ..y..), fun = dgamma, colour="brown", n = length(z$myVar), args = list(shape=shapeML, rate=rateML))
p1 + stat4

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
# Function: my_histo
# Purpose: creates a ggplot histogram
# Requires: ggplot
# Input: x = a numeric vector
#        data_type= "cont" or "disc"
# Output: a ggplot histogram
############################
library(ggplot2)
my_histo <- function(x=NULL,data_type="cont") {
  df <- data.frame(x=x) 
  
  # if data are continuous bounded (0,1), adjust bins for histogram  
  if (data_type=="cont" & min(df$x) > 0 & max(df$x) < 1) {
    p1 <- ggplot(data=df) +
      aes(x=x) +
      geom_histogram(boundary=0,
                     binwidth=1/30,
                     color="black",
                     fill="goldenrod") +
      scale_x_continuous(limits=c(0,1))}  
  
  print(p1)
} 

# Plotting simulated data set based on gamma distribution parameters
gammaPars <- fitdistr(z$myVar,"gamma")
shapeML <- gammaPars$estimate["shape"]
rateML <- gammaPars$estimate["rate"]
length_data <- gammaPars$n

# Making gamma distribution using above parameters
simulated_gamma <- rgamma(n=length_data,shape=shapeML,rate=rateML)

# Plotting histogram
df <- data.frame(x=simulated_gamma)
p1 <- ggplot(data=df, aes(x=x, y=..density..)) +
  geom_histogram(color="black",fill="goldenrod",size=0.2) 

# Getting distribution line
stat4 <- stat_function(aes(x = xval, y = ..y..), fun = dgamma, colour="brown", n = length(z$myVar), args = list(shape=shapeML, rate=rateML))

p1 + stat4

# How do the two histogram profiles compare? Do you think the model is doing a good job of simulating realistic data that match your original measurements? Why or why not?

# 
