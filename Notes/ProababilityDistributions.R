# Notes on probability distributions

#####################
# Function: my_histo
# Purpose: creates a ggplot histogram
# Requires: ggplot2
# Input: a numeric vector
# Output: a ggplot histogram
library(ggplot2)
my_histo <- function(x=NULL) {
  if (is.null(x)) df=data.frame(x=runif(1000)) else df <- data.frame(x=x)
  p1 <- ggplot(data=df) + aes(x=x) + geom_histogram(color='black',fill='goldenrod')
  print(p1)
}

my_histo()

library(MASS)
#-----------------------
# Poisson distribution
# Discrete X >= 0
# Random events with a constant rate lambda
# (observations per time or per unit area)
# Parameter lambda > 0

# note: "zero-inflated poisson" has an additional process
# generating zeroes (see ifelse())

# "d" function generates probability density
hits <- 0:10
myVec <- dpois(x=hits,lambda=1)
my_histo(myVec)
plot(myVec,type="b")

hits <- 0:15
myVec <- dpois(x=hits, lambda=20)
plot(myVec,type="b")

sum(myVec)
head(myVec)

dpois(x=0,lambda=2)

# ppois is cdf
hits <- 0:10
myVec <- ppois(q=hits, lambda=2)
plot(myVec, type="b")

# equal to dpois(0) + dpois(1)
ppois(q=1, lambda=2)

# the q function is the inverse of p
# What is the number of hits corresponding to 50% of the probability mass
qpois(p=0.5, lambda=2.5)
ppois(q=2,lambda=2.5)

ranPois <- rpois(n=1000,lambda=2.5)
my_histo(ranPois)

quantile(x=ranPois,probs=c(0.025,0.975))

#---------------------------------
# Binomial distribution
# p = probability of a dichotomous outcome
# size = number of trials
# x = possible outcomes
# outcome x is bounded between 0 and number of trials

# use "d" binom for density function
hits <- 0:10
myVec <- dbinom(x=hits,size=10,prob=0.5)
plot(myVec,type="b")

myCoins <- rbinom(n=5000,size=100,prob=0.5)
plot(myCoins,type="b")
my_histo(myCoins)

quantile(x=myCoins,probs=c(0.025,0.975))

#-----------------------------------------
# negative binomial: number of failures (values of MyVec)
# in a series of (Bernoulli) with p=probability of success
# before a target number of successes (= size)
# generates a discrete distribution that is more
# heterogeneous ("over dispersed") than Poisson

hits <- 0:40
myVec <- dnbinom(x=hits, size=5, prob=0.5)
plot(myVec,type="b")

# geometric series is a special case where N = 1 success
# each bar is a constant fraction 1 - "prob" of the bar before it

myVec <- dnbinom(x=hits, size=1, prob=0.1)
plot(myVec, type="b")





