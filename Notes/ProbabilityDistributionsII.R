############################
# Function: my_histo
# Purpose: creates a ggplot histogram
# Requires: ggplot
# Input: x = a numeric vector
#        data_type= "cont" or "disc"
# Output: a ggplot histogram
############################
library(ggplot2)
my_histo <- function(x=NULL,data_type="cont") {
  if(is.null(x)) x=runif(1000)
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
  
  
  # if data are continuous, but not bounded (0,1), use
  # ggplot default bins
  if (data_type=="cont" & (min(df$x) < 0 | max(df$x) > 1)) {
    p1 <- ggplot(data=df) +
      aes(x=x) +
      geom_histogram(color="black",
                     fill="goldenrod")}
  
  
  
  
  # if data are discrete integers, 
  #  use geom_bar to create a histogram
  if (data_type=="disc") {
    p1 <- ggplot(data=df) + 
      aes(x=x) +
      geom_bar(color="black",fill="goldenrod") }
  
  print(p1)
} 
my_histo()
my_histo(data_type="disc",x=rpois(1000,lambda=0.2))
my_histo(data_type="cont",x=runif(1000))
my_histo(data_type="cont",x=rnorm(n=1000,mean=0,sd=1))




############################
# Function: my_pdf
# Purpose: creates a ggplot probability density function
# Requires: ggplot
# Input: x = a numeric vector of x values
#        y = pdf values calculated for each value of x
#        data_type= "cont" or "disc"
# Output: a ggplot pdf
############################
my_pdf <- function(x=NULL,y=NULL,data_type="cont") {
  if(is.null(x) | is.null(y)) {
    x=seq(from=-3,to=3,length.out=100)
    y=dnorm(x) }
  
  df <- data.frame(x=x,y=y) 
  
  # for continuous distributions, 
  # plot the line for the pdf
  if(data_type=="cont") {
    p1 <- ggplot(data=df) +
      aes(x=x,y=y) +
      geom_line() +
      geom_area(fill = "cornflower blue") } 
  
  # for discrete distributions,
  # plot a bar for the probability at each value
  if (data_type=="disc") {
    p1 <- ggplot(data=df) + 
      aes(x=x,y=y) +
      geom_col(color="black",fill="cornflower blue") }
  print(p1)
}
my_pdf()
my_x=seq(from=0,to=1,length.out=100)
my_pdf(x=my_x,y=dbeta(x=my_x,shape1=15,shape2=10))
my_pdf(x=0:10,y=dpois(x=0:10,lambda=1.1),data_type="disc")

#-------------------------------------------------
# negative binomial: number of failures (values of MyVec)
# in a series of (Bernouli) with p=probability of success 
# before a target number of successes (= size)
# generates a discrete distribution that is more 
# heterogeneous ("overdispersed") than Poisson
hits <- 0:40
my_vec <- dnbinom(x=hits, size=5, prob=0.5)
my_pdf(x=hits,y=my_vec,data_type="disc")

# geometric series is a special case where N= 1 success
# each bar is a constant fraction 1 - "prob" of the bar before it
my_vec <- dnbinom(x=hits, size=1, prob=0.1)
my_pdf(x=hits,y=my_vec,data_type="disc")


# alternatively specify mean = mu of distribution and size, 
# the dispersion parameter (small is more dispersed)
# this gives us a poisson with a lambda value that varies
# the dispersion parameter is the shape parameter in the gamma
# as it increases, the distribution has a smaller variance
# just simulate it directly

nbi_ran <- rnbinom(n=1000,size=10,mu=5)
my_histo(x=nbi_ran,data_type="disc")


nbi_ran <- rnbinom(n=1000,size=0.1,mu=5)
my_histo(x=nbi_ran,data_type="disc")

#-------------------------------------------------
# uniform
# params specify minimum and maximum

#runif for random data
my_histo(x=runif(n=100),data_type="cont")
my_histo(x=runif(n=1000),data_type="cont")

#-------------------------------------------------

# normal 
my_norm <- rnorm(n=100,mean=100,sd=2)
my_histo(x=my_norm,data_type="cont")


# problems with normal when mean is small but zero is not allowed.
my_norm <- rnorm(n=100,mean=2,sd=2)
my_histo(x=my_norm,data_type="cont")

summary(my_norm)
toss_zeroes <- my_norm[my_norm>0]
my_histo(x=toss_zeroes,data_type="cont")

summary(toss_zeroes)


#-------------------------------------------------
# gamma distribution, continuous positive values, but bounded at 0

my_gamma <- rgamma(n=100,shape=1,scale=10)
my_histo(x=my_gamma,data_type="cont")

# gamma with shape= 1 is an exponential with scale = mean

# shape <=1 gives a mode near zero; very small shape rounds to zero
my_gamma <- rgamma(n=100,shape=0.1,scale=1)
my_histo(x=my_gamma,data_type="cont")

# large shape parameters moves towards a normal
my_gamma <- rgamma(n=100,shape=20,scale=1)
my_histo(x=my_gamma,data_type="cont")

# scale parameter changes mean- and the variance!
my_histo(x=rgamma(n=100,shape=2,scale=100),data_type="cont")
my_histo(x=rgamma(n=100,shape=2,scale=10),data_type="cont")
my_histo(x=rgamma(n=100,shape=2,scale=1),data_type="cont")
my_histo(x=rgamma(n=100,shape=2,scale=0.1),data_type="cont")



# unlike the normal, the two parameters affect both mean and variance

# mean = shape*scale
# variance= shape*scale^2


#-------------------------------------------------

# beta distribution 
# bounded at 0 and 1
# analagous to a binomial, but result is a continuous distribution of probabilities
# parameter shape1 = number of successes + 1
# parameter shape2 = number of failures + 1
# interpret these in terms of a coin you are tossing

# shape1 = 1, shape2 = 1 = "no data"
my_beta <- rbeta(n=1000,shape1=1,shape2=1)
my_histo(x=my_beta,data_type="cont")

# shape1 = 2, shape1 = 1 = "1 coin toss, comes up heads!"
my_beta <- rbeta(n=1000,shape1=2,shape2=1)
my_histo(x=my_beta,data_type="cont")
# two tosses, 1 head and 1 tail
my_beta <- rbeta(n=1000,shape1=2,shape2=2)
my_histo(x=my_beta,data_type="cont")
# two tosses, both heads
my_beta <- rbeta(n=1000,shape1=2,shape2=1)
my_histo(x=my_beta,data_type="cont")
# let's get more data
my_beta <- rbeta(n=1000,shape1=20,shape2=20)
my_histo(x=my_beta,data_type="cont")
my_beta <- rbeta(n=1000,shape1=500,shape2=500)
my_histo(x=my_beta,data_type="cont")

# if the coin is biased
my_beta <- rbeta(n=1000,shape1=1000,shape2=500)
my_histo(x=my_beta,data_type="cont")
my_beta <- rbeta(n=1000,shape1=10,shape2=5)
my_histo(x=my_beta,data_type="cont")


# shape parameters less than 1.0 give us a u-shaped distribution
my_beta <- rbeta(n=1000,shape1=0.1,shape2=0.1)
my_histo(x=my_beta,data_type="cont")

my_beta <- rbeta(n=1000,shape1=0.5,shape2=0.2)
my_histo(x=my_beta,data_type="cont")



library(MASS)
data <- c(100,100,104,99)
z <- fitdistr(data,"normal")
print(z)

frog_data <- c(30.15,26.3,27.5,22.9,27.8,26.2)

# get and print model parameters assuming a normal distribution
z<- fitdistr(frog_data,"normal")
print(z)

# plot the density function for the normal and annotate the plot with the original data
x <- 1:100
g_density <- dnorm(x=x,mean=z$estimate["mean"],sd=z$estimate["sd"])
qplot(x,g_density,geom="line") + annotate(geom="point",x=frog_data,y=0.001)   

# now do the same for a gamma distribution, which has a shape and rate parameter as outputs from fitdistr
z<- fitdistr(frog_data,"gamma")
print(z)

# plot the density function for the gamma and annotate the plot with the original data
x <- 1:100
g_density <- dgamma(x=x,shape=z$estimate["shape"],rate=z$estimate["rate"])
qplot(x,g_density,geom="line") + annotate(geom="point",x=frog_data,y=0.001,color="red") 

#----------------------------------------
frog_data <- c(30.15,26.3,27.5,22.9,27.8,26.2)
outlier <- 0.050
frog_data <- c(frog_data,outlier)

# get and print model parameters assuming a normal distribution
z<- fitdistr(frog_data,"normal")
print(z)

# plot the density function for the normal and annotate the plot with the original data
x <- 1:100
g_density <- dnorm(x=x,mean=z$estimate["mean"],sd=z$estimate["sd"])
qplot(x,g_density,geom="line") + annotate(geom="point",x=frog_data,y=0.001)   

# now do the same for a gamma distribution, which has a shape and rate parameter as outputs from fitdistr
z<- fitdistr(frog_data,"gamma")
print(z)

# plot the density function for the gamma and annotate the plot with the original data
x <- 1:100
g_density <- dgamma(x=x,shape=z$estimate["shape"],rate=z$estimate["rate"])
qplot(x,g_density,geom="line") + annotate(geom="point",x=frog_data,y=0.001,color="red")   

