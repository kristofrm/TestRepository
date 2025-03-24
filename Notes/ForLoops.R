# ForLoops notes

# Working with only odd numbered elements
z <- 1:20
for (i in 1:length(z)) {
  if (i %% 2 !=0) next
  print(i)
}

# Another method, probably faster (why?)
z <- 1:20
zsub <- z[z %% 2 != 0]
length(zsub)

for (i in seq_along(zsub)) {
  cat("i = ", i, "zsub[i] = ", zsub[i], "\n")
}

###########################################################
# FUNCTION: ran_walk
# stochastic random walk
# input: times = number of time steps
#        n1 = initial population size (= n[1])
#        lambda = finite rate of increase
#        noise_sd = sd of a normal distribution with mean 0
# output: vector n with population sizes > 0
#         until extinction, then NA
#----------------------------------------------------------

library(ggplot2)
ran_walk <- function(times=100,n1=50,lambda=1.00,noise_sd=10) {
  n <- rep(NA,times) # create output vector
  n[1] <- n1 # initialize with starting population size
  noise <- rnorm(n=times,mean=0,sd=noise_sd) # create noise vector
  
  for (i in 1:(times-1)) {
    n[i+1] <- lambda*n[i] + noise[i]
    if(n[i+1] <= 0) {
      n[i+1] <- NA
      cat("Population extinction at time",i+1,"\n")
      break}
  }
  return(n)
}

ran_walk()
qplot(x=1:100, y=ran_walk(),geom='line')

m <- matrix(round(runif(20),digits=2),nrow=5)
# loop over rows
for (i in 1:nrow(m)) { # could use for (i in seq_len(nrow(m)))
  m[i,] <- m[i,] + i
}
print(m)

# loop over columns
for (j in 1:ncol(m)) {
   m[,j] <- m[,j] + j
}
m

# loop over rows and columns
for (i in 1:nrow(m)) {
  for (j in 1:ncol(m)) {
    m[i,j] <- m[i,j] + i + j
  }
}
m

species_area_curve <- function(A=1:5000,c=0.5,z=0.26) {
  S <- c*(A^z)
  return(S)
}

head(species_area_curve())

species_area_plot <- function(A=1:5000,c=0.5,z=0.26) {
  plot(x=A,y=species_area_curve(A,c,z),type='l',xlab='Island Area',ylab='S',ylim=c(0,2500))
  mtext(paste('c = ', c,' z =',z), cex=0.7)
}

species_area_plot()

# global variables
c_pars <- c(100,150,175)
z_pars <- c(0.10, 0.16, 0.26, 0.3)
par(mfrow=c(3,4))
for (i in seq_along(c_pars)) {
  for (j in seq_along(z_pars)) {
    species_area_plot(c=c_pars[i],z=z_pars[j])
  }
}

# looping with for
cut_point <- 0.1
z <- NA
ran_data <- runif(100)
for (i in seq_along(ran_data)) {
  z <- ran_data[i]
  if (z < cut_point) break
}
print(z)
print(i)

# looping with while

z <- NA
cycle_number <- 0
while (is.na(z) | z >= cut_point) {
  z <- runif(1)
  cycle_number <- cycle_number + 1
  
}
print(z)
print(cycle_number)

##################################################
# function: sa_output
# Summary stats for species-area power function
# input: vector of predicted species richness 
# output: list of max-min, coefficient of variation 
#------------------------------------------------- 
sa_output <- function(S=runif(1:10)) {
  
  sum_stats <- list(s_gain=max(S)-min(S),s_cv=sd(S)/mean(S))
  
  return(sum_stats)
}
sa_output()

# Build program body with a single loop through 
# the parameters in modelFrame

# Global variables
area <- 1:5000
c_pars <- c(100,150,175)
z_pars <- c(0.10, 0.16, 0.26, 0.3)

# set up model frame
model_frame <- expand.grid(c=c_pars,z=z_pars)
model_frame$s_gain <- NA
model_frame$s_cv <- NA
print(model_frame)


# cycle through model calculations
for (i in 1:nrow(model_frame)) {
  
  # generate S vector
  temp1 <- species_area_curve(A=area,
                              c=model_frame[i,1],
                              z=model_frame[i,2])
  # calculate output stats
  temp2 <- sa_output(temp1)
  # pass results to columns in data frame
  model_frame[i,c(3,4)] <- temp2
  
}
print(model_frame)

library(ggplot2)

area <- 1:5 #keep this very small initially
c_pars <- c(100,150,175)
z_pars <- c(0.10, 0.16, 0.26, 0.3)

# set up model frame
model_frame <- expand.grid(c=c_pars,z=z_pars,A=area)
model_frame$S <- NA

# loop through the parameters and fill with SA function

for (i in 1:length(c_pars)) {
  for (j in 1:length(z_pars)) {
    model_frame[model_frame$c==c_pars[i] & model_frame$z==z_pars[j],"S"] <-   species_area_curve(A=area,c=c_pars[i],z=z_pars[j])
  }
}

for (i in 1:nrow(model_frame)) {
  model_frame[i,"S"] <- species_area_curve(A=model_frame$A[i],
                                           c=model_frame$c[i],
                                           z=model_frame$z[i])
}

head(model_frame)

library(ggplot2)

p1 <- ggplot(data=model_frame)
p1 + geom_line(mapping= aes(x=A,y=S)) + facet_grid(c~z)

p2 <- p1
p2 + geom_line(mapping=aes(x=A,y=S,group=z)) +
  facet_grid(.~c)





