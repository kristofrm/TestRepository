# Strategic Coding Practices notes

# load libraries ----
library(pracma)
library(pryr)
library(devtools)

## subheader A ----

## subheader B ----

# demonstrating snippets
# KRM
# 20 March 2025
#
library(package)

library(upscaler)
add_folder()
add_folder(c("specialfolder1","specialfolder2"))
metadata_template(file="OriginalData/MyData.csv")

x <- runif(10) # an object to save
saveRDS(object=x,
        file="DataObjects/x.rds") # save to disk
restored_x <- readRDS(file="DataObjects/x.rds") # reopen to new name

y <- rnorm
z <- pi
bundle <- list(x=x,y=y,z=z) # save multiple objects in a single list
saveRDS(object=bundle,
        file="DataObjects/bundle.rds")
restored_bundle <- readRDS(file="DataObjects/bundle.rds")
restored_bundle[[3]] # reference content of item number in a list

set_up_log()
echo_log_console(TRUE)
l() # plain log entry
l('log message that is echoed to the screen')
echo_log_console(FALSE)
l('this message only shows in the log file')
l()
# load libraries
l('load libraries')
set_up_log(overwrite_log=FALSE)

for (k in 1:100) {
  show_progress_bar(k)
  Sys.sleep(0.075)
}
l('end of loop')

for (k in 1:100) {
  show_progress_bar(k)
  Sys.sleep(0.075)
  if (k==52)print(ghost) # this throws an error!
}

l('end of loop with error')

# Adjust parameters of progress bar for longer loops
for (k in 1:1000) {
  show_progress_bar(index=k, counter=50,dot=5)
  Sys.sleep(0.0075)
}
l('end of long loop')

# Add a timer for long loops (from pracma package)
library(pracma)
tic()
for (k in 1:10) {
  show_progress_bar(k)
  Sys.sleep(1)
}
toc()
l('end of timed loop')

library(pryr)
set_up_log(overwrite=FALSE)
for (i in 1:100) {
  #show_progress_bar()
  l(paste('memory_used=',trunc(mem_used()/10^6),
          " MB;"," i=",i,sep=''))
  z <- runif(n=10^i)
}

build_function("fit_regression") # creates an R script template for the function
source("Functions/FitRegression.R")




