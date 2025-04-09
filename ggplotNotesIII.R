# ggplot notes III
# 4/3/25
# KRM

devtools::install_github("wilkelab/cowplot")
install.packages("colorspace", repos = "http://R-Forge.R-project.org")
devtools::install_github("clauswilke/colorblindr")

library(ggplot2)
library(ggthemes)
library(colorblindr)
library(colorspace)
library(wesanderson)
library(ggsci)

d <- mpg

# use to plot the counts of rows for a categorical variable
table(d$drv)

p1 <- ggplot(d) +
  aes(x=drv) + 
  geom_bar(color="black",fill="cornsilk")
print(p1)

# aesthetic mapping gives multiple groups for each bar
p1 <- ggplot(d) + 
  aes(x=drv,fill=fl) + 
  geom_bar()
print(p1)


