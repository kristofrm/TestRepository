# ggplot II
# 1 April
# KRM

library(ggplot2)
library(ggthemes)
library(patchwork)

g1 <- ggplot(data=d) +
  aes(x=displ,y=cty) + 
  geom_point() + 
  geom_smooth()

g2 <- ggplot(data=d) +
  aes(x=fl) +
  geom_bar(fill="tomato",color="black")+ 
  theme(legend.position="none")

g3 <- ggplot(data=d) +
  aes(x=displ) + 
  geom_histogram(fill="royalblue",
                 color="black")

g4 <- ggplot(data=d) +
  aes(x=fl,y=cty,fill=fl)+
  geom_boxplot()+
  theme(legend.position='none')
g4

# Add tags to plots
g1 / (g2 | g3) +
  plot_annotation(tag_levels = 'A')

# Swapping axis orientation for multipanel plot
g3a <- g3 + scale_x_reverse()
g3b <- g3 + scale_y_reverse() 
g3c <- g3 + scale_x_reverse() + scale_y_reverse()

(g3 | g3a)/(g3b | g3c)

m1 <- ggplot(data=mpg) +
  aes(x=displ, y=cty) +
  geom_point()
m1 + facet_grid(class~fl)





