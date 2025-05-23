# ggplot graph types from R Graph Gallery
# 8 April 2025

library(ggplot2)
library(ggridges) # ridge plots
library(ggbeeswarm) # beeswarm plots
library(GGally) # parallel coordinates plots
library(ggpie) # pie charts
library(ggmosaic) # mosaic plots
library(scatterpie) # scatter pies on map
library(waffle) # for waffle plots
library(DescTools) # for converting table to long
library(treemap) # for tree maps


d <- mpg

# use to plot the counts of rows for a categorical variable
table(d$drv)

# now overlay the raw data
p1 <- ggplot(d) + 
  aes(x=fl,y=hwy) +
  geom_boxplot(fill="thistle",outlier.shape=NA) + 
  # geom_point()
  geom_point(position=position_jitter(width=0.1, height=0.7),
             color="grey60",size=2)

print(p1)

# violin plot
# symmetric density plot
p1 <- ggplot(data=d) +
  aes(x=drv, y=cty, fill=drv) + 
  geom_violin() +
  geom_point(position=position_jitter(width=0.2,
                                      height=0.3),color="black",size=0.4)
p1

# ridgeline plot
p2 <- ggplot(data=d) +
  aes(x=cty,y=drv,fill=drv) + 
  ggridges::geom_density_ridges() +
  ggridges::theme_ridges() 
p2  

# beeswarm plot
p3 <- ggplot(data=d) +
  aes(x=drv,y=cty,color=drv) + 
  ggbeeswarm::geom_beeswarm(method = "center",size=2) 
p3

# bubble plot
p4 <- ggplot(data=d) +
  aes(x=displ,y=hwy,size=cty,fill=drv) +
  geom_point(shape=21,color="black",stroke=0.5)
p4

# parallel coordinates plot
p5 <- GGally::ggparcoord(data=d,
                         columns = c(3,9), # c(3,5,8,9) 
                         groupColumn = 7) 
p5

fuel_data <- data.frame(
  table(d$fl),
  fuel=c("Natural Gas",
         "Diesel",
         "Ethanol",
         "Premium",
         "Regular"))
fuel_data <- fuel_data[,-1] # remove tabled column
fuel_data$Freq <- fuel_data$Freq+100

p6 <- ggplot(data=fuel_data) +
  aes(x=fuel, y=Freq) +
  geom_segment(aes(x=fuel, 
                   xend=fuel, 
                   y=0, 
                   yend=Freq), 
               linewidth=2) +
  geom_point( color="orange", size=7) +
  labs(title="Fuel Type",
       x="",
       y="Count") +
  coord_flip() +
  theme_light(base_size=20,base_family=
                  "Monaco"))  
p6

# typical unsatisfactory pie chart

p7 <- ggpie::ggpie(data=mpg,
                   group_key="class",
                   count_type="full",
                   label_info="ratio", 
                   label_type="none") # try circle
p7

# much better waffle plot
tabled_data <- as.data.frame(table(class=mpg$class))

p8 <- ggplot(data=tabled_data) +
  aes(fill = class, values = Freq) +
  waffle::geom_waffle(n_rows = 8, size = 0.33, colour = "white") + 
  coord_equal() +
  theme_void()
p8

# highly effective scatterpie diagrams
d <- data.frame(x=rnorm(5), y=rnorm(5))
d$A <- abs(rnorm(5, sd=1))
d$B <- abs(rnorm(5, sd=2))
d$C <- abs(rnorm(5, sd=3))

p9 <- ggplot(data=d) + 
  scatterpie::geom_scatterpie(
    aes(x=x, y=y), 
    pie_scale=1:5,
    cols=c("A", "B","C")) + 
  coord_fixed()
# scale_fill_manual(values=c("coral","grey95","grey90"))
p9

# mosaic plots for proportional data (1 or 2 factors). All combinations of factors must be present!
# no zero values

# Simple 1-factor partition
# options hspine (default),vspine,vbar,hbar
city_tree <- expand.grid(Tree=c("Oak","Pine","Maple","Spruce","Beech"),City=c("Bur","Col","Win"))

city_tree$Freq <- c(100,2,25,
                    9,4,7,
                    3,30,30,
                    2,2,5,
                    6,6,6)
city_tree_long <- DescTools::Untable(city_tree)
d <- city_tree_long

p10 <- ggplot(data = d) +
  geom_mosaic(aes(x = product(Tree), 
                  fill=Tree),
              divider="hspine") + 
  labs(title='Tree Type')
p10

# 2-factor partition
# divider options mosaic("h"),mosaic("v"),ddecker()
d <- city_tree_long

p11 <- ggplot(data = d) +
  geom_mosaic(aes(x = product(Tree,City), 
                  fill=Tree),
              divider=mosaic("v")) +
  labs(title='mosaic v')
p11

# 2 factor with conditioning
p13 <- ggplot(data = d) +
  geom_mosaic(aes(x=product(Tree), fill = Tree, 
                  conds = product(City))) +
  labs(title='f(Tree | City)')
p13

# alternative to conditioning: faceting
p13 <- ggplot(data = d) +
  geom_mosaic(aes(x = product(Tree), fill=Tree), divider = "vspine") +
  labs(title='f(Tree | City)') + 
  facet_grid(~City) +
  theme(aspect.ratio = 3,
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())
p13

