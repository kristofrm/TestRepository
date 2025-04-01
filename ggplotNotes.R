# ggplot graphics 1
# KRM
# 27 March 2025
#

library(ggplot2)
library(ggthemes)
library(patchwork)

# p1 <- ggplot(data= <DATA>) +
#   aes(<MAPPINGS>) +
#   <GEOM_FUNCTION>(aes(<MAPPINGS>),
#                   stat=<STAT>,
#                   position=<POSITION>)+
#   <COORDINATE_FUNCTION> + 
#   <FACET_FUNCTIONS>

# print(p1)

ggsave(plot=p1,
       filename='MyPlot',
       width=5,
       height=3,
       units='in',
       device='pdf')

d <- mpg # use build in mpg data frame
str(d)
table(d$fl)

# basic histogram plot
ggplot(data=d) +
  aes(x=hwy) + 
  geom_histogram()

ggplot(data=d) +
  aes(x=hwy) + 
  geom_histogram(fill='khaki', color='black')

# basic density plot
ggplot(data=d) + 
  aes(x=hwy) +
  geom_density(fill='mintcream', color='blue')

# basic scatter plot
ggplot(data=d) +
  aes(x=displ, y=hwy) +
  geom_point() +
  geom_smooth()

# bar plot with specified counts or means
x_treatment <- c("Control", "Low", "High")
y_response <- c(12,2.5,22.9)
summary_data <- data.frame(x_treatment, y_response)

ggplot(data = summary_data) +
  aes(x=x_treatment, y=y_response) +
  geom_col(fill=c('grey50', 'goldenrod', 'goldenrod'),col='black')

# plot user-defined functions
my_fun <- function(x) sin(x) + 0.1*x
d_frame <- data.frame(x=my_vec, y=my_fun(my_vec))
ggplot(data=d_frame) +
  aes(x=x, y=y) + 
  geom_line()

p1 <- ggplot(data=d) +
  (mapping=aes(x=displ,y=cty)) +
  geom_point()
print(p1)

p1 + theme_classic() # no grid lines
p1 + theme_linedraw() # black frame
p1 + theme_dark() # good for brightly colored points
p1 + theme_base() # mimics base R
p1 + theme_par() # Matches current par settings in base
p1 + theme_void() # data only
p1 + theme_solarized() # good for web pages
p1 + theme_economist()

