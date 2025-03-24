# dplyr Lecture
# 2/26

# dplyr for data manipulation and structuring dataframes

# core verbs are:

# filter(), arrange(), select(), summarize() and group_by(), mutate()

library(tidyverse)

# specify package name before function call
dplyr::filter()

data(starwars)
class(starwars)

# this dataset is a tibble: tibbles do "less" as a tradeoff to make code simpler and less prone to crashing

head(starwars)
print(starwars$gender)

# cleaning up our dataset

# base R has the complete.cases function - this will remove rows with NAs

starwarsClean <- starwars[complete.cases(starwars[,1:10]), ]

# you can check for NAs
is.na(starwarsClean[,1])

anyNA(starwarsClean)

# filter() picks/subsets observations by their values
# uses > , >=, <, <=, == , !
# filter automatically exlcudes NA, have to ask for them specifically

filter(starwarsClean, gender == "masculine" && height < 180)

filter(starwars, gender == "masculine", height < 180, height > 100)

# filter eye_color for blue or brown eyes
filter(starwars, eye_color %in% c("blue", "brown")) # similar to ==

# arrange() reorder rows

arrange(starwarsClean, by= height) # defaults to ascending order

arrange(starwarsClean, by=desc(height)) # descending order

arrange(starwarsClean, height, desc(mass)) # any additional columns will break ties in preceding column

starwars1 <- arrange(starwars, height)
tail(starwars1)

# select() choose variables by their names
# we can do in base R
starwarsClean[1:10, ]

select(starwarsClean, 1:10)
select(starwarsClean, name:homeworld)
select(starwarsClean, -(films::starships)) # you can subset everything except particular variables

# rearrange columns
select(starwarsClean, homeworld, name, gender, species, everything())

select(starwarsClean, contains("color")) # other helpers include: ends_with, starts_with, matches (reg ex), num_range

# rename columns
select(starwars, haircolor=hair_color) # new name on the left

# mutate create new variables with functions of existing variables

# create a column that's height divided by mass
mutate(starwarsClean, ratio=height/mass)

starwars_lbs <- mutate(starwarsClean, mass_lbs=mass*2.2)

select(starwars_lbs, 1:3, mass_lbs, everything()) # using select allows us to bring it to the beginning

# if you only wanted the new variable, you can use the transmute function

transmute(starwarsClean, mass_lbs = mass*2.2)

transmute(starwarsClean, mass, mass_lbs=mass*2.2)

# summarize() and group_by() collapse many values down to a single summary
summarize(starwarsClean, meanHeight=mean(height)) # gives the summary stat for the entire tibble

summarize(starwars, meanHeight = mean(height)) # need dataset to be clean

summarize(starwars, meanHeight = mean(height, na.rm=TRUE), TotalNumber=n()) # work around

# use group_by() for additional calculations
starwarsGender <- group_by(starwars, gender)

head(starwarsGender)

summarize(starwarsGender, meanHeight = mean(height, na.rm=TRUE), number=n())

# pipe statements, or piping %>%

# these are sequences of actions that will change your dataset
# it will let you pass an intermediate result onto the next function in sequence
# you should avoid this when you need to manipulate more than one object at a time, or if there are meaningful intermediate objects
# formatting: should always have a space before it and usually followed by a new line (usually automatic indent)

starwarsClean%>% # tells you that you're using starwarsClean as the data set
  group_by(gender)%>%
  summarize(meanHeight=mean(height, na.rm=T), number=n())


  
  
  
