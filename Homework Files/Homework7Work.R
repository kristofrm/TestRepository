# Homework 7
# 2/26
# KRM

# Q1: Examine the structure of the iris data set. How many observations and variables are in the data set?

data(iris)

# There are 150 observations and 5 variables

# Q2: Create a new data frame iris1 that contains only the species virginica and versicolor with sepal lengths longer than 6 cm and sepal widths longer than 2.5 cm. How many observations and variables are in the data set?

iris1 <- filter(iris, Species %in% c("virginica", "versicolor"), Sepal.Length > 6, Sepal.Width > 2.5)

# There are 56 observations and 5 variables in iris1

# Q3: Now, create a iris2 data frame from iris1 that contains only the columns for Species, Sepal.Length, and Sepal.Width. How many observations and variables are in the data set?

iris2 <- select(iris1, Species, Sepal.Length, Sepal.Width)

# 56 observations and 3 variables

# Q4: Create an iris3 data frame from iris2 that orders the observations from largest to smallest sepal length. Show the first 6 rows of this data set.

iris3 <- arrange(iris2, by=desc(Sepal.Length))
head(iris3)

# Q5: Create an iris4 data frame from iris3 that creates a column with a sepal area (length * width) value for each observation. How many observations and variables are in the data set?

iris4 <- mutate(iris3, Sepal.Area = Sepal.Length*Sepal.Width)

# 56 obs and 4 variables

# Q6: Create iris5 that calculates the average sepal length, the average sepal width, and the sample size of the entire iris4 data frame and print iris5.

iris5 <- summarize(iris4, avg_sepal_length = mean(Sepal.Length), avg_sepal_width = mean(Sepal.Width), number = n())
print(iris5)

# Q7: Finally, create iris6 that calculates the average sepal length, the average sepal width, and the sample size for each species of in the iris4 data frame and print iris6.

iris_grouped <- group_by(iris4, Species)
iris6 <- summarize(iris_grouped, avg_sepal_length = mean(Sepal.Length), avg_sepal_width = mean(Sepal.Width), number = n())
print(iris6)

# Q8: See if you can rework all of your previous statements (except for iris5) into an extended piping operation that uses iris as the input and generates irisFinal as the output.

irisFinal <- iris%>%
  filter(Species %in% c("virginica", "versicolor"), Sepal.Length > 6, Sepal.Width > 2.5)%>%
  select(Species, Sepal.Length, Sepal.Width) %>%
  arrange(by=desc(Sepal.Length)) %>%
  mutate(Sepal.Area = Sepal.Length*Sepal.Width) %>%
  group_by(Species) %>%
  summarize(avg_sepal_length = mean(Sepal.Length), avg_sepal_width = mean(Sepal.Width), number = n())

# Q9: Create a ‘longer’ data frame using the original iris data set with three columns named “Species”, “Measure”, “Value”. The column “Species” will retain the species names of the data set. The column “Measure” will include whether the value corresponds to Sepal.Length, Sepal.Width, Petal.Length, or Petal.Width and the column “Value” will include the numerical values of those measurements.

iris_longer <- iris%>%
  pivot_longer(cols=-Species,
               names_to = "Measure",
               values_to= "Value",
               values_drop_na = T)


  
  