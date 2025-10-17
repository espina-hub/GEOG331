#use built in iris dataset
#take a look at it 
head(iris)
#load in some tidyverse packages
library(tidyverse)

#####################################
##### Part 1: for loops         #####
#####################################

#Using only data for iris versicolor
#write a for loop
#that produces a regression table
#for each of the following relationships
#1. iris  sepal length x width
#2. iris  petal length x width
#3. iris sepal length x petal length

# hint: consider using a list, and also new vectors for regression variables

versicolor <- subset(iris, Species == "versicolor")
variables <- list(versicolor$Sepal.Length ~ versicolor$Sepal.Width, 
               versicolor$Petal.Length ~ versicolor$Petal.Width, 
               versicolor$Sepal.Length ~ versicolor$Petal.Length)
models <- list()

for (item in variables) {
  model <- lm(item, data = versicolor)
  models[[deparse(item)]] <- summary(model)$coefficients 
}
#deparse adds the names of each model so i can differentiate
#converts formula to text 
models

#####################################
##### Part 2: data in dplyr     #####
#####################################
install.packages("dplyr")
library(dplyr)
#use dplyr to join data of maximum height
#to a new iris data frame
height <- data.frame(Species = c("virginica","setosa","versicolor"),
                     Height.cm = c(60,100,11.8))

iris_with_height <- left_join(iris, height, by = "Species")

#####################################
##### Part 3: plots in ggplot2  #####
#####################################
library(ggplot2)

#look at base R scatter plot
plot(iris$Sepal.Length,iris$Sepal.Width)

#3a. now make the same plot in ggplot
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) + geom_point()
#geom_point makes it a scatter plot

#3b. make a scatter plot with ggplot and get rid of  busy grid lines
ggplot(iris, aes(Sepal.Length, Sepal.Width)) + geom_point() + theme_classic()
#adding theme_classic gets rid of the ugly stuff (grid lines, color)

#3c. make a scatter plot with ggplot, remove grid lines, add a title and axis labels, 
#    show species by color, and make the point size proportional to petal length
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, color = Species, size = Petal.Length)) + geom_point() + theme_classic() + 
  labs(title = "Iris Sepal Dimensions by Species", x = "Sepal Length (cm)", y = "Sepal Width (cm)", size = "Petal Length (cm)")

#####################################
##### Question: how did         #####
##### arguments differ between  #####
##### plot and ggplot?          #####
#####################################		
# the arguments differ between regular and ggplot in that in base R, all the
#information regarding the plot goes into the plot function. we specify the 
#color, size, etc. within the function. in ggplot, it is separated. the aes() 
#function controls what is plotted (the contents -- x/y/color) but everything
#else goes in a separate piece, joined by "+". ggplot uses layers to add to
#a plot which is done separately from the initial aes fn. 