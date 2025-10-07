#linear regression example from class 10/7
#iris example

rm(list = ls())

#subset the virginica species
#create a new data frame, create index using species == virginica
#comma indexes all rows
flower <- iris[iris$Species == "virginica",]
#unique fn tells all the unique values


#make a scatter plot to look at sepal length vs petal length
plot(flower$Sepal.Length, flower$Petal.Length, 
     pch = 19, xlab = "Sepal length", ylab = "Petal length",
     main = "Iris virginica")
#abline adds straight lines to a plot

#fit a regression model
fit <- lm(flower$Petal.Length ~ flower$Sepal.Length)
#tilde does...?

#?fn_name calls help 

#plot residuals 
#summary of the fit $ residuals gives the residuals for the model
plot(flower$Sepal.Length, summary(fit)$residuals, pch = 19,
     xlab = "Sepal Length", ylab = "Residuals")
#add horizontal line at 0
abline(h=0)

#check normality of residuals
hist(summary(fit)$residuals, col = "red", main = "residual distribution", xlab = "residuals")
#looks fairly normally, not massively skewed in any direction

#qqnorm or qq line can provide another visual check
qqnorm(summary(fit)$residuals, pch = 19)
qqline(summary(fit)$residuals, pch = 19)

#shapiro wilks test to check normality
shapiro.test(summary(fit)$residuals)
#tells us if distribution of data differs significantly from standard distribution
#p-value is way above .05 so it does not significantly differ from normal distribution

#all assumptions are met so results can be interpreted
