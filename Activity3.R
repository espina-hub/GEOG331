#activity3
print("Hello World")
#create a function. The names of the arguments for your function will be in parentheses. Everything in curly brackets will be run each time the function is run.

#helpful for testing your code for errors/mistaken assumptions
assert <- function(statement,err.message){
  #if evaluates if a statement is true or false for a single item
  if(statement == FALSE){
    print(err.message)
  }
  
}
#check how the statement works
#evaluate a false statement
assert(1 == 2, "error: unequal values")
#evaluate a true statement
assert(2 == 2, "error: unequal values")
#set up assert to check if two vectors are the same length
a <- c(1,2,3,4)
b <- c(8,4,5)
assert(length(a) == length(b), "error: unequal length")
#read in weather data, skip first 3 rows, specify NA
weather_data <- read.csv("Z:\\espina\\Data for Class\\bewkes\\bewkes_weather.csv", na.strings=c("#N/A"), skip=3, header=FALSE)
#print(weather_data[1,])
sensor_info <- read.csv("Z:\\espina\\Data for Class\\bewkes\\bewkes_weather.csv", na.strings=c("#N/A"), nrows=2)
#print(sensor_info)
#get column names from sensor_info table
# and set weather station colnames  to be the same
colnames(weather_data) <-   colnames(sensor_info)
#preview data
#print(weather_data([1,])
#use install.packages to install lubridate
#install.packages(c("lubridate"))
#it is helpful to comment this line after you run this line of code on the computer
#and the package installs. You really don't want to do this over and over again. 
#library function loads package to working environment
library(lubridate)
#returns message says objects are masked from base package:base
#objects exist in both packages, lubridate is overriding the base R functions
#use lubridate to correct data format, standardize
dates <- mdy_hm(weather_data$timestamp, tz= "America/New_York")
#calculate day of year
weather_data$doy <-yday(dates)
weather_data$hour <- hour(dates) + (minute(dates)/60)
weather_data$DD <- weather_data$doy + (weather_data$hour/24)
weather_data[1,]

#check missing data
#see how many values have missing data for each sensor observation
#air temperature
length(which(is.na(weather_data$air.temperature)))
#wind speed
length(which(is.na(weather_data$wind.speed)))
#precipitation
length(which(is.na(weather_data$precipitation)))
#soil temperature
length(which(is.na(weather_data$soil.moisture)))
#soil moisture
length(which(is.na(weather_data$soil.temp)))
#make plot using filled in points
plot(weather_data$DD, weather_data$soil.moisture, pch=19, type="b", xlab = "Day of Year",
     ylab="Soil moisture (cm3 water per cm3 soil)")
#stopped at qa/qc tests