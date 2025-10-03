#activity3
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
#read in weather data on mac
weather_data <- read.csv("//Volumes//GEOG331_F25//espina//Data for Class//bewkes//bewkes_weather.csv", na.strings=c("#N/A"), skip=3, header=FALSE)
#weather data on pc
#weather_data <- read.csv("Z:\\espina\\Data for Class\\bewkes\\bewkes_weather.csv", na.strings=c("#N/A"), skip=3, header=FALSE)
print(weather_data[1,])
#sensor info on pc
#sensor_info <- read.csv("Z:\\espina\\Data for Class\\bewkes\\bewkes_weather.csv", na.strings=c("#N/A"), nrows=2)
#sensor info on mac
sensor_info <- read.csv("//Volumes//GEOG331_F25//espina//Data for Class//bewkes//bewkes_weather.csv",  na.strings=c("#N/A"), nrows=2)
print(sensor_info)
#above lines answer question 3
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
#see how many values have missing data for each sensor observation -- GET RID OF IN FINAL SUBMIT
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

#QA/QC TESTS
#make a plot with filled in points (using pch)
#line lines
plot(weather_data$DD, weather_data$air.temperature, pch=19, type="b", xlab = "Day of Year",
     ylab="Air temperature (degrees C)")
#converting unreliable data to NA -- using example of temps below freezing 
weather_data$air.tempQ1 <- ifelse(weather_data$air.temperature < 0, NA, weather_data$air.temperature)
#stopped at realistic values heading
#check the values at the extreme range of the data
#and throughout the percentiles
quantile(weather_data$air.tempQ1)
#look at days with really low air temperature
weather_data[weather_data$air.tempQ1 < 8,]
#look at days with really high air temperature
weather_data[weather_data$air.tempQ1 > 33,]  

#precipitation & lightning sensor to find unreliable data - question 4
#plot precipitation and lightning strikes on the same plot
#normalize lighting strikes to match precipitation
lightscale <- (max(weather_data$precipitation)/max(weather_data$lightning.acvitivy)) * weather_data$lightning.acvitivy
plot(weather_data$DD , weather_data$precipitation, xlab = "Day of Year", ylab = "Precipitation & lightning",
     type="n")
#plot precipitation points only when there is precipitation 
#make the points semi-transparent
points(weather_data$DD[weather_data$precipitation > 0], weather_data$precipitation[weather_data$precipitation > 0],
       col= rgb(95/255,158/255,160/255,.5), pch=15)   
#plot lightning points only when there is lightning     
points(weather_data$DD[lightscale > 0], lightscale[lightscale > 0],
       col= "tomato3", pch=19)
#assert function to provide evidence
#first, proves the lengths match
assert(length(lightscale) == nrow(weather_data),
       "Error: lightscale is not the same length as weather_data rows")

#q6
weather_data$air.tempQ2 <- ifelse(weather_data$precipitation  >= 2 & weather_data$lightning.acvitivy > 0, NA,
                          ifelse(weather_data$precipitation > 5, NA, weather_data$air.tempQ1))
#begin cleaning data, replace any record where wind speed is less than 0 with NA
weather_data$wind.speedQ1<- ifelse(weather_data$wind.speed < 0, NA, weather_data$wind.speed)
#continue cleaning data by adding the storm filter from above in airtemp
weather_data$wind.speedQ2 <- ifelse(weather_data$precipitation >= 2 & weather_data$lightning.activity > 0, NA,
                                    ifelse(weather_data$precipitation > 5, NA, weather_data$wind.speedQ1))
#assert test here -- make sure the "NA" values were input 
#there should be more "NA" values now than there was before 
assert(sum(is.na(weather_data$wind.speedQ2)) > sum(is.na(weather_data$wind.speed)),"Error: wind.speedQ2 does not have any new NA values")
#time to create a plot - using the earlier format, but doing type = b to include points and lines
#also make it pink hardy har har
plot(weather_data$DD, weather_data$wind.speedQ2, xlab = "Day of Year", ylab = "Wind Speed - No Storms (m/s)", main = "Cleaned Wind Speed (points and lines)", type = "b", pch=15, col = "palevioletred")

