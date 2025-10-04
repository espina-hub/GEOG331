#activity3
#create a function. The names of the arguments for your function will be in parentheses. Everything in curly brackets will be run each time the function is run.
#helpful for testing your code for errors/mistaken assumptions
assert <- function(statement,err.message){
  #if evaluates if a statement is true or false for a single item
  if(statement == FALSE){
    print(err.message)
  }
}

#read in weather data, skip first 3 rows, specify NA
#read in weather data on mac
weather_data <- read.csv("//Volumes//GEOG331_F25//espina//Data for Class//bewkes//bewkes_weather.csv", na.strings=c("#N/A"), skip=3, header=FALSE)
#weather data on pc
#weather_data <- read.csv("Z:\\espina\\Data for Class\\bewkes\\bewkes_weather.csv", na.strings=c("#N/A"), skip=3, header=FALSE)
#sensor info on pc
#sensor_info <- read.csv("Z:\\espina\\Data for Class\\bewkes\\bewkes_weather.csv", na.strings=c("#N/A"), nrows=2)
#sensor info on mac
sensor_info <- read.csv("//Volumes//GEOG331_F25//espina//Data for Class//bewkes//bewkes_weather.csv",  na.strings=c("#N/A"), nrows=2)
#above lines answer question 3
#get column names from sensor_info table
# and set weather station colnames  to be the same
colnames(weather_data) <- colnames(sensor_info)
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
weather_data$doy <- yday(dates)
weather_data$hour <- hour(dates) + (minute(dates)/60)
weather_data$DD <- weather_data$doy + (weather_data$hour/24)

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
#check the values at the extreme range of the data and throughout the percentiles
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
points(weather_data$DD[weather_data$precipitation > 0], weather_data$precipitation[weather_data$precipitation > 0],
       col= rgb(95/255,158/255,160/255,.5), pch=15)   
#plot lightning points only when there is lightning     
points(weather_data$DD[lightscale > 0], lightscale[lightscale > 0],
       col= "tomato3", pch=19)
#assert function to provide evidence
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
#assert test here -- make sure the "NA" values were input; there should be more "NA" values now than there was before 
assert(sum(is.na(weather_data$wind.speedQ2)) > sum(is.na(weather_data$wind.speed)),"Error: wind.speedQ2 does not have any new NA values")
#time to create a plot - using the earlier format, but doing type = b to include points and lines
#also make it pink hardy har har
plot(weather_data$DD, weather_data$wind.speedQ2, xlab = "Day of Year", ylab = "Wind Speed - No Storms (m/s)", main = "Cleaned Wind Speed (points and lines)", type = "b", pch=15, col = "palevioletred")

#question 7 HOME STRETCH
#i want to see the plots side by side
par(mfrow = c(2, 2))
# plot soil moisture
plot(weather_data$DD, weather_data$soil.moisture, type = "b", pch = 19, col = "forestgreen", xlab = "Day of Year", ylab = "Soil moisture (cm³/cm³)", main = "Soil moisture over season")
# plot soil temperature
plot(weather_data$DD, weather_data$soil.temp, type = "b", pch = 19, col = "darkgoldenrod4", xlab = "Day of Year", ylab = "Soil temperature (°C)", main = "Soil temperature over season")
#compare to air temperature and precipitation 
#chose type h for the precipitation graph for easier visualization of whether or not rain happened
plot(weather_data$DD, weather_data$precipitation, type = "h", col = "darkorchid1", xlab = "Day of Year", ylab = "Precipitation (mm)", main = "Rain events")
plot(weather_data$DD, weather_data$air.temperature, type = "b", pch = 19, col = "deeppink4", xlab = "Day of Year", ylab = "Air temperature (°C)", main = "Air temperature (°C)")

#question 8!!!
#first, find the averages of each column, use mean function, remove all NA values, use cleaned data from above
#also find total precipitation using sum 
mean_airtemp <- mean(weather_data$air.tempQ2, na.rm = TRUE)
mean_windspeed <- mean(weather_data$wind.speedQ2, na.rm = TRUE)
mean_soilmoist <- mean(weather_data$soil.moisture, na.rm = TRUE)
mean_soiltemp <- mean(weather_data$soil.temp, na.rm = TRUE)
total_precip <- sum(weather_data$precipitation, na.rm = TRUE)

#not all days have valid observations -- find num of days for each variable, using cleaned data from earlier
#use !is.na to say that true = values that are present, false = NA 
n_air <- sum(!is.na(weather_data$air.tempQ2))
n_wind <- sum(!is.na(weather_data$wind.speedQ2))
n_moist <- sum(!is.na(weather_data$soil.moisture))
n_soiltemp <- sum(!is.na(weather_data$soil.temp))
n_precip <- sum(!is.na(weather_data$precipitation))

#find dates covered, add it into table
range_dates <- range(weather_data$timestamp, na.rm = TRUE)
#format it 
start_str <- format(min(dates, na.rm = TRUE), "%b %d, %Y")
end_str   <- format(max(dates, na.rm = TRUE), "%b %d, %Y")
#make a table -- using data.frame so it appears in the console 
results <- data.frame(Variable = c("Air temperature (°C)", "Wind speed (m/s)", "Soil moisture (cm³/cm³)", "Soil temperature (°C)", "Total precipitation (mm)"), 
                      Average_or_Total = c(mean_airtemp, mean_windspeed, mean_soilmoist, mean_soiltemp, total_precip), 
                      Observations_used = c(n_air, n_wind, n_moist, n_soiltemp, n_precip))
#round decimal places to account for the sensor's error abilities
results$Average_or_Total <- c(round(mean_airtemp, 1), round(mean_windspeed, 1), round(mean_soilmoist, 3), round(mean_soiltemp, 1),round(total_precip, 1))
#add date range
results$Period <- paste(start_str, "to", end_str)
results

#question 9 YAYYY
## Same x-axis for all four panels -- set it so i dont have to do it later lol
xr <- range(weather_data$DD, na.rm = TRUE)
par(mfrow = c(2, 2), mar = c(4,4,2.5,1))
# soil moisture
plot(weather_data$DD, weather_data$soil.moisture,
     type = "b", pch = 1, col = "forestgreen",
     xlab = "Day of Year", ylab = "Soil moisture (cm³/cm³)",
     xlim = xr, main = "Soil moisture")

#soil temp
plot(weather_data$DD, weather_data$soil.temp,
     type = "b", pch = 1, col = "darkgoldenrod4",
     xlab = "Day of Year", ylab = "Soil temperature (°C)",
     xlim = xr, main = "Soil temperature")

#precipitation
plot(weather_data$DD, weather_data$precipitation,
     type = "h", col = "darkorchid1",
     xlab = "Day of Year", ylab = "Precipitation (mm)",
     xlim = xr, main = "Precipitation")

#air temperature
plot(weather_data$DD, weather_data$air.temperature,
     type = "b", pch = 1, col = "deeppink4",
     xlab = "Day of Year", ylab = "Air temperature (°C)",
     xlim = xr, main = "Air temperature")
#ALL DONE

