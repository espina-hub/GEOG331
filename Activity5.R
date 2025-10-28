library(lubridate)
stream_data <- read.csv("Z:\\espina\\Data for Class\\hw5_data\\stream_flow_data.csv",
                 na.strings = c("Eqp"))
#head(stream_data)              
precip_data <- read.csv("Z:\\espina\\Data for Class\\hw5_data\\2049867.csv")                            
#head(precip_data)
reliable_data <- stream_data[stream_data$discharge.flag == "A",]

#### define time for streamflow #####
#convert date and time
datesD <- as.Date(reliable_data$date, "%m/%d/%Y")
#get day of year
reliable_data$doy <- yday(datesD)
#calculate year
reliable_data$year <- year(datesD)
#define time
timesD <- hm(reliable_data$time)

#### define time for precipitation #####    
dateP <- ymd_hm(precip_data$DATE)
#get day of year
precip_data$doy <- yday(dateP)
#get year 
precip_data$year <- year(dateP)

#### get decimal formats #####
#convert time from a string to a more usable format
#with a decimal hour
reliable_data$hour <- hour(timesD ) + (minute(timesD )/60)
#get full decimal time
reliable_data$decDay <- reliable_data$doy + (reliable_data$hour/24)
#calculate a decimal year, but account for leap year
reliable_data$decYear <- ifelse(leap_year(reliable_data$year), reliable_data$year + (reliable_data$decDay/366),
                       reliable_data$year + (reliable_data$decDay/365))
#calculate times for datP                       
precip_data$hour <- hour(dateP ) + (minute(dateP )/60)
#get full decimal time
precip_data$decDay <- precip_data$doy + (precip_data$hour/24)
#calculate a decimal year, but account for leap year
precip_data$decYear <- ifelse(leap_year(precip_data$year), precip_data$year + (precip_data$decDay/366),
                       precip_data$year + (precip_data$decDay/365))         
#plot discharge
plot(reliable_data$decYear, reliable_data$discharge, type="l", xlab="Year", ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")))

#question 4
?expression
