library(lubridate)
stream_data <- read.csv("//Volumes//GEOG331_F25//espina//Data for Class//hw5_data//stream_flow_data.csv", na.strings = c("Eqp"))
## desktop code to read in data
##("Z:\\espina\\Data for Class\\hw5_data\\stream_flow_data.csv", na.strings = c("Eqp"))
             
precip_data <- read.csv("//Volumes//GEOG331_F25//espina//Data for Class//hw5_data//2049867.csv")
## desktop code
##("Z:\\espina\\Data for Class\\hw5_data\\2049867.csv")                            
head(precip_data)
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

#question 4 -- coming back to later
?expression
??leap_year

#basic formatting
aveF <- aggregate(reliable_data$discharge, by=list(reliable_data$doy), FUN="mean")
colnames(aveF) <- c("doy","dailyAve")
sdF <- aggregate(reliable_data$discharge, by=list(reliable_data$doy), FUN="sd")
colnames(sdF) <- c("doy","dailySD")

#new plot
dev.new(width=8,height=8)
#bigger margins
par(mai=c(1,1,1,1))

#make plot
plot(aveF$doy,aveF$dailyAve, 
     type="l", 
     xlab="Year", ##potentially change to "Day of Year" to make it more accurate
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")),
     lwd=2,
     ylim=c(0,90),
     xaxs="i", yaxs ="i",#remove gaps from axes
     axes=FALSE)#no axes
polygon(c(aveF$doy, rev(aveF$doy)),#x coordinates
        c(aveF$dailyAve-sdF$dailySD,
          rev(aveF$dailyAve+sdF$dailySD)),#ycoord
        col=rgb(0.392, 0.584, 0.929,.2), #color that is semi-transparent
        border=NA #no border
        )   
month_ticks <- yday(ymd(paste(2017, 1:12, 1, sep = "-")))
axis(1, at = month_ticks, labels = month.abb, las = 2)
axis(2, seq(0,80, by=20),
   labels = seq(0, 80, by=20),
   las = 2)#show ticks at 90 degree angle
flow_2017 <- subset(reliable_data, year == 2017) #question 5 this line and below
lines(flow_2017$doy, flow_2017$discharge, col="tomato3", lwd=2)
legend("topright", c("mean","1 standard deviation"), #legend items
       lwd=c(2,NA),#lines
       col=c("black",rgb(0.392, 0.584, 0.929,.2)),#colors
       pch=c(NA,15),#symbols
       fill=c(NA,rgb(0.392, 0.584, 0.929,.2)),#fill boxes
       border=NA,#no border for both fill boxes (don't need a vector here since both are the same)
       bty="n")#no legend border


