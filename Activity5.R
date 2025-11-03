#load packages
library(lubridate)
library(ggplot2) #for later

#read in data
stream_data <- read.csv("//Volumes//GEOG331_F25//espina//Data for Class//hw5_data//stream_flow_data.csv", na.strings = c("Eqp"))
## desktop code to read in data
##("Z:\\espina\\Data for Class\\hw5_data\\stream_flow_data.csv", na.strings = c("Eqp"))
precip_data <- read.csv("//Volumes//GEOG331_F25//espina//Data for Class//hw5_data//2049867.csv")
## desktop code
##("Z:\\espina\\Data for Class\\hw5_data\\2049867.csv")                            
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

#basic formatting
aveF <- aggregate(reliable_data$discharge, by=list(reliable_data$doy), FUN="mean")
colnames(aveF) <- c("doy","dailyAve")
sdF <- aggregate(reliable_data$discharge, by=list(reliable_data$doy), FUN="sd")
colnames(sdF) <- c("doy","dailySD")

#question 5
#new plot
dev.new(width=8,height=8)
#bigger margins
par(mai=c(1,1,1,1))

# 2017 data
flow_2017 <- subset(reliable_data, year == 2017) #get 2017 data, not a leap year
avg_2017 <- aggregate(discharge ~ doy, flow_2017, mean, na.rm = TRUE)
colnames(avg_2017) <- c("doy", "daily2017")
#set upper limit
ymax <- max(c(aveF$dailyAve + sdF$dailySD, avg_2017$discharge), na.rm = TRUE)
ymax <- ceiling(ymax/10)* 10 #round

#set month ticks
month_ticks <- yday(ymd(paste(2017, 1:12, 1, sep = "-")))

#plot
plot(aveF$doy, aveF$dailyAve, type = "l",
     xlab = "Month",
     ylab = expression(paste("Discharge ft "^"3 ", "sec"^"-1")),
     lwd = 2,
     ylim = c(0, ymax), #extend from example plot
     xlim = c(1, 365),
     xaxs = "i", yaxs = "i",
     axes = FALSE)

#standard deviation polygon
polygon(c(aveF$doy, rev(aveF$doy)),
        c(aveF$dailyAve - sdF$dailySD,
          rev(aveF$dailyAve + sdF$dailySD)),
        col = rgb(0.392, 0.584, 0.929, .2),
        border = NA)
axis(1, at = month_ticks, labels = month.abb, las = 2)
axis(2, las = 2)#show ticks at 90 degree angle

#add 2017 line
lines(avg_2017$doy, avg_2017$daily2017, col="tomato3", lwd=2)

#add legend
legend("topright", 
       c("mean","1 standard deviation", "2017"), #legend items
       lwd=c(2,NA, 2),#lines
       col=c("black", NA, "tomato3"),#colors
       fill=c(NA, rgb(0.392, 0.584, 0.929,.2), NA),#fill boxes
       border=NA,#no border for both fill boxes (don't need a vector here since both are the same)
       bty="n")#no legend border

#question 7: data frame that indicates what days have full 24 hours of precip measurements
#plot all of them and symbolize the data that has the full 24
#count # of precip measurements per day
precip_counts <- aggregate(precip_data$HPCP, 
                        by = list(year = precip_data$year, doy = precip_data$doy),
                        FUN = length)
colnames(precip_counts) <- c("year", "doy", "num_rows")

#find days with complete measurements
precip_counts$full_day <- precip_counts$num_rows == 24

#add streamflow data -- join using the matching columns (year,doy)
flow_precip <- merge(reliable_data, precip_counts,
                     by = c("year", "doy"),
                     all.x = TRUE) #keep all rows from reliable_data even if there isn't a match
#replace missing full_day values with false
flow_precip$num_rows [is.na(flow_precip$num_obs)] <- 0L
flow_precip$full_day[is.na(flow_precip$full_day)] <- FALSE

#now plot
dev.new(width=8,height=8)
par(mai=c(1,1,1,1))

plot(flow_precip$decYear, flow_precip$discharge,
     type="l",
     lwd = 2,
     col="gray",
     xlab="Year",
     ylab=expression(paste("Discharge ft"^"3"," sec"^"-1")))
points(flow_precip$decYear[flow_precip$full_day],
       flow_precip$discharge[flow_precip$full_day],
       pch=16,
       col="dodgerblue")
legend("topright",
       legend=c("All discharge","Days with full precip data"),
       pch= c(NA, 16),
       lwd = c(2, NA),
       col=c("gray60","dodgerblue"),
       bty="n")


#hydrograph time - EXAMPLE 
#subsest discharge and precipitation within range of interest
hydroD <- reliable_data[reliable_data$doy >= 248 & reliable_data$doy < 250 & reliable_data$year == 2011,]
hydroP <- precip_data[precip_data$doy >= 248 & precip_data$doy < 250 & precip_data$year == 2011,]

#get minimum and maximum range of discharge to plot
#go outside of the range so that it's easy to see high/low values
#floor rounds down the integer
yl <- floor(min(hydroD$discharge))-1
#ceiling rounds up to the integer
yh <- ceiling(max(hydroD$discharge))+1
#minimum and maximum range of precipitation to plot
pl <- 0
pm <-  ceiling(max(hydroP$HPCP))+.5
#scale precipitation to fit on the 
hydroP$pscale <- (((yh-yl)/(pm-pl)) * hydroP$HPCP) + yl

#make plot with points and polygons
par(mai=c(1,1,1,1))
#make plot of discharge
plot(hydroD$decDay,
     hydroD$discharge, 
     type="l", 
     ylim=c(yl,yh), 
     lwd=2,
     xlab="Day of year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")))
#add bars to indicate precipitation 
for(i in 1:nrow(hydroP)){
  polygon(c(hydroP$decDay[i]-0.017,hydroP$decDay[i]-0.017,
            hydroP$decDay[i]+0.017,hydroP$decDay[i]+0.017),
          c(yl,hydroP$pscale[i],hydroP$pscale[i],yl),
          col=rgb(0.392, 0.584, 0.929,.2), border=NA)
}


#question 8 home stretch
#Feb 17–18, 2011
# subset discharge and precip
hydroD_w <- reliable_data[reliable_data$doy >= 48 & reliable_data$doy < 50 & reliable_data$year == 2011, ]
hydroP_w <- precip_data[precip_data$doy >= 48 & precip_data$doy < 50 & precip_data$year == 2011, ]

min(hydroD_w$discharge)

# turn NA precip into 0 so max() works
#hydroP_w$HPCP[is.na(hydroP_w$HPCP)] <- 0

# discharge range
yl_w <- floor(min(hydroD_w$discharge)) - 1
yh_w <- ceiling(max(hydroD_w$discharge)) + 1

# precip range
pl_w <- 0
pm_w <- ceiling(max(hydroP_w$HPCP)) + .5  
# scale precip
hydroP_w$pscale <- (((yh_w - yl_w) / (pm_w - pl_w)) * hydroP_w$HPCP) + yl_w

# plot discharge
par(mai = c(1,1,1,1))

plot(hydroD_w$decDay,
     hydroD_w$discharge,
     type = "l",
     ylim = c(yl_w, yh_w),
     lwd = 2,
     xlab = "Day of year",
     ylab = expression(paste("Discharge ft"^"3"," sec"^"-1")),
     main = "Winter hydrograph (Feb 17–18, 2017)")

# add precip bars (will do nothing if there are 0 rows)
for (i in seq_len(nrow(hydroP_w))) {
  polygon(c(hydroP_w$decDay[i]-0.017, hydroP_w$decDay[i]-0.017,
            hydroP_w$decDay[i]+0.017, hydroP_w$decDay[i]+0.017),
          c(yl_w, hydroP_w$pscale[i], hydroP_w$pscale[i], yl_w),
          col = rgb(0.392, 0.584, 0.929, .2),
          border = NA)
}


#specify year as a factor
reliable_data$yearPlot <- as.factor(reliable_data$year)
#make a boxplot
ggplot(data= reliable_data, aes(yearPlot,discharge)) + 
  geom_boxplot()
#make a violin plot
ggplot(data= reliable_data, aes(yearPlot,discharge)) + 
  geom_violin()

#question 9
#make a season column from the month
dates_all <- as.Date(reliable_data$date, "%m/%d/%Y")
month_all <- as.numeric(format(dates_all, "%m"))

reliable_data$season <- ifelse(month_all %in% 3:5,  "Spring",
                               ifelse(month_all %in% 6:8,  "Summer",
                                      ifelse(month_all %in% 9:11, "Fall", "Winter")))
#subset for 2016 and 2017
flow16 <- reliable_data[reliable_data$year == 2016, ]
flow17 <- reliable_data[reliable_data$year == 2017, ]

#violin plot for 2016
ggplot(data = flow16,
       aes(x = season, y = discharge)) +
  geom_violin(fill = "lightblue") +
  labs(title = "Streamflow by season – 2016",
       x = "Season",
       y = expression(paste("Discharge ft"^"3"," sec"^"-1"))) +
  theme_bw()

# violin plot for 2017
ggplot(data = flow17,
       aes(x = season, y = discharge)) +
  geom_violin(fill = "lightgreen") +
  labs(title = "Streamflow by season – 2017",
       x = "Season",
       y = expression(paste("Discharge ft"^"3"," sec"^"-1"))) +
  theme_bw()

