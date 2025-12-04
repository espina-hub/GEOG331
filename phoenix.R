##Beginning Final Project!! Work day 1 (11/6) and 2 (11/13)

library("terra")

# Path to your MODIS HDF file -- file contains tile that phoenix is included in
#desktop file
file <- "Z:\\espina\\Data for Class\\final project data\\MOD21A1N.A2000173.h08v05.061.2020046031144.hdf"
#mac file
#file <- "//Volumes//GEOG331_F25//espina/Data for Class//final project data//MOD21A1N.A2000173.h08v05.061.2020046031144.hdf"

#subset to the LST file within
data <- rast(file)
lst <- data[[1]]

summary(lst)
#phoenix shapefile
#desktop
phx <- vect("Z:\\espina\\Data for Class\\final project data\\kx-phoenix-city-boundary-SHP\\phoenix-city-boundary.shp")
#mac version
#phx <- vect('//Volumes//GEOG331_F25//espina//Data for Class//final project data//kx-phoenix-city-boundary-SHP//phoenix-city-boundary.shp')

#project phoenix shapefile into the modis
phx_modis <- project(phx, crs(lst)) 
#confirm re projection worked and now matches the modis tile
crs(phx_modis)
#extract phx from modis tile
values_phx <- extract(lst, phx_modis)
#subset to get the value column, not the ID column
lst_raw_phx <- values_phx[[2]]
summary(lst_raw_phx) #tells me the data is already in kelvin
#convert to celsius
lst_c_phx <- lst_raw_phx - 273.15
summary(lst_c_phx) #check values

#remove NA values
lst_c_phx_clean <- lst_c_phx[!is.na(lst_c_phx)]

#now plot, see what we got
plot(lst, main = "MODIS Nighttime Land Surface Temperature (Phoenix Tile",
     col = terrain.colors(50))
#overlay boundary -- shows teeny tiny phoenix
plot(phx_modis, add = TRUE, border = "red", lwd = 2)

#crop to phoenix 
lst_crop_phx <- crop(lst, phx_modis)
lst_crop_phx_c <- lst_crop_phx - 273.15
plot(lst_crop_phx_c, main = "Phoenix Nighttime Land Surface Temperature (C)", 
     col = terrain.colors(50))
plot(phx_modis, add = TRUE, border = "red", lwd = 2) #overlay outline of phoenix

#get a histogram of the values
hist(lst_c_phx_clean, 
     main = "Distribution of Phoenix Nighttime LST (C)",
     xlab = "Temperature (C)",
     col = "steelblue",
     breaks = 40)

#idk what day it is but next step is to work on making a buffer around phoenix proper
#to allow for comparison b/w values

#set buffer distance 
phx_buf_dist <- 15000  # 15 km suburban buffer
#make buffer
phx_buf <- buffer(phx_modis, width = phx_buf_dist)
#buffer ring --> remove city from it
phx_buf_ring <- erase(phx_buf, phx_modis)

plot(lst_crop_phx_c,
     main = "Phoenix Urban + Suburban Nighttime LST (°C)",
     col = terrain.colors(50))

#suburban ring (BLUE) first
plot(phx_buf_ring,
     add = TRUE,
     border = "blue",
     col = NA,
     lwd = 2)

# urban core (RED) on top
plot(phx_modis,
     add = TRUE,
     border = "red",
     col = NA,
     lwd = 2)

legend("bottomleft",
       legend = c("Urban core", "Suburban ring (15 km)"),
       lwd = 2,
       col = c("red", "blue"))

#now that i know i have the buffer, lets separate it and do specific analysis
# suburban ring
suburb_vals <- extract(lst_crop_phx_c, phx_buf_ring)
lst_suburb_c <- suburb_vals[[2]]
# clean NAs
lst_suburb_c_clean <- lst_suburb_c[!is.na(lst_suburb_c)]

summary(lst_suburb_c_clean)

#extract phoenix
urban_vals <- extract(lst_crop_phx_c, phx_modis)
lst_urban_c_clean <- urban_vals[[2]]
lst_urban_c_clean <- lst_urban_c_clean[!is.na(lst_urban_c_clean)]

#extract suburb
suburb_vals <- extract(lst_crop_phx_c, phx_buf_ring)
lst_suburb_c_clean <- suburb_vals[[2]]
lst_suburb_c_clean <- lst_suburb_c_clean[!is.na(lst_suburb_c_clean)]

#compare
mean_urban <- mean(lst_urban_c_clean)
mean_suburban <- mean(lst_suburb_c_clean)
difference <- mean_urban - mean_suburban

#begin plotting
par(mfrow = c(1, 2))

all_vals <- c(lst_urban_c_clean, lst_suburb_c_clean)
common_breaks <- pretty(range(all_vals, na.rm = TRUE), n = 40)

#build hist objects to get density + ylim
urban_hist  <- hist(lst_urban_c_clean,
                    breaks = common_breaks,
                    plot   = FALSE)
suburb_hist <- hist(lst_suburb_c_clean,
                    breaks = common_breaks,
                    plot   = FALSE)
#set y axis
max_y <- max(urban_hist$density, suburb_hist$density, na.rm = TRUE)

#plots
hist(lst_urban_c_clean,
     breaks = common_breaks,
     freq   = FALSE,
     ylim   = c(0, max_y),
     col    = "tomato",
     main   = "Urban Phoenix Nighttime LST (°C)",
     xlab   = "Temperature (°C)")

hist(lst_suburb_c_clean,
     breaks = common_breaks,
     freq   = FALSE,
     ylim   = c(0, max_y),
     col    = "steelblue",
     main   = "Suburban Ring Nighttime LST (°C)",
     xlab   = "Temperature (°C)")

#density overlay plot
#calculate density 
dens_urban  <- density(lst_urban_c_clean, na.rm = TRUE)
dens_suburb <- density(lst_suburb_c_clean, na.rm = TRUE)

#find a shared y-limit so they fit together
max_y <- max(dens_urban$y, dens_suburb$y)

#plot the urban density
plot(dens_urban,
     col = "tomato",
     lwd = 3,
     ylim = c(0, max_y),
     main = "Urban vs Suburban Nighttime LST Density",
     xlab = "Temperature (°C)",
     ylab = "Density")

#add suburban density
lines(dens_suburb,
      col = "steelblue",
      lwd = 3)

#legend
legend("topright",
       legend = c("Urban Phoenix", "Suburban Ring (15 km)"),
       col = c("tomato", "steelblue"),
       lwd = 3,
       bty = "n")

#t test time -- TRIPLE TTTTT

#check normality first using a shapiro test
urban_sample <- sample(lst_urban_c_clean, min(5000, length(lst_urban_c_clean)))
suburb_sample <- sample(lst_suburb_c_clean, min(5000, length(lst_suburb_c_clean))) 
shapiro.test(urban_sample)
shapiro.test(suburb_sample)

#t test
t_test_result <- t.test(lst_urban_c_clean, lst_suburb_c_clean)
t_test_result
