library("terra")
#setting up workflow! goal: for loop or lapply to iterate through locations and dates

#begin with city shapefiles in a list
city_shps <- list(
    phoenix = '//Volumes//GEOG331_F25//espina//Data for Class//final project data//vegas and phoenix//kx-phoenix-city-boundary-SHP',
    vegas = '//Volumes//GEOG331_F25//espina//Data for Class//final project data//vegas and phoenix//Vegas City_Limits',
    orlando = '//Volumes//GEOG331_F25//espina//Data for Class//final project data//humid//orlando//OrlandoCityLimits_20251204',
    houston = '//Volumes//GEOG331_F25//espina//Data for Class//final project data//humid//houston/kx-houston-texas-city-limits-SHP'
)

#add modis tiles in a list: first, list of cities, with a list of the tiles nested within 
city_modis <- list(
  phoenix = list(
    '2000' = '//Volumes//GEOG331_F25//espina//Data for Class//final project data//vegas and phoenix//MOD21A1N.A2000173.h08v05.061.2020046031144.hdf',
    '2024' = '//Volumes//GEOG331_F25//espina//Data for Class//final project data//vegas and phoenix//MOD21A1N.A2024173.h08v05.061.2024174075550.hdf'
  ),
  vegas = list(
    '2000' = '//Volumes//GEOG331_F25//espina//Data for Class//final project data//vegas and phoenix//MOD21A1N.A2000173.h08v05.061.2020046031144.hdf',
    '2024' = '//Volumes//GEOG331_F25//espina//Data for Class//final project data//vegas and phoenix//MOD21A1N.A2024173.h08v05.061.2024174075550.hdf'
  ),
  orlando = list(
    '2000' = '//Volumes//GEOG331_F25//espina//Data for Class//final project data//humid//orlando//MOD21A1N.A2000173.h10v06.061.2020046031443.hdf',
    '2024' = '//Volumes//GEOG331_F25//espina//Data for Class//final project data//humid//orlando//MOD21A1N.A2024173.h10v06.061.2024174075607.hdf'
  ),
  houston = list(
    '2000' = '//Volumes//GEOG331_F25//espina//Data for Class//final project data//humid//houston//MOD21A1N.A2000173.h09v06.061.2020046031416.hdf',
    '2024' = '//Volumes//GEOG331_F25//espina//Data for Class//final project data//humid//houston//MOD21A1N.A2024173.h09v06.061.2024174075414.hdf'
  ))


# ORLANDO 2000 -- ASK LORANTY

raster_path    <- city_modis$orlando[["2000"]]
shapefile_path <- city_shps$orlando

data  <- rast(raster_path)
lst   <- data[[1]]
city  <- vect(shapefile_path)

city_modis <- project(city, crs(lst))

lst_c <- lst - 273.15

# no crop this time, just full tile
buffer_dist_m    <- 15 * 1000
city_buffer      <- buffer(city_modis, width = buffer_dist_m)
city_buffer_ring <- erase(city_buffer, city_modis)

urban_vals  <- extract(lst_c, city_modis)[[2]]
suburb_vals <- extract(lst_c, city_buffer_ring)[[2]]

cat("Raw lengths (before NA removal):\n")
length(urban_vals)
length(suburb_vals)

cat("Non-NA counts:\n")
sum(!is.na(urban_vals))
sum(!is.na(suburb_vals))

cat("Urban summary:\n")
summary(urban_vals)

cat("Suburb summary:\n")
summary(suburb_vals)

#long story short: something weird is happening here


#now, set up function
uhi_analysis <- function(raster_path, shapefile_path, buffer_km = 15){
 data <- rast(raster_path) #load raster (modis tile)
 lst <- data[[1]] #subset to the correct band --> LST currently in K
 city <- vect(shapefile_path) #load shapefile of city boundary
 
 city_modis <- project(city, crs(lst)) #project shp to raster
 
 lst_c <- lst - 273.15 #convert values to Celsius
 
 lst_crop_city_c <- crop(lst_c, city_modis) #crop raster to city ext
 
 #buffer and suburban ring
 buffer_dist_m <- buffer_km * 1000 #km --> meters so it works in terra
 city_buffer <- buffer(city_modis, width = buffer_dist_m)
 city_buffer_ring <- erase(city_buffer, city_modis)
 
 #extract values
 urban_vals <- extract(lst_c, city_modis)[[2]]
 suburb_vals <- extract(lst_c, city_buffer_ring)[[2]]

 lst_urban_c_clean <- urban_vals[!is.na(urban_vals)]
 lst_suburb_c_clean <- suburb_vals[!is.na(suburb_vals)]
 
 #stats time
 mean_urban <- mean(lst_urban_c_clean)
 mean_suburban <- mean(lst_suburb_c_clean)
 difference <- mean_urban - mean_suburban
 
 #normality checks
 urban_sample <- sample(lst_urban_c_clean, min(5000, length(lst_urban_c_clean)))
 suburb_sample <- sample(lst_suburb_c_clean, min(5000, length(lst_suburb_c_clean)))
 
 #t-test 
 if (length(lst_urban_c_clean) < 2 || length(lst_suburb_c_clean) < 2) {
   t_test_result <- NA
 } else {
   t_test_result <- t.test(lst_urban_c_clean, lst_suburb_c_clean)
 }
 
 
 #return list of outputs
 return(list(
   mean_urban = mean_urban,
   mean_suburban = mean_suburban,
   difference = difference,
   n_urban = length(lst_urban_c_clean),
   n_suburban = length(lst_suburb_c_clean),
   t_test = t_test_result
 ))
                        
 }
 
results <- list()

#now run the functino
for (city in names(city_modis))
{
  shapefile_path <- city_shps[[city]]
results[[city]] <- list()

  for (yr in names(city_modis[[city]])) {
    raster_path <- city_modis[[city]][[yr]]
    cat("Processing:", city, "-", yr, "\n")
    
    results[[city]][[yr]] <- uhi_analysis(
      raster_path = raster_path,
      shapefile_path = shapefile_path,
      buffer_km = 15
    )
  }
}

results
test_phx_2000 <- uhi_analysis(
  raster_path    = city_modis$phoenix[['2000']],
  shapefile_path = city_shps$phoenix,
  buffer_km      = 15
)

#suggests that the code works but there are differences from my original version despite using
#the same data and methods
test_phx_2000$mean_urban
test_phx_2000$mean_suburban
test_phx_2000$difference
test_phx_2000$t_test$p.value




#all of my original code is here -- the t test values are diff from loop and from this for phx 
#keeping this for now as well so i can easily add plots and histograms and whatever else to
#my function and for loops above --> right now they are just for phoenix in 2000

#also, do i need to do a shapiro test? the volume of my data is LARGE so i feel like its not 
#really necessary, especially because it will only work for some locations

# Path to MODIS HDF file -- file contains tile that phoenix is included in
#desktop file
#file <- "Z:\\espina\\Data for Class\\final project data\\MOD21A1N.A2000173.h08v05.061.2020046031144.hdf"
#mac file
file <- '/Volumes/GEOG331_F25/espina/Data for Class/final project data/vegas and phoenix/MOD21A1N.A2000173.h08v05.061.2020046031144.hdf'

#subset to the LST file within
data <- rast(file)
lst <- data[[1]]

summary(lst)
#phoenix shapefile
#desktop
#phx <- vect("Z:\\espina\\Data for Class\\final project data\\kx-phoenix-city-boundary-SHP\\phoenix-city-boundary.shp")
#mac version
phx <- vect('/Volumes/GEOG331_F25/espina/Data for Class/final project data/vegas and phoenix/kx-phoenix-city-boundary-SHP')

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
lst_suburb_c_clean <- suburb_vals[!is.na(lst_suburb_c_clean)]

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
