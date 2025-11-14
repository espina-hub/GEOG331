##Beginning Final Project!! Work day 1 (11/6) and 2 (11/13)

library("terra")

# Path to your MODIS HDF file -- file contains tile that phoenix is included in
#desktop file
#file <- "Z:\\espina\\Data for Class\\final project data\\MOD21A1N.A2000173.h08v05.061.2020046031144.hdf"
#mac file
file <- "//Volumes//GEOG331_F25//espina/Data for Class//final project data//MOD21A1N.A2000173.h08v05.061.2020046031144.hdf"

#subsets
#desktop subset

data <- rast(file)
lst <- data[[1]]

summary(lst)
#phoenix shapefile
#desktop
#phx <- vect("Z:\\espina\\Data for Class\\final project data\\City_Parcels\\City_Parcels.shp")
#mac version
phx <- vect('//Volumes//GEOG331_F25//espina//Data for Class//final project data//kx-phoenix-city-boundary-SHP//phoenix-city-boundary.shp')

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

