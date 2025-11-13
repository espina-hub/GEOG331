##Beginning Final Project!! Work day 1 (11/6) and 2 (11/13)

library(terra)

# Path to your MODIS HDF file -- file contains tile that phoenix is included in
#desktop file
#file <- "Z:\\espina\\Data for Class\\final project data\\MOD21A1N.A2000173.h08v05.061.2020046031144.hdf"
#mac file
file <- "//Volumes//GEOG331_F25//espina/Data for Class//final project data//MOD21A1N.A2000173.h08v05.061.2020046031144.hdf"

#subsets
#desktop subset
data <- rast(file)
lst <- data[[1]]

#phoenix shapefile
#desktop
#phx <- vect("Z:\\espina\\Data for Class\\final project data\\City_Parcels\\City_Parcels.shp")
#mac version
phx <- vect('//Volumes//GEOG331_F25//espina//Data for Class//final project data//City_Parcels//City_Parcels.shp')
#project phoenix shapefile into the modis

phx_modis <- project(phx, crs(lst)) 
print(phx_modis)

#extract phx from modis tile
values_phx <- extract(lst, phx_modis)
values_phx[2]


#convert temperature to celsius - already in Kelvin with a scale of .02
value_phx_k <- value_phx[[2]] * .02
value_phx_k

lst_c_phx <- lst_k_phx - 273.15
lst_c_phx

plot(lst_c_phx, main = "Phoenix Nighttime LST (C)")

hist(values(lst_c_phx), 
     main = "Distribution of Nighttime LST - Phoenix",
     xlab = "Temperature (C)",
     breaks = 50)
quantile (values(lst_c_phx), probs = c(0.01, 0.5, 0.99), na.rm = TRUE)
