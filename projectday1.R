##Beginning Final Project!! Work day 1 (11/6) and 2 (11/13)

library(terra)

# Path to your MODIS HDF file -- file contains tile that phoenix is included in
file <- "Z:\\espina\\Data for Class\\final project data\\MOD21A1N.A2000173.h08v05.061.2020046031144.hdf"

#subsets
data <- rast("Z:\\espina\\Data for Class\\final project data\\MOD21A1N.A2000173.h08v05.061.2020046031144.hdf")
lst <- data[[1]]

#phoenix shapefile
phx <- vect("Z:\\espina\\Data for Class\\final project data\\City_Parcels\\City_Parcels.shp")
#project phoenix shapefile into the modis

phxphx_modis <- project(phx, crs(lst)) 

#extract phx from modis tile
values_phx <- extract(lst, phx_modis)
values_phx


#convert temperature to celsius -- supposedly just raw numbers
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
