##Beginning Final Project!! Work day 1 (11/6)
#install.packages(c("terra", "gdalUtilities")
#install.packages("MODISTools")

library(terra)

# Path to your MODIS HDF file -- file contains tile that phoenix is included in
file <- "Z:\\espina\\Data for Class\\final project data\\MOD21A1N.A2000173.h08v05.061.2020046031144.hdf"

#subsets
sds <- terra::sds("Z:\\espina\\Data for Class\\final project data\\MOD21A1N.A2000173.h08v05.061.2020046031144.hdf")
sds

names(sds)
ndvi <- sds[[1]]

#reproject to WGS84 so i can use lat/long coordinates
ndvi_wgs84 <- terra::project(ndvi,"EPSG:4326")
extent_phoenix <- ext(-113, -111, 32.5, 34.5)

ndvi_phoenix <- crop(ndvi_wgs84, extent_phoenix)
plot(ndvi_phoenix)
