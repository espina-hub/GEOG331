#libraries
library(terra)
library(tidyterra)
library(FedData)

#load data
#list files lists all files in a particular directory
f <- list.files("Z:\\data\\landsat", full.names = T)
#full names = T gives full file path

# read in files 3-10 as a single multi-band raster (bands 1-7)
lc <- rast(f[3:10])

#look at Band 5 to get information about the data
lc[[5]]

# create a summary of the data values
summary(lc[[5]])

# make a quick plot to see how the data looks
plot(lc[[5]])

# we can perform math on the raster layer right inside the plot call
#doesnt create a permanent change bc its not a new object or being
#written to the file

#high numbers on legend because it's smaller, easier to store
#do math to scale it -- scaling factor given in info page
plot(lc[[5]]*0.0000275-0.2)

#NDVI = normalized difference vegetation index --> diff between
#near infrared and red reflectance divided by sum
# calculate ndvi - performing math using individual bands
ndvi <- (lc[[5]]-lc[[4]])/(lc[[5]]+lc[[4]])


names(ndvi) <- "NDVI"
# create a quick plot to see how the data looks
plot(ndvi)

# read in the shape file of dec lands
dec <- vect("Z://data//NYS_DEC_Lands//")
plot(dec)

# remember from last time that the attribute table can be used
# just like a data frame
# we'll use this to subset to Madison County
mad_dec <- dec[dec$COUNTY=="MADISON",]

# we could also use the crop function
# what dimensions of our raster layer are used to crop the vector layer
# crop dec by lc
      # lc_dec <- crop(dec,lc)

# lastly, create a buffer around the Madison County DEC lands
# what are the units? 
mad_buf <- buffer(mad_dec, width = 1000, singlesided = T)

# create a plot to look at our Madison County data
plot(mad_dec, col = "red")
plot(mad_buf, col = "yellow", add = T)

# calculate ndvi for dec lands
# zonal fn treats all red areas as zones
# zonal gives us vector w ndvi value for each polygon; declared as new
# attribute
mad_dec$ndvi_in <- zonal(ndvi, mad_dec, fun = "mean")

# calculate ndvi for the buffer outside dec lands
mad_dec$ndvi_out <- zonal(ndvi, mad_buf, fun = "mean")

# calculate the difference to see if DEC lands are more productive
mad_dec$ndvi_dif <- mad_dec$ndvi_in-mad_dec$ndvi_out
