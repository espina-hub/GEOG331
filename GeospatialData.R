library(terra)
library(tidyterra)
library(FedData)

nlcd_meve16 <-get_nlcd(template = FedData::meve,
                       label = "meve",
                       year = 2016,
                       extraction.dir = "Z:\\espina\\Data for Class")
nlcd_meve16

terra::plot(nlcd_meve16)

cavm <- vect("Z:\\GEOG331_F25\\espina\\Data for Class\\cp_veg_la_shp\\")

cavm