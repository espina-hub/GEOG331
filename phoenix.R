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
    '2000' = '//Volumes//GEOG331_F25//espina//Data for Class//final project data//vegas and phoenix//MOD21A1N.A2000177.h08v05.061.2020047214800.hdf',
    '2024' = '//Volumes//GEOG331_F25//espina//Data for Class//final project data//vegas and phoenix//MOD21A1N.A2024173.h08v05.061.2024174075550.hdf'
  ),
  vegas = list(
    '2000' = '//Volumes//GEOG331_F25//espina//Data for Class//final project data//vegas and phoenix//MOD21A1N.A2000173.h08v05.061.2020046031144.hdf',
    '2024' = '//Volumes//GEOG331_F25//espina//Data for Class//final project data//vegas and phoenix//MOD21A1N.A2024173.h08v05.061.2024174075550.hdf'
  ),
  orlando = list(
    '2000' = '//Volumes//GEOG331_F25//espina//Data for Class//final project data//humid//orlando//MOD21A1N.A2000162.h10v06.061.2020045221848.hdf', #june 10
    '2024' = '//Volumes//GEOG331_F25//espina//Data for Class//final project data//humid//orlando//MOD21A1N.A2024178.h10v06.061.2024180010435.hdf'#june 26
  ),
  houston = list(
    '2000' = '//Volumes//GEOG331_F25//espina//Data for Class//final project data//humid//houston//MOD21A1N.A2000173.h09v06.061.2020046031416.hdf',
    '2024' = '//Volumes//GEOG331_F25//espina//Data for Class//final project data//humid//houston//MOD21A1N.A2024173.h09v06.061.2024174075414.hdf'
  ))


#now, set up function
uhi_analysis <- function(raster_path, shapefile_path, buffer_km = 10){
  data <- rast(raster_path) #load raster (modis tile)
  lst <- data[[1]] #subset to the correct band --> LST currently in K
  city <- vect(shapefile_path) #load shapefile of city boundary
  
  city_modis <- project(city, crs(lst)) #project shp to raster (sinusoidal)
  city_unified <- aggregate(city_modis) #merge to one polygon
  
  lst_c <- lst - 273.15 #convert values to Celsius
  
  lst_crop_city_c <- crop(lst_c, city_unified)
  #buffer and suburban ring
  buffer_dist_m <- buffer_km * 1000 #km --> meters so it works in terra
  city_buffer <- buffer(city_unified, width = buffer_dist_m)
  city_buffer_ring <- erase(city_buffer, city_unified)
  
  #extract values
  urban_vals <- extract(lst_c, city_unified)[[2]]
  suburb_vals <- extract(lst_c, city_buffer_ring)[[2]]
  
  lst_urban_c_clean <- urban_vals[!is.na(urban_vals)]
  lst_suburb_c_clean <- suburb_vals[!is.na(suburb_vals)]
  
  #stats time
  num_urban <- length(lst_urban_c_clean)
  num_suburb <- length(lst_suburb_c_clean)
  
  mean_urban <- mean(lst_urban_c_clean)
  mean_suburb <- mean(lst_suburb_c_clean)
  difference <- mean_urban - mean_suburban #uhi intensity
  
  median_urban <- median(lst_urban_c_clean)
  median_suburb <- median(lst_suburb_c_clean)
  median_diff <- median_urban - median_suburban
  
  sd_urban <- sd(lst_urban_c_clean)
  sd_suburb <- sd(lst_suburb_c_clean)
  
  #Cohen's D (effect size) + T test
  cohens_d <- NA
  t_test_result <- NA
  shapiro_urban <- NA
  shapiro_suburb <- NA
  
  #cohens d
  if (num_urban > 1 && num_suburb > 1) {
    s_pooled <- sqrt(
      ((num_urban-1)* sd_urban^2 + (num_suburb - 1)* sd_suburb^2) /
        (num_urban + num_suburb -2))
    cohens_d <- (mean_urban - mean_suburb) / s_pooled
    
    #t test
    t_test_result <- t.test(lst_urban_c_clean, lst_suburb_c_clean)
    
    #check normality (shapiro wilk)
    urban_sample <- sample(lst_urban_c_clean, min(5000, num_urban))
    suburb_sample <- sample(lst_suburb_c_clean, min(5000, num_suburb))
    
    if (length(urban_sample) >= 3) {
      shapiro_urban <- shapiro.test(urban_sample)
    }
    if (length(suburb_sample) >= 3) {
      shapiro_suburb <- shapiro.test(suburb_sample)
    }
  }
  
  
  #return list of outputs
  return(list(
    # summary vectors
    summary_urban = summary(lst_urban_c_clean),
    summary_suburb = summary(lst_suburb_c_clean),
    
    # stats
    mean_urban = mean_urban,
    mean_suburb = mean_suburb,
    difference = difference,
    median_urban = median_urban,
    median_suburb = median_suburb, 
    median_diff = median_diff,
    sd_urban = sd_urban,
    sd_suburb = sd_suburb,
    cohens_d = cohens_d,
    
    # counts
    n_urban = num_urban,
    n_suburb = num_suburb,
    
    #vectors for plotting 
    urban_values = lst_urban_c_clean,
    suburb_values = lst_suburb_c_clean,
    city_boundary = city_unified,         # For the map overlay
    suburb_ring = city_buffer_ring,    # For the map overlay
    
    #t_test + distribution test
    t_test = t_test_result,
    shapiro_urban = shapiro_urban,
    shapiro_suburb = shapiro_suburb
  ))
}

stats_df <- data.frame()
plot_data <- list()


#now run the function
for (city in names(city_modis)){
  shapefile_path <- city_shps[[city]]
  
  plot_data[[city]] <- list() #initialize for city in plot_data
  
  for (yr in names(city_modis[[city]])) {
    raster_path <- city_modis[[city]][[yr]]
    cat("Processing:", city, "-", yr, "\n") #lets me track where the code is at
    
    res <- uhi_analysis(
      raster_path = raster_path,
      shapefile_path = shapefile_path,
      buffer_km = 10
    )
    
    new_row <- data.frame(
      City = city,
      Year = yr,
      Mean_Diff_C = res$difference,
      Median_Diff_C = res$median_diff,
      Cohens_D = res$cohens_d,
      Urban_Mean = res$mean_urban,
      Suburb_Mean = res$mean_suburb,
      Urban_N = res$n_urban,
      Suburban_N = res$n_suburb,
      P_value = if(is.list(res$t_test)) res$t_test$p.value else NA
    )
    
    #append to stats dataframe
    stats_df <- rbind(stats_df, new_row)
    
    #extract vectors for plotting list
    plot_data[[city]][[yr]] <- list(
      urban_values = res$urban_values,
      suburb_values = res$suburb_values
    )
  }
}



plot_uhi_results <- function(city_name, year, res, raster_path)
{
  par(mfrow = c(1,3), mar = c(4, 4, 2, 1) + .1, oma = c(0, 0, 0, 0)) #set margins (in and out)
  
  #spatial map
  #data <- terra::rast(raster_path)
  #lst_c <- data[[1]] - 273.15
  
  #plot(lst_c, #raster first
  # main = "Urban Heat Island Context",
  #cex.main = 1,
  #col = hcl.colors(n = 25, palette = "Heat"),
  #mar = c(3, 3, 2, 6))
  
  #title for grid
  #  main_title <- paste("UHI Analysis for", city_name, year)
  #  mtext(main_title, outer = TRUE, cex = 1.5, font = 2)
  
  #  plot(res$suburb_ring,
  #      add = TRUE, 
  #     border = "blue",
  #      col = NA,
  #     lwd = 2)
  
  # plot(res$city_boundary, #urban core
  #   add = TRUE,
  #    border = "red",
  #      col = NA,
  #    lwd = 3)
  
  #legend("bottomleft",
  #     legend = c("Urban Core", "Suburban Ring"),
  #    lwd = c(3, 2),
  #   col = c("red", "blue"), 
  #  bty = "n")
  
  #histograms and plots
  all_vals <- c(res$urban_values, res$suburb_values)
  common_breaks <- pretty(range(all_vals, na.rm = TRUE), n = 40)
  
  dens_urban <- density(res$urban_values, na.rm = TRUE)
  dens_suburb <- density(res$suburb_values, na.rm = TRUE)
  
  #calculate y limits
  max_y_hist <- max(hist(res$urban_values, breaks = common_breaks, plot = FALSE)$density,
                    hist(res$suburb_values, breaks = common_breaks, plot = FALSE)$density) * 1.1 #google says good
  max_y_dens <- max(dens_urban$y, dens_suburb$y) * 1.1 #use 1.1 to make look pretty
  
  #urban histogram
  hist(res$urban_values,
       breaks = common_breaks, freq = FALSE, 
       ylim = c(0, max_y_hist),
       col = "tomato",
       main = "Urban LST Distribution (C)",
       xlab = "Temperature (C)")
  #suburb histogram
  hist(res$suburb_values,
       breaks = common_breaks, freq = FALSE,
       ylim = c(0, max_y_hist),
       col = "steelblue",
       main = "Suburban LST Distribution (C)",
       xlab = "Temperature (C)")
  
  #density plot
  plot(dens_urban,
       col = "tomato", lwd = 3,
       ylim = c(0, max_y_dens),
       main = "Urban vs Suburban Density Comparison",
       xlab = "Temperature (C)", ylab = "Density")
  lines(dens_suburb, col = "steelblue", lwd = 3)
  legend("topright",
         legend = c("Urban", "Suburban"),
         col = c("tomato", "steelblue"),
         lwd = 3, bty = "n")
  par(mfrow = c(1,1), mar = c(5, 4, 4, 2) + 0.1, oma = c(0,0,0,0))
}

#next loop for plotting
for (city in names(results)) {
  
  for (yr in names(results[[city]])) {
    cat("Generating plots for:", city, "-", yr, "\n")
    
    #current_raster_path <- city_modis[[city]][[yr]]  
    
    plot_uhi_results(
      city_name = city,
      year = yr,
      res = results[[city]][[yr]],
      #raster_path = current_raster_path
    )
  }
}




################################################################################

#all of my original code is here -- the t test values are diff from loop and from this for phx 
#keeping this for now as well so i can easily add plots and histograms and whatever else to
#my function and for loops above --> right now they are just for phoenix in 2000

# Path to MODIS HDF file -- file contains tile that phoenix is included in
#desktop file
#file <- "Z:\\espina\\Data for Class\\final project data\\MOD21A1N.A2000173.h08v05.061.2020046031144.hdf"
#mac file
file <- '/Volumes/GEOG331_F25/espina/Data for Class/final project data/vegas and phoenix/MOD21A1N.A2000173.h08v05.061.2020046031144.hdf'

#subset to the LST file within
data <- rast(file)
lst <- data[[1]]

summary(lst) #matches above from fn
#phoenix shapefile
#desktop
#phx <- vect("Z:\\espina\\Data for Class\\final project data\\kx-phoenix-city-boundary-SHP\\phoenix-city-boundary.shp")
#mac version
phx <- vect('/Volumes/GEOG331_F25/espina/Data for Class/final project data/vegas and phoenix/kx-phoenix-city-boundary-SHP')

#project phoenix shapefile into the modis
phx_modis <- project(phx, crs(lst)) 
#confirm re projection worked and now matches the modis tile
crs(phx_modis)

plot(lst)
plot(phx, add = TRUE)


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
phx_buf_dist <- 10000  # 10 km suburban buffer
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
length(lst_suburb_c_clean)
length(lst_urban_c_clean)

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
mean_suburb <- mean(lst_suburb_c_clean)
difference <- mean_urban - mean_suburban

mean_urban
mean_suburban

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
