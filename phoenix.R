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
    '2000' = '//Volumes//GEOG331_F25//espina//Data for Class//final project data//humid//houston//MOD21A1N.A2000174.h09v06.061.2020046033705.hdf', #june 22
    '2024' = '//Volumes//GEOG331_F25//espina//Data for Class//final project data//humid//houston//MOD21A1N.A2024174.h09v06.061.2024175080147.hdf'
  ))


#now, set up function
uhi_analysis <- function(raster_path, shapefile_path, buffer_km = 10){
  data <- rast(raster_path) #load raster (modis tile)
  names(data)
  lst <- data[[1]] #subset to the correct band --> LST currently in K
 
  city <- vect(shapefile_path) #load shapefile of city boundary
  city_projected <- project(city, crs(lst)) #project shp to raster (sinusoidal)
  city_unified <- aggregate(city_projected) #merge to one polygon
  
  lst_c <- lst - 273.15 #convert values to Celsius
  
  #buffer and suburban ring
  buffer_dist_m <- buffer_km * 1000 #km --> meters so it works in terra
  city_buffer <- buffer(city_unified, width = buffer_dist_m)
  city_buffer_ring <- erase(city_buffer, city_unified)
  
  #crop
  lst_crop <- crop(lst_c, city_buffer)
  
  #extract values
  urban_raw <- extract(lst_crop, city_unified)
  urban_vals <- urban_raw[,2]
  suburb_raw <- extract(lst_crop, city_buffer_ring)
  suburb_vals <- suburb_raw[,2]
  
  lst_urban_c_clean <- urban_vals[!is.na(urban_vals)]
  lst_suburb_c_clean <- suburb_vals[!is.na(suburb_vals)]
  
  #stats time
  num_urban <- length(lst_urban_c_clean)
  num_suburb <- length(lst_suburb_c_clean)
  
  mean_urban <- mean(lst_urban_c_clean)
  mean_suburb <- mean(lst_suburb_c_clean)
  difference <- mean_urban - mean_suburb #uhi intensity
  
  median_urban <- median(lst_urban_c_clean)
  median_suburb <- median(lst_suburb_c_clean)
  median_diff <- median_urban - median_suburb
  
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
    set.seed(331)
    
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

#initialize to store data from loop below
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
      suburb_values = res$suburb_values,
      city_boundary = res$city_boundary,
      suburb_ring = res$suburb_ring,
      r_path = raster_path
    )
  }
}

stats_df

plot_uhi_results <- function(city_name, year, res)
{
  par(mfrow = c(2,2), mar = c(4, 4, 3, 1) + .1, oma = c(0, 0, 2, 0)) #set margins (in and out)
  
  #spatial map
  r <- rast(res$r_path)
  lst_raw <- r[[1]] - 273.15
  
  #crop to suburb extent
  lst_crop <- crop(lst_raw, res$suburb_ring)
  
  #plot
  plot(lst_crop, #raster first
  main = paste(city_name, year),
  col = hcl.colors(n = 25, palette = "Heat"),
  axes = FALSE, box = FALSE)
  #mar = c(2, 2, 2, 4))
  
  #add polygons
  plot(res$suburb_ring, add = TRUE, border = "blue", lwd = 2)
  plot(res$city_boundary, add = TRUE, border = "green", lwd = 2)
  
  legend("bottomleft", legend = c("Urban", "Suburban"), col = c("green", "blue"), lwd = 2,
         bty = "n", cex = 0.8)
  
  #histograms and plots
  all_vals <- c(res$urban_values, res$suburb_values)
  common_range <- range(all_vals, na.rm = TRUE)
  common_breaks <- pretty(common_range, n = 40)
  
  dens_urban <- density(res$urban_values, na.rm = TRUE)
  dens_suburb <- density(res$suburb_values, na.rm = TRUE)
  
  #calculate y limits
  max_y_dens <- max(dens_urban$y, dens_suburb$y) * 1.1
  
  max_y_hist <- max(hist(res$urban_values, breaks = common_breaks, plot = FALSE)$density,
                    hist(res$suburb_values, breaks = common_breaks, plot = FALSE)$density) * 1.1
 
  #density plots
  plot(dens_urban,
       col = "tomato", lwd = 3,
       xlim = common_range,
       ylim = c(0, max_y_dens),
       main = "Density Comparison",
       xlab = "Temperature (C)", ylab = "Density")
  lines(dens_suburb, col = "steelblue", lwd = 3)
  legend("topright",
         legend = c("Urban", "Suburban"),
         col = c("tomato", "steelblue"),
         lwd = 3, bty = "n", cex=0.8)
  
  
  #urban histogram
  hist(res$urban_values,
       breaks = common_breaks, freq = FALSE, 
       xlim = common_range,
       ylim = c(0, max_y_hist),
       col = "tomato",
       main = "Urban Distribution",
       xlab = "Temperature (C)")
  lines(dens_urban, col = "darkred", lwd = 2)
 
   #suburb histogram
  hist(res$suburb_values,
       breaks = common_breaks, freq = FALSE,
       xlim = common_range,
       ylim = c(0, max_y_hist),
       col = "steelblue",
       main = "Suburban Distribution",
       xlab = "Temperature (C)")
  lines(dens_suburb, col = "darkblue", lwd = 2)
  
  par(mfrow = c(1,1), mar = c(5, 4, 4, 2) + 0.1, oma = c(0,0,0,0))
}



#next loop for plotting
for (city in names(plot_data)) {
  
  for (yr in names(plot_data[[city]])) {
    cat("Generating plots for:", city, "-", yr, "\n")
    
    plot_uhi_results(
      city_name = city,
      year = yr,
      res = plot_data[[city]][[yr]]
    )
  }
}

######compare climates (arid v humid)########
library("tidyr") ##adding down here so terra extract works above

#add climate label
stats_df$Climate <- factor(stats_df$Climate, levels = c("arid","humid"))

# ensure year is numeric if needed
stats_df$Year <- as.numeric(stats_df$Year)

#compare UHI intensity by climate for each year
t_arid_humid_2000 <- t.test(Mean_Diff_C ~ Climate, data = subset(stats_df, Year == 2000))
t_arid_humid_2024 <- t.test(Mean_Diff_C ~ Climate, data = subset(stats_df, Year == 2024))

t_arid_humid_2000
t_arid_humid_2024

#check change over time (2000 vs 2024)
stats_wide <- pivot_wider(stats_df,
  id_cols = c(City, Climate),
  names_from = Year,
  values_from = Mean_Diff_C)

#keep only rows that have both years
stats_wide <- stats_wide[complete.cases(stats_wide), ]
stats_wide

#create Delta_UHI
stats_wide$Delta_UHI <- stats_wide$`2024` - stats_wide$`2000`

#run test
t_delta <- t.test(Delta_UHI ~ Climate, data = stats_wide)
t_delta

#check
stats_df[stats_df$City == "houston", ]

################################################################################