library("tidyverse")
library("terra")
library("sf")

#get climate from downloaded macav2-metdata files

isro_poly <- sf::st_read("./Parameterization/Parameterization data/ir_polygon/ir_polygon2.shp")%>%
  sf::st_union() %>%
  sf::st_transform(crs = "epsg:4326")

model <- "MRI-CGCM3"
rcp <- "85"
vars <- c("pr", 
          "rhsmax",
          "rhsmin",
          "tasmin",
          "tasmax",
          "uas",
          "vas")
years <- 2020:2060

filenames <- paste0(model, "-", vars)
filenames <- paste0("D:/Data/maca/", filenames)


climate_extract <- data.frame(variable = character(0L),
                              day = character(0L),
                              value = numeric(0L))

i <- 1
for(i in 1:length(filenames)){
  clim_rast <- terra::rast(filenames[i])
  crs(clim_rast) <- "EPSG:4326"
  extent <- ext(clim_rast) %>% as.vector()
  extent[c(1:2)] <- extent[c(1:2)] - 360
  ext(clim_rast) <- extent
  # plot(clim_rast[[1]])
  # plot(vect(isro_poly), add = TRUE)
  # ext(vect(isro_poly))

  clim_rast <-  terra::crop(clim_rast, vect(isro_poly))
  layer_names <- names(clim_rast)
  
  extracted <- global(clim_rast, mean, na.rm = TRUE)
  day <- time(clim_rast)
  variable <- vars[i]
  
  climate_extract_temp <- data.frame(variable = variable,
                                     day = day,
                                     value = extracted$mean)
  
  climate_extract <- rbind(climate_extract, climate_extract_temp)
}

climate_extract2 <- climate_extract %>%
  filter(lubridate::year(day) >= min(years) & lubridate::year(day) <= max(years))

#---------------------
#restructure for landis-ii-v7 climate library format
var_rows <- c("#ppt",
              "#MaxRH",
              "#MinRH",
              "#Tmin",
              "#Tmax",
              "#wind_easting",
              "#wind_northing")

units_means <- c("mm/d",
                 "%",
                 "%",
                 "C",
                 "C",
                 "m/s",
                 "m/s")
units_variance <- c("mm/d^2",
                    "%^2",
                    "%^2",
                    "C^2",
                    "C^2",
                    "m/s^2",
                    "m/s^2")


TIMESTEP <- character()
means <- character()
variances <- character()
stdev <- character()
for(i in 1:length(var_rows)){
  clim <- climate_extract2 %>% filter(variable == vars[i])
  
  #assemble each column first, as a vector
  
  #the first column has timesteps but also the headers for each variable followed by a blank cell
  TIMESTEP <- c(TIMESTEP, var_rows[i], "", "TIMESTEP", format(clim$day))
  
  means <- c(means, "", "eco1", paste0("MEAN(", units_means[i], ")"), clim$value)
  variances <- c(variances, "", "eco1", paste0("VARIANCE(", units_variance[i], ")"), clim$value)
  stdev <- c(stdev, "", "eco1", paste0("STD_DEV(", units_means[i], ")"), clim$value)
}

output_data <- cbind(TIMESTEP, means, variances, stdev)

write.table(output_data,               # Write CSV file without header
            paste0("./Models/LANDIS inputs/NECN files/MACA_", model, "_rcp", rcp, "_scr.csv"),
            sep = ",",
            col.names = FALSE,
            row.names = FALSE,
            quote = FALSE) # quote = false is important! Otherwise the CL can't read the file, 
                           # but it won't be apparent looking at the data in Excel