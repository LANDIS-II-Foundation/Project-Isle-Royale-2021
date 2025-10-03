library("tidyverse")
library("terra")

#get climate from downloaded gridmet
isro_poly <- sf::st_read("./Parameterization/Parameterization data/ir_polygon/ir_polygon2.shp")%>%
  sf::st_union() %>%
  sf::st_transform(crs = "epsg:4326")

vars <- c("rmin", "rmax")
years <- 1979:2019

filenames <- expand.grid("D:/Data/gridmet/", vars, "_", years, ".nc",
                         stringsAsFactors = FALSE)%>%
  tidyr::unite(col = filename, sep = "")
filenames <- filenames$filename

climate_extract <- data.frame(variable = character(0L),
                              day = character(0L),
                              value = numeric(0L))

i <- 1
for(i in 1:length(filenames)){
  test <- rast(filenames[i]) %>%
    terra::crop(vect(isro_poly))
  layer_names <- names(test)
  
  extracted <- global(test, mean, na.rm = TRUE)
  day <- map_chr(str_split(rownames(extracted), "="), 2)
  variable <- filenames[i] %>% 
    str_split("/")%>%
    pluck(1,4) %>%
    str_split("_") %>%
    pluck(1,1)
  
  climate_extract_temp <- data.frame(variable = variable,
                                     day = day,
                                     value = extracted$mean)
  
  climate_extract <- rbind(climate_extract, climate_extract_temp)
}

climate_extract2 <- climate_extract %>%
  mutate(date = as.Date(as.numeric(day), origin = "1900-01-01"))

rmin_data <- climate_extract2[climate_extract2$variable == "rmin", ] %>%
  mutate(TIMESTEP = paste0(format(date, "%Y-%m-%d"),"T00:00:00Z"), #trick it into the right format; this should probably be done with some POSIX wrangling
         `MEAN(%)` = value,
         `VARIANCE(%^2)` = value,
         `STD_DEV(%)` = value) %>%
  dplyr::select(TIMESTEP, `MEAN(%)`, `VARIANCE(%^2)`, `STD_DEV(%)`)
write.csv(rmin_data, "rmin_data_gridmet.csv")
         
rmax_data <- climate_extract2[climate_extract2$variable == "rmax", ] %>%
  mutate(TIMESTEP = paste0(format(date, "%Y-%m-%d"),"T00:00:00Z"),
         `MEAN(%)` = value,
         `VARIANCE(%^2)` = value,
         `STD_DEV(%)` = value) %>%
  dplyr::select(TIMESTEP, `MEAN(%)`, `VARIANCE(%^2)`, `STD_DEV(%)`)
write.csv(rmax_data, "rmax_data_gridmet.csv")
