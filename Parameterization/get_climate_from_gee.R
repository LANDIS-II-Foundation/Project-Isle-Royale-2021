library("rgee")
library("sf")
library("tidyverse")

ee_Initialize(user = "swflake@ncsu.edu", drive = TRUE)

#what date range and climate variables do we want? 
date_start <- "1980-01-01"
date_end <- "1980-01-02"
climate_layers <- c("tmmx", "tmmn", "pr", "rmax", "rmin", "vs", "th") #names of variables in GridMET
# climate_layers <- "tmmx"
# climate_names <- c("Tmax", "Tmin", "Prcp", "maxRH", "minRH", "wind_speed", "winddirection") #names needed by Climate Library

#load gridmet dataset
# gridmet = ee$ImageCollection('IDAHO_EPSCOR/GRIDMET')$
#   filterDate(date_start, date_end)$
#   select(climate_layers)
gridmet = ee$ImageCollection('IDAHO_EPSCOR/GRIDMET')$
    filterDate(date_start, date_end)$
    select(climate_layers)


regions <- sf::st_read("./Parameterization/Parameterization data/ir_polygon/ir_polygon2.shp") %>%
  sf::st_make_valid() %>%
  sf::st_transform("EPSG:4326") %>%
  sf::st_union() %>%
  rgee::sf_as_ee()

# st_write(regions, "ecoregions_elev.shp")

#plot them to make sure things line up right
Map$addLayer(gridmet$select("tmmx")$first()) + 
  Map$addLayer(regions)

# regions$select('label')$getInfo()
# ee_print(regions)

imfunc <- function(image) {
  return(
    image$select(climate_layers)$
      reduceRegions(
        collection = regions, 
        reducer = ee$Reducer$mean(), #get mean of raster within each (multi)polygon
        scale = 120)$
      map(function(f) { 
        return(f$set(list('imageId' = image$id())))} #add imageID (date) to tabular data
      )
  )
}

#map the extracting function over every date in the gridmet stack
triplets = gridmet$map(imfunc)$flatten()

# triplets$first()$getInfo()

#this might take a long time, depending on the length of the timeseries, number
#of climate variables, and number of ecoregions
system.time(clim_data <- ee_as_sf(triplets, via = "drive"))




