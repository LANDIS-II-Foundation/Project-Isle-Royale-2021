#make slope steepness and azimuth from DEM
# dem from SRTM, downloaded from earthexplorer
library("tidyverse")
library("terra")
library("stars")

region <- sf::st_read("D:/Data/epa_ecoregions/us_eco_l3/us_eco_l3.shp") %>%
  filter(US_L3CODE %in% c(50,51)) %>%
  sf::st_union()%>%
  sf::st_transform("EPSG:4326")


#import SRTM rasters
srtm_dir <- "D:/Data/SRTM 1 Arc-Second Global/midwest"
srtm_list <- list.files(srtm_dir, recursive = TRUE) %>% 
  as.data.frame() %>%
  mutate(ext = tools::file_ext(.)) %>%
  filter(ext == "hgt") %>%
  mutate(full_path = paste0(srtm_dir, "/", .))

raster_list <- lapply(srtm_list$full_path, terra::rast)

clip_raster <- function(dem_rast){
  clipped <- tryCatch({
  terra::crop(dem_rast, region) %>%
      terra::aggregate(3)
  },
  error=function(cond) {
    return(NA)
  })
  
  return(clipped)
}


#mosaic rasters
clipped_rasters <- lapply(raster_list, clip_raster)
clipped_rasters <- clipped_rasters[!is.na(clipped_rasters)]
srtm_stack <- terra::mosaic(sprc(clipped_rasters))

dem <- terra::crop(srtm_stack, vect(region)) %>% terra::mask(vect(region))
dem <- terra::project(dem, "EPSG:5070")
plot(dem)

writeRaster(dem, "./Parameterization/Parameterization data/topography/midwest_dem.tif", overwrite = TRUE)

dem <- rast("./Parameterization/Parameterization data/topography/midwest_dem.tif")

slope <- terra::terrain(dem, v = "slope", unit = "degrees") #%>%
# raster::projectRaster(crs = "+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")
writeRaster(slope, "./Parameterization/Parameterization data/topography/midwest_slope.tif", overwrite = TRUE,
            datatype = "INT2S")
rm(slope)

aspect <- terra::terrain(dem, v = "aspect", unit = "degrees") #%>%
#  raster::projectRaster(crs = "+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")
writeRaster(aspect, "./Parameterization/Parameterization data/topography/midwest_aspect.tif", overwrite = TRUE,
            datatype = "INT2S")
rm(aspect)
rm(dem)
