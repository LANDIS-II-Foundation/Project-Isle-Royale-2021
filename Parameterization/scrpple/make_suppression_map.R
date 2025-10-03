library("terra")
library("sf")
library("spatstat")
library("tidyverse")
sf::sf_use_s2(TRUE)

# raster to get bounding box from
mask <- terra::rast("./Models/LANDIS inputs/input rasters/ecoregions_inv.tif")
crs(mask)
poly_bound <- sf::st_read("./Parameterization/Parameterization data/isle_royale_boundary_buffer/isle_royale_boundary_buffer.shp")%>%
  sf::st_union()%>%
  sf::st_transform(crs = crs(mask)) %>%
  st_as_sf()

#geomac data are in EPSG:4269 or EPSG:4326
region <- sf::st_read("D:/Data/epa_ecoregions/us_eco_l3/us_eco_l3.shp") %>%
  filter(US_L3CODE %in% c(50,51)) %>%
  sf::st_union() %>%
  sf::st_transform(crs = crs(mask))


#-------------------------------------------------------------------------------
#import location data
locs <- sf::st_read("./Parameterization/Parameterization data/NPS_POI/poi.gpkg")
locs <- locs %>%
  sf::st_transform(crs = crs(mask)) %>%
  st_intersection(poly_bound, join = st_within)
plot(poly_bound)
plot(locs, add = TRUE)

locs$buffer <- ifelse(locs$POITYPE %in% c("Office", "Visitor Center", "Food Service"), 2000,
                      ifelse(locs$POITYPE %in% c("Boat Launch", "Campground"), 1000, 0))
locs$priority_buffer <- ifelse(locs$POITYPE %in% c("Office", "Visitor Center", "Food Service"), 1000, 0)

bldgs <-  sf::st_read("./Parameterization/Parameterization data/Facility.gdb", layer = "ISRO_FACILITY_Buildings")%>%
  sf::st_transform(crs = crs(mask)) %>%
  st_intersection(poly_bound, join = st_within) %>%
  mutate(buffer = 2000,
         priority_buffer = 1000)
st_geometry(bldgs) <- "geom"

locs <- bind_rows(locs, bldgs)

protect <- st_buffer(locs, locs$buffer) %>% st_union()
plot(mask)
plot(protect, add = TRUE)

priority_protect <- st_buffer(locs, locs$priority_buffer) %>% st_union()
plot(priority_protect, add = TRUE)

protect_rast <- terra::rasterize(vect(protect), mask, values = 1, background = 0) %>% crop(mask)
plot(protect_rast)

priority_rast <- terra::rasterize(vect(priority_protect), mask, values = 1, background = 0) %>% crop(mask)

zone_rast <- sum(protect_rast, priority_rast)
plot(zone_rast)

writeRaster(zone_rast, "suppression_zones.tif")
