# create stack of annual mtbs fire severity mosaic

library("terra")
library("sf")
library("tidyverse")


#get all the mtbs data to use for fire severity => combustion buoyancy

# first, unzip all of the annual MTBS mosaics -- there is a .bat file in the directory, but if
# you need it, just make a batch file in the main directory that contains all years' zip files,
# and run this script that will recursively unzip all zip files in the directory. 
# see https://serverfault.com/questions/8092/how-do-i-extract-all-archives-in-the-subdirectories-of-this-folder
#
# FOR /D /r %%F in ("*") DO (
# pushd %CD%
#   cd %%F
# FOR %%X in (*.rar *.zip) DO (
#   "C:\Program Files\7-zip\7z.exe" x "%%X"
# )
# popd
# )


region <- sf::st_read("D:/Data/epa_ecoregions/us_eco_l3/us_eco_l3.shp") %>%
  filter(US_L3CODE %in% c(50,51)) %>%
  sf::st_union()%>%
  sf::st_transform("EPSG:5070") %>%
  sf::st_as_sf()
region_wgs <- region %>%
  sf::st_transform(crs = "+proj=longlat +datum=WGS84 +no_defs")


template <- rast("./Models/LANDIS inputs/input rasters/ecoregions_inv.tif")
terra::values(template) <- 1

#make a template for the whole sierra
region_template <- terra::project(template, )
ext(region_template) <- ext(region)
res(region_template) <- res(template)
values(region_template) <- 1

mask <- terra::rast("./Models/LANDIS inputs/input rasters/ecoregions_inv.tif")
crs(mask)
poly_bound <- sf::st_read("./Parameterization/Parameterization data/isle_royale_boundary_buffer/isle_royale_boundary_buffer.shp")%>%
  sf::st_union()%>%
  sf::st_transform(crs = crs(mask))



#where are the files? Path to all .tifs
mtbs_files <- paste0("D:/Data/mtbs_isro/MTBS_BSmosaics/", 2000:2022, "/mtbs_CONUS_", 2000:2022, ".tif")

#import all mtbs years
# they can't be raster::stacked because the extents are inexplicably different for each year
mtbs <- lapply(mtbs_files, rast)

#reproject vector to match raster
region_poly_reproject <- sf::st_transform(region, crs = crs(mtbs[[1]]))


# crop rasters to study area and coarsen resolution to match project files
# this is slow, maybe could be optimized
mtbs2 <- mtbs %>%
  purrr::map( ~ terra::crop(.x, region_poly_reproject)) %>%
  purrr::map( ~ terra::aggregate(.x, fact = 2, fun = mean, na.rm = TRUE))

mtbs_reprojected <- mtbs2[[17]] %>%
  purrr::map( ~ terra::project(.x, region_template))



writeRaster(mtbs, "D:/Data/mtbs_isro/MTBS_BSmosaics/combined_mosaic.tif", overwrite = TRUE)
