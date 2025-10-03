#assess individual fires
library(terra)
library(tidyverse)

ecoregions <- terra::rast("./Models/LANDIS inputs/input rasters/ecoregions_inv.tif")

scenario_folder <- "./Models/v2 model templates/"
scenarios <- list.dirs(scenario_folder, recursive = FALSE) #%>%
# `[`(grep("Scenario", .))
scenarios <- scenarios[6]

fire_events <- paste0(scenarios, "/scrapple-events-log.csv")  %>%
  purrr::map_df(~read_plus(.))

year <- fire_events[which(fire_events$TotalSitesBurned == max(fire_events$TotalSitesBurned)), "SimulationYear"]

fire_damage <- rast(paste0(scenarios, "/social-climate-fire/biomass-mortality-", year, ".img"))
plot(fire_damage)
fire_damage_project <- ecoregions
fire_damage_project[] <- fire_damage[]                    
fire_damage_project[fire_damage_project[] == 0] <- NA
plot(vect(poly_bound))
plot(fire_damage_project, add = TRUE)

cropper <- rast(xmin = 0, xmax = 800, ymin = 560, ymax = 1000)
fire_damage <- crop(fire_damage, cropper)
burned_cells <- xyFromCell(fire_damage, which(fire_damage[] > 1))
xrange <- range(burned_cells[, 1])
yrange <- range(burned_cells[, 2])

cropper <- rast(xmin = xrange[1] - 1, xmax = xrange[2] +1, ymin = yrange[1] - 1, ymax = yrange[2] + 1)
subsample <- crop(fire_damage, cropper)
plot(subsample)

bio_pre_fire <- rast(paste0(scenarios, "/biomass/TotalBiomass-10.img")) %>%
  crop(cropper)
plot(bio_pre_fire, range = c(0, 25000))

bio_post_fire <- rast(paste0(scenarios, "/biomass/TotalBiomass-40.img")) %>%
  crop(cropper)
plot(bio_post_fire, range = c(0, 25000))


