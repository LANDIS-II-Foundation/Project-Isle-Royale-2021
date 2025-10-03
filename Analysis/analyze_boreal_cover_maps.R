library(terra)
library(tidyverse)
library(tidyterra)
library("colorspace")

source("./Analysis/r_functions.R")
diverging_color_ramp <- function(ras){
  the_palette_fc <- leaflet::colorNumeric(palette = "RdBu", 
                                          domain = c(-max(abs(ras[]), na.rm = TRUE), max(abs(ras[]), na.rm = TRUE)),
                                          reverse = FALSE)
  the_colors <- the_palette_fc(seq(min(ras[], na.rm = TRUE), max(ras[], na.rm = TRUE), length.out = 50))
  
}

theme_set(theme_bw())
theme_update(panel.grid.minor = element_blank(),
             strip.background = element_rect(fill = "white"))

scenario_folder <- "D:/ISRO landis/"
# scenario_folder <- "./Models/v2 model templates/"
# scenarios <- list.dirs(scenario_folder, recursive = FALSE) 
scenarios <- list.dirs(scenario_folder, recursive = FALSE) %>%
  `[`(!grepl("no_suppression", .))
# scenarios <- scenarios[c(4:9, 16:21, 28:33)]

func_types <- data.frame(FunctionalGroupIndex = seq(1, 10),
                         Type = c("Northern hardwood/conifer",
                                  "Boreal conifer",
                                  "Temperate hardwood/conifer",
                                  "Boreal conifer",
                                  "Boreal conifer",
                                  "Northern hardwood/conifer",
                                  "Boreal hardwood",
                                  "Temperate hardwood/conifer",
                                  "Shrubs",
                                  "Shrubs"))
spp_table <- read.csv("./Models/LANDIS inputs/NECN files/NECN_Spp_Table_inv.csv") %>%
  left_join(func_types)
spp_table[spp_table$SpeciesCode == "BEPA", "Type"] <- "Boreal hardwood"
spp_table[spp_table$SpeciesCode == "FRNI", "Type"] <- "Boreal hardwood"
# spp_table[spp_table$SpeciesCode == "ABBA", "Type"] <- "Balsam fir"


get_mgmt <- function(scenario){
  list.files(scenario, pattern = "Scenario") %>%
    pluck(1) %>%
    as.character() %>%
    strsplit(x = ., split = "[.]") %>%
    pluck(1, 1) %>%
    strsplit(x = ., split = "[_]") %>%
    pluck(1, 1)
}

get_climate <- function(scenario){
  list.files(scenario, pattern = "NECN_Succession") %>%
    pluck(1) %>%
    as.character() %>%
    strsplit(x = ., split = "[.]") %>%
    pluck(1, 1)
}

scenario_type <- data.frame(run_name = character(length(scenarios)), 
                            mgmt = character(length(scenarios)),
                            climate = character(length(scenarios)))

scenario_type <- scenario_type %>%
  mutate(run_name = unlist(map(strsplit(scenarios, split = "/"), pluck(4, 1)))) %>%
  # mutate(mgmt = unlist(map(scenarios, get_mgmt))) %>%
  mutate(browse = ifelse(grepl(pattern = "no pred", run_name), "Low", 
                         ifelse(grepl(pattern = "low pred", run_name), "Medium",
                                "High"))) %>%
  mutate(climate = ifelse(grepl(pattern = "CANESM", run_name), "Hot/Dry (CanESM2 8.5)",
                                 ifelse(grepl(pattern = "GFDL", run_name), "Warm (GFDL 4.5)", 
                                        ifelse(grepl(pattern = "MRI", run_name), "Hot/Wet (MRI-CGCM3 8.5)", "Present Climate")))) %>%
  mutate(browse = factor(browse, levels = c("Low", "Medium", "High"))) %>%
  mutate(fire_model = ifelse(grepl(pattern = "no_suppression", run_name), "No Suppression", "BAU"))
         

template <- rast("./Models/LANDIS inputs/input rasters/ecoregions.tif")
isro_poly <- st_read("./Parameterization/Parameterization data/ir_polygon/isro_boundary_tight.gpkg") %>%
  st_transform(crs(template))

lai_maps <- paste0("D:/ISRO landis/", scenario_type$run_name, "/NECN/LAI-40.img")
lai_init <- paste0("D:/ISRO landis/", scenario_type$run_name[[1]], "/NECN/LAI-5.img")

comm_map_init <- paste0("D:/ISRO landis/", scenario_type$run_name[1], "/output-community-0.img")
comm_mat_init <- paste0("D:/ISRO landis/", scenario_type$run_name[1], "/community-input-file-0.csv") %>% 
  read_csv() %>%
  group_by(MapCode, SpeciesName) %>%
  summarize(Biomass = sum(CohortBiomass), .groups = "drop") %>%
  left_join(dplyr::select(spp_table, SpeciesCode, Type), 
            by = c("SpeciesName" = "SpeciesCode"))

comm_maps <- paste0("D:/ISRO landis/", scenario_type$run_name, "/output-community-40.img")
comm_cohorts <- paste0("D:/ISRO landis/", scenario_type$run_name, "/community-input-file-40.csv")

comm_cohorts_biomass <- comm_cohorts %>%
  purrr::map(., .f = read_csv) 

comm2 <- comm_cohorts_biomass %>%
  purrr:::map(., .f = function(x) {x %>% 
                                   group_by(MapCode, SpeciesName) %>%
                                   summarize(Biomass = sum(CohortBiomass), .groups = "drop")})

comm_mat_ft <- comm2 %>%
  purrr::map(.f = function(x) {left_join(x, dplyr::select(spp_table, SpeciesCode, Type), 
                                         by = c("SpeciesName" = "SpeciesCode")) %>%
                                group_by(MapCode, Type) %>%
                                summarise(Biomass = sum(Biomass))%>% 
                                slice(which.max(Biomass))})

comm_mat_boreal_juvenile <- comm_cohorts_biomass %>%
  purrr::map(.f = function(x) {left_join(x, dplyr::select(spp_table, SpeciesCode, Type), 
                                         by = c("SpeciesName" = "SpeciesCode")) %>%
      filter(Type %in% c("Boreal conifer", "Boreal hardwood"),
             CohortAge <= 11) %>%
      group_by(MapCode, Type) %>%
      summarise(Biomass = sum(CohortBiomass))%>% 
      slice(which.max(Biomass))})

comm_mat_tree_bio <- comm2 %>%
  purrr::map(.f = function(x) {left_join(x, dplyr::select(spp_table, SpeciesCode, Type), 
                                         by = c("SpeciesName" = "SpeciesCode")) %>%
      filter(Type %in% c("Temperate hardwood/conifer", "Northern hardwood/conifer")) %>%
      group_by(MapCode) %>%
      summarise(Biomass = sum(Biomass))})

veg_types = data.frame(value=c(1:5), desc=levels(as.factor(comm_mat_ft[[1]]$Type)))

classify_type <- function(comm_mat, comm_map){
  comm_mat$MapLabel <- veg_types[match(comm_mat$Type, veg_types$desc), "value"]
  map2 <- rast(comm_map)
  vals <- comm_mat[match(map2[], comm_mat$MapCode), "MapLabel"]
  values(map2) <- vals
  values(map2)[is.na(values(map2))] <- 0
  return(map2)
  # plot(map2)
}

classify_boreal <- function(comm_mat, comm_map){
  comm_mat$MapLabel <- as.integer(comm_mat$Type %in% c("Boreal conifer", "Boreal hardwood"))
  map2 <- rast(comm_map)
  vals <- comm_mat[match(map2[], comm_mat$MapCode), "MapLabel"]
  values(map2) <- vals
  values(map2)[is.na(values(map2))] <- 0
  return(map2)
  plot(map2)
}

classify_balsam <- function(comm_mat, comm_map){
  comm_mat <- filter(comm_mat, SpeciesName == "ABBA" & Biomass > 1000)
  map2 <- rast(comm_map)
  vals <- as.integer(map2[] %in% comm_mat$MapCode)
  values(map2) <- vals
  values(map2)[is.na(values(map2))] <- 0
  return(map2)
  plot(map2)
}

classify_boreal_understory <- function(comm_mat_juv, comm_map){
  comm_mat_juv$MapLabel <- as.integer(comm_mat_juv$Type %in% c("Boreal conifer", "Boreal hardwood"))
  map2 <- rast(comm_map)
  vals <- comm_mat_juv[match(map2[], comm_mat_juv$MapCode), "MapLabel"]
  values(map2) <- vals
  values(map2)[is.na(values(map2))] <- 0
  return(map2)
  # plot(map2)
}

classify_forest <- function(comm_tree, comm_map){
  comm_tree$MapLabel <- as.integer(comm_tree$Biomass > 500)
  map2 <- rast(comm_map)
  vals <- comm_tree[match(map2[], comm_tree$MapCode), "MapLabel"]
  values(map2) <- vals
  values(map2)[is.na(values(map2))] <- 0
  return(map2)
  # plot(map2)
}

classify_tree_biomass <- function(comm_tree, comm_map){
  map2 <- rast(comm_map)
  vals <- comm_tree[match(map2[], comm_tree$MapCode), "Biomass"]
  values(map2) <- vals
  values(map2)[is.na(values(map2))] <- 0
  return(map2)
  # plot(map2)
}

classify_forest_by_lai <- function(lai_map){
  map2 <- rast(lai_map)
  map2[] <- as.integer(map2[] >= 3)
  return(map2)
  # plot(map2)
}

boreal_initial <- classify_boreal(comm_mat_init, comm_map_init) %>%
  mask(rast(lai_init) > 3)
balsam_init <- classify_balsam(comm_mat_init, comm_map_init) %>%
  mask(rast(lai_init) > 3)
tree_bio_init <- comm_mat_init %>%
  # filter(Type != "Shrubs") %>%
  # filter(Type %in% c("Boreal hardwood", "Boreal conifer")) %>%
  filter(Type %in% c("Temperate hardwood/conifer", "Northern hardwood/conifer")) %>%
  group_by(MapCode) %>%
  summarise(Biomass = sum(Biomass)) %>%
  classify_tree_biomass(comm_map_init)
forest_lai_map_init <- classify_forest_by_lai(lai_init)
  

ft_maps <- purrr:::map2(.x = comm_mat_ft, 
                        .y = comm_maps, 
                        .f = ~ classify_type(.x, .y))

boreal_maps <- purrr:::map2(.x = comm_mat_ft, 
                            .y = comm_maps, 
                            .f = ~ classify_boreal(.x, .y))
boreal_understory_maps <- purrr:::map2(.x = comm_mat_boreal_juvenile, 
                            .y = comm_maps, 
                            .f = ~ classify_boreal_understory(.x, .y))
forest_maps <- purrr:::map2(.x = comm_mat_tree_bio, 
                                       .y = comm_maps, 
                                       .f = ~ classify_forest(.x, .y))
forest_lai_maps <- purrr::map(.x = lai_maps,
                              .f = ~classify_forest_by_lai(.x))
balsam_maps <- purrr::map2(.x = comm2,
                          .y = comm_maps,
                          .f = ~classify_balsam(.x, .y))
biomass_maps <- purrr::map2(.x = comm_mat_tree_bio,
                            .y = comm_maps,
                            .f = ~classify_tree_biomass(.x, .y))

boreal_two_class <- purrr::pmap(.l = list(boreal_maps, boreal_understory_maps, forest_lai_maps),
                                .f = function(boreal, under, forest){
                                  boreal <- mask(boreal, forest)
                                  both <- mask(under, boreal)
                                  
                                  return(boreal + both)
                                  })
boreal_masked <- purrr::pmap(.l = list(boreal_maps, forest_lai_maps),
                             .f = function(boreal, forest){
                               boreal <- mask(boreal, forest)
                               return(boreal)
                             })
balsam_masked <- purrr::pmap(.l = list(balsam_maps, forest_lai_maps),
                             .f = function(boreal, forest){
                               boreal <- mask(boreal, forest)
                               return(boreal)
                             })

boreal_under_masked <- purrr::pmap(.l = list(boreal_understory_maps, forest_lai_maps),
                                   .f = function(boreal, forest){
                                     boreal <- mask(boreal, forest)
                                     return(boreal)
                                   })

plot(boreal_two_class[[12]], col = c("white", "#addd8e", "#31a354"))

#-------------------------------
# merge by category
scens <- expand.grid( unique(scenario_type$climate), c("Low", "Medium", "High"))

# scens <- expand.grid( unique(scenario_type$climate), c("BAU", "No Suppression"))

svg(filename = "boreal_change_fire.svg",
    width = 8,
    height = 5, 
    pointsize = 12)

  # layout(mat = matrix(c(1,2,3,10,
  #                       4,5,6,10,
  #                       7,8,9,10),
  #                     byrow = TRUE, ncol = 4),
  #        widths = c(1,1,1,0.2,
  #                   1,1,1,0.2,
  #                   1,1,1,0.2))

layout(mat = matrix(c(1,2,3,7,
                      4,5,6,7),
                    byrow = TRUE, ncol = 4),
       widths = c(1,1,1,0.2,
                  1,1,1,0.2,
                  1,1,1,0.2))
  par(oma = c(1, 5.5, 4.5, 2))
  
  for(i in 1:6){
    boreal_sub <- boreal_masked[which(scenario_type$climate == scens$Var1[i] & 
                                          scenario_type$fire_model == scens$Var2[i])]
    
    overstory <- template
    values(overstory) <- (rast(boreal_sub) - boreal_initial) %>%
      mean() %>%
      values()
    values(overstory)[values(template) == 0] <- NA
    
    overstory <- crop(overstory, vect(isro_poly)) %>% mask(vect(isro_poly))
    plot(overstory, 
         mar = c(1,1,1,1),
         legend = FALSE,
         col = diverging_color_ramp(overstory))
    lines(vect(isro_poly))
  }
  
dev.off()


#--------------
#balsam fir change
scens <- expand.grid( unique(scenario_type$climate), c("Low", "Medium", "High"))

svg(filename = "balsam_change.svg",
    width = 8,
    height = 5, 
    pointsize = 12)

layout(mat = matrix(c(1,2,3,10,
                      4,5,6,10,
                      7,8,9,10),
                    byrow = TRUE, ncol = 4),
       widths = c(1,1,1,0.2,
                  1,1,1,0.2,
                  1,1,1,0.2))
par(oma = c(1, 5.5, 4.5, 2))
for(i in 1:9){
  balsam_sub <- balsam_masked[which(scenario_type$climate == scens$Var1[i] & 
                                      scenario_type$browse == scens$Var2[i])]
  
  overstory <- template
  values(overstory) <- (rast(balsam_sub) - balsam_init) %>%
    mean() %>%
    values()
  values(overstory)[values(template) == 0] <- NA
  
  overstory <- crop(overstory, vect(isro_poly)) %>% mask(vect(isro_poly))
  plot(overstory, 
       mar = c(1,1,1,1),
       legend = FALSE,
       col = diverging_color_ramp(overstory))
  lines(vect(isro_poly))
}

dev.off()


#--------------
#biomass maps
scens <- expand.grid( unique(scenario_type$climate), c("Low", "Medium", "High"))

svg(filename = "boreal_biomass_change_maps.svg",
    width = 8,
    height = 5, 
    pointsize = 12)

layout(mat = matrix(c(1,2,3,10,
                      4,5,6,10,
                      7,8,9,10),
                    byrow = TRUE, ncol = 4),
       widths = c(1,1,1,0.2,
                  1,1,1,0.2,
                  1,1,1,0.2))
par(oma = c(1, 5.5, 4.5, 2))

the_palette_fc <- leaflet::colorNumeric(palette = "RdBu", 
                                        domain = c(-10000, 10000),
                                        reverse = FALSE)
the_colors <- the_palette_fc(seq(-10000, 10000, length.out = 50))

for(i in 1:9){
  biomass_sub <- biomass_maps[which(scenario_type$climate == scens$Var1[i] & 
                                      scenario_type$browse == scens$Var2[i])]
  
  overstory <- template
  values(overstory) <- (rast(biomass_sub) - tree_bio_init) %>%
    mean() %>%
    values()
  values(overstory)[values(template) == 0] <- NA

  
  overstory <- crop(overstory, vect(isro_poly)) %>% mask(vect(isro_poly))

  plot(overstory/100, 
       # mar = c(1,1,1,1),
       # legend = FALSE,
       range = c(-100, 100),
       col = the_colors)
  lines(vect(isro_poly))
}

dev.off()

#--------------
#biomass maps
scens <- expand.grid( unique(scenario_type$climate), c("Low", "Medium", "High"))

svg(filename = "temperate_biomass_change_maps.svg",
    width = 8,
    height = 5, 
    pointsize = 12)

layout(mat = matrix(c(1,2,3,10,
                      4,5,6,10,
                      7,8,9,10),
                    byrow = TRUE, ncol = 4),
       widths = c(1,1,1,0.2,
                  1,1,1,0.2,
                  1,1,1,0.2))
par(oma = c(1, 5.5, 4.5, 2))

the_palette_fc <- leaflet::colorNumeric(palette = "RdBu", 
                                        domain = c(-10000, 10000),
                                        reverse = FALSE)
the_colors <- the_palette_fc(seq(-10000, 10000, length.out = 50))

for(i in 1:9){
  biomass_sub <- biomass_maps[which(scenario_type$climate == scens$Var1[i] & 
                                      scenario_type$browse == scens$Var2[i])]
  
  overstory <- template
  values(overstory) <- (rast(biomass_sub) - tree_bio_init) %>%
    mean() %>%
    values()
  values(overstory)[values(template) == 0] <- NA
  
  
  overstory <- crop(overstory, vect(isro_poly)) %>% mask(vect(isro_poly))
  
  plot(overstory/100, 
       mar = c(1,1,1,1),
       legend = FALSE,
       range = c(-100, 100),
       col = the_colors)
  lines(vect(isro_poly))
}

dev.off()



#----------------------
#hange from forest to nonforst

svg(filename = "forest_cover_change_lai.svg",
    width = 8,
    height = 5, 
    pointsize = 12)

layout(mat = matrix(c(1,2,3,10,
                      4,5,6,10,
                      7,8,9,10),
                    byrow = TRUE, ncol = 4),
       widths = c(1,1,1,0.2,
                  1,1,1,0.2,
                  1,1,1,0.2))
par(oma = c(1, 5.5, 4.5, 2))

for(i in 1:9){
  lai_sub <- forest_lai_maps[which(scenario_type$climate == scens$Var1[i] & 
                                      scenario_type$browse == scens$Var2[i])]
  
  overstory <- template
  values(overstory) <- (rast(lai_sub) - forest_lai_map_init) %>%
    mean() %>%
    values()
  values(overstory)[values(template) == 0] <- NA
  
  overstory <- crop(overstory, vect(isro_poly)) %>% mask(vect(isro_poly))
  plot(overstory, 
       mar = c(1,1,1,1),
       legend = FALSE,
       col = diverging_color_ramp(overstory))
  lines(vect(isro_poly))
}

dev.off()

