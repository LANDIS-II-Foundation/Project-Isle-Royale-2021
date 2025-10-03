# process SCRPPLE outputs and create visualizations for presentation

library("tidyverse")
library("sf")
library("vioplot")
library("terra")
options(warn = 1)


ecoregions <- terra::rast("./Models/LANDIS inputs/input rasters/ecoregions_inv.tif")
ecoregion_size <- table(values(ecoregions))

poly_bound <- sf::st_read("./Parameterization/Parameterization data/ir_polygon/isro_boundary_tight.gpkg")%>%
  sf::st_union()%>%
  sf::st_transform(crs = crs(ecoregions))

#geomac data are in EPSG:4269 or EPSG:4326
region <- sf::st_read("D:/Data/epa_ecoregions/us_eco_l3/us_eco_l3.shp") %>%
  filter(US_L3CODE %in% c(50,51)) %>%
  sf::st_union()%>%
  sf::st_transform(crs = crs(ecoregions))


#-------------------------------------------------------
#import fire occurrence records

#short dataset already extracted for epa regions 50 and 51
short_subset <- sf::st_read("./Parameterization/Parameterization data/short/short_region.gpkg")%>%
  # filter(FIRE_SIZE >= 0.6) %>%
  sf::st_transform(crs = crs(ecoregions))

#Fires per year
hist(short_subset$FIRE_YEAR)
short_subset_by_year <- short_subset %>%
  sf::st_drop_geometry() %>%
  mutate(FIRE_YEAR = factor(FIRE_YEAR, 1992:2018)) %>% #workaround to make years with no fire have area burned = 0
  group_by(FIRE_YEAR) %>%
  summarize(n = n(),
            total = sum(FIRE_SIZE/2.47),
            density = n / units::set_units(st_area(region), "hectare"),
            proportion = total / units::set_units(st_area(region), "hectare"))
hist(short_subset[short_subset$NWCG_CAUSE_CLASSIFICATION == "Natural" &
                    short_subset$FIRE_SIZE > 100, ]$FIRE_SIZE)
plot(short_subset_by_year$proportion ~ short_subset_by_year$FIRE_YEAR)

short_isro <- short_subset %>%
  sf::st_intersection(poly_bound) %>%
  sf::st_drop_geometry()
short_isro_by_year <- short_subset %>%
  sf::st_intersection(poly_bound) %>%
  sf::st_drop_geometry() %>%
  mutate(FIRE_YEAR = factor(FIRE_YEAR, 1992:2018)) %>% #workaround to make years with no fire have area burned = 0
  group_by(FIRE_YEAR) %>%
  summarize(n = n(),
            total = sum(FIRE_SIZE/2.47),
            density = n / (units::drop_units(st_area(poly_bound))/10000),
            proportion = total / (units::drop_units(st_area(poly_bound))/10000))

#NIFC InFORM Fire Occurrence Data Records
isro_fire_records_all <- sf::st_read("./Parameterization/Parameterization data/fire_history/InFORM_FireOccurrence_Public_-3936383734279734000/Incidents.shp") %>%
  # mutate(FIRE_YEAR = factor(CalendarYe, 1992:2022)) %>% #workaround to make years with no fire have area burned = 0
  mutate(FIRE_YEAR = CalendarYe,
         FIRE_SIZE = IncidentSi/2.47)%>%
  st_drop_geometry()

isro_fire_records <- isro_fire_records_all %>%
  group_by(FIRE_YEAR) %>%
  summarize(n = n(),
            total = sum(IncidentSi/2.47),
            density = n / (units::drop_units(st_area(poly_bound))/10000),
            proportion = total / (units::drop_units(st_area(poly_bound))/10000)) 

plot(isro_fire_records$total ~ isro_fire_records$FIRE_YEAR)

#records from NPS at ISRO
isro_historical_records_all <- read.csv("./Parameterization/Parameterization data/fire_history/nri-ISROFireHistory1930-87.csv") %>%
  `[`(-c(1:34), ) %>%
  dplyr::mutate(Size.in.Acres = gsub(",", "", Size.in.Acres),
                FIRE_SIZE = as.numeric(ifelse(Size.in.Acres == "<1", 0.5, Size.in.Acres))/2.47,
                FIRE_YEAR = as.numeric(lubridate::year(base::as.Date(Date, tryFormats = c("%m/%d/%Y")))))

isro_historical_records <- isro_historical_records_all %>%
  group_by(FIRE_YEAR) %>%
  summarize(n = n(),
            total = sum(FIRE_SIZE)/2.47,
            density = n / (units::drop_units(st_area(poly_bound))/10000),
            proportion = total / (units::drop_units(st_area(poly_bound))/10000))

isro_combined <- rbind(isro_fire_records, isro_historical_records) %>%
  mutate(FIRE_YEAR = factor(FIRE_YEAR, levels = as.character(c(1936:2023)))) %>%
  tidyr::complete(FIRE_YEAR, fill = list(n = 0, 
                                         total = 0, 
                                         density = 0,
                                         proportion = 0)) %>%
  mutate(FIRE_YEAR = as.numeric(as.character(FIRE_YEAR)))

isro_combined_all <- rbind(select(isro_fire_records_all, FIRE_YEAR, FIRE_SIZE),
                           select(isro_historical_records_all, FIRE_YEAR, FIRE_SIZE))

plot(isro_combined$proportion ~ isro_combined$FIRE_YEAR,
     xlab = "Year",
     ylab = "Proportion burned")

1/sum(isro_combined[isro_combined$FIRE_YEAR > 1936, ]$proportion / (max(isro_combined[isro_combined$FIRE_YEAR > 1936, ]$FIRE_YEAR) - min(isro_combined[isro_combined$FIRE_YEAR > 1936, ]$FIRE_YEAR)))
#MFRI is a little longer than expected -- ~430 years


#---------------------------------------------------------------------------
# Import observed MTBS fire severity records
fire_severity_data <- list.files("./Parameterization/Parameterization data/fire severity/",
                                 pattern = "catcher",
                                 full.names = TRUE)

col_types <- list(
  fire_code = col_character(),
  fire_name = col_character(),
  mtbs_filename = col_character(),
  year = col_integer(),
  dnbr = col_double(),
  rdnbr = col_double(),
  mtbs_severity = col_integer()
)


mtbs_dnbr <- fire_severity_data %>%
  purrr::map_df(~read_csv(., col_types = col_types) %>%
                  group_by(fire_name)) %>%
  dplyr::filter(dnbr > 0) 
mtbs_dnbr_year <- mtbs_dnbr %>%
  group_by(year) %>%
  summarise(area_burned = n()*30*30/10000,
            prop_high_severity = sum(mtbs_severity == 4)/n(),
            prop_medium_severity = sum(mtbs_severity == 3)/n(),
            prop_low_severity = sum(mtbs_severity == 2)/n(),
            mean_dnbr = mean(dnbr),
            median_dnbr = median(dnbr))
plot(mtbs_dnbr_year$mean_dnbr ~ mtbs_dnbr_year$year)
plot(mtbs_dnbr_year$prop_high_severity ~ mtbs_dnbr_year$year)
plot(mtbs_dnbr_year$prop_high_severity ~ mtbs_dnbr_year$area_burned)

hist(mtbs_dnbr$dnbr)
sum(mtbs_dnbr$dnbr > 300, na.rm = TRUE)/nrow(mtbs_dnbr) 
sum(mtbs_dnbr$dnbr > 400, na.rm = TRUE)/nrow(mtbs_dnbr) 

horne_fire <- sf::st_read("./Parameterization/Parameterization data/fire_history/ISRO_Post_HorneFire/ISRO_Post_HorneFire.shp")
horne_fire_prop <- horne_fire %>%
  sf::st_drop_geometry() %>%
  group_by(Burn_Index) %>%
  summarise(sum = sum(Hectares)) %>%
  ungroup() %>%
  filter(Burn_Index != "Unburned") %>%
  mutate(proportion = sum / sum(sum))
  

#-------------------------------------------------------------------------------
# Import SCRPPLE data
#-------------------------------------------------------------------------------
# This chunk of code is designed to run directly on the folder of LANDIS model runs
# and requires additional files like the scenario.txt file to grab some information.
# It depends on the naming convention of the files used to extract management and climate
# information.

maxyear <- 40
#what folder do all the runs to be analyze live in?

scenario_folder <- "D:/ISRO landis/"
# scenario_folder <- "./Models/v2 model templates/"
# scenarios <- list.dirs(scenario_folder, recursive = FALSE) 
scenarios <- list.dirs(scenario_folder, recursive = FALSE)# %>%
# `[`(!grepl("no_suppression", .))
scenarios <- scenarios[c(4:9, 16:21, 28:33)]


#some helper functions
read_plus <- function(flnm) {
  read_csv(flnm, show_col_types = FALSE) %>% 
    mutate(filename = as.character(flnm),
           run_name = basename(substr(flnm, 0, regexpr("/[^/]*$", flnm)))) 
  
}

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


#set scenarios manually if needed
# scenario_type$mgmt <- c(rep("Scenario1", 5), "Scenario10", "Scenario7", "Scenario8", "Scenario9")


fire_summaries <- paste0(scenarios, "/scrapple-summary-log.csv")  %>%
  purrr::map_df(~read_plus(.)) %>%
  left_join(scenario_type, c("run_name" = "run_name")) %>%
  dplyr::filter(SimulationYear <= maxyear) 

#----------------------

fire_summaries$TotalBurnedSites <- fire_summaries$TotalBurnedSitesAccidental + 
  fire_summaries$TotalBurnedSitesLightning + 
  fire_summaries$TotalBurnedSitesRx
fire_summaries$TotalFires <- fire_summaries$NumberFiresAccidental + 
  fire_summaries$NumberFiresLightning + 
  fire_summaries$NumberFiresRx
fire_summaries$per_fire_ha <- (fire_summaries$TotalBurnedSites / fire_summaries$TotalFires) * 180 * 180 / 10000 #average area burned per fire by year, in ha
plot(fire_summaries$per_fire_ha ~ fire_summaries$SimulationYear)

#------------------------------------
#SCRPPLE events
fire_events <- paste0(scenarios, "/scrapple-events-log.csv")  %>%
  purrr::map_df(~read_plus(.)) %>%
  left_join(scenario_type, c("run_name" = "run_name"))%>%
  dplyr::filter(SimulationYear <= maxyear) # %>%
#dplyr::filter(TotalSitesBurned > (1000/8)) #match MTBS criteria


#-------------------------------------------------------------------------------
# process fire rasters
# this can take a long time

get_burn_intensity <- function(raster, intensity){
  return(sum(terra::values(raster) >= intensity))
}

years <- 1:maxyear
#need to summarize fire data to 5-year chunks to compare with NECN data
# year_bins <- cut(years, breaks = seq(0,20, by = 5))

intensity_paths <- paste0(rep(paste0(scenarios, "/social-climate-fire/"), each = length(years)), "/fire-intensity-", years, ".img")

high_intensity_cells <- NA
for(i in 1:length(intensity_paths)){
  #TODO remake this a purrr::map workflow
  high_intensity_cells[i] <- terra::rast(intensity_paths[i]) %>%
    get_burn_intensity(., 6)
}

fire_summaries$TotalSitesHighIntensity <- high_intensity_cells


get_burn_intensity <- function(raster, intensity){
  return(sum(terra::values(raster) >= intensity))
}


## to make histogram of DNB individual cells; just for testing
years <- 1:maxyear
#need to summarize fire data to 5-year chunks to compare with NECN data
# year_bins <- cut(years, breaks = seq(0,20, by = 5))
# scenarios2 <- scenarios[4]
dnbr_paths <- paste0(rep(paste0(scenarios, "/social-climate-fire/"), each = length(years)), "/fire-dnbr-", years, ".img")

# dnbr_paths <- paste0("E:/TCSI LANDIS/LANDIS runs/Scenario1 - historical - Run 4/social-climate-fire/fire-dnbr-", years, ".img")


dnbr_cells <- NA
for(i in 1:length(dnbr_paths)){
  #TODO remake this a purrr::map workflow
  dnbr_rast <- terra::rast(dnbr_paths[i])
  # plot(dnbr_rast)
  dnbr_cells_temp <- dnbr_rast[]
  dnbr_cells <- c(dnbr_cells, dnbr_cells_temp[dnbr_cells_temp > 1])
}

hist(dnbr_cells)
mean(dnbr_cells, na.rm = TRUE) #target is 382
median(dnbr_cells, na.rm = TRUE) #target is 330
sum(dnbr_cells > 300, na.rm = TRUE)/length(dnbr_cells) #target is .53 above 300
sum(dnbr_cells > 400, na.rm = TRUE)/length(dnbr_cells) #target is .42 above 400


ladder_paths <- paste0(rep(paste0(scenarios, "/social-climate-fire/"), each = length(years)), "/ladder-fuels-", years, ".img")

ladder_cells <- NA
for(i in 1:length(dnbr_paths)){
  #TODO remake this a purrr::map workflow
  ladder_rast <- terra::rast(ladder_paths[i])
  # plot(ladder_rast)
  ladder_cells_temp <- ladder_rast[]
  ladder_cells <- c(ladder_cells, ladder_cells_temp[ladder_cells_temp[] > 0])
}

hist(ladder_cells)
mean(ladder_cells, na.rm = TRUE)

#TODO extract more information from rasters?

## aggregate to five-year chunks
# scr_summaries_5_year <- scr_summaries %>%
#   dplyr::mutate(year_round = plyr::round_any(SimulationYear, 5, f = ceiling)) %>%
#   dplyr::group_by(id, year_round) %>%
#   dplyr::summarise(across(where(is.numeric), sum))

#-------------------------------------------------------------------------------
#Compare ignitions
subset_proportion_isro <- st_area(poly_bound)/st_area(region)
subset_proportion_isro <- units::drop_units(subset_proportion_isro)

hist(fire_summaries$TotalFires)
mean(fire_summaries$TotalFires)
hist(I(short_subset_by_year$n * subset_proportion_isro))
mean(short_subset_by_year$n * subset_proportion_isro)
vioplot(fire_summaries$TotalFires, 
        isro_combined$n,
        short_isro_by_year$n,
        short_subset_by_year$n * subset_proportion_isro,
        names = c("Simulated", "ISRO (Historical)", "ISRO (Short)", "Region (Short)"),
        ylab = "Number of ignitions per year")

n_fires_short_subset_by_year_type <- short_subset %>%
  sf::st_drop_geometry() %>%
  mutate(FIRE_YEAR = factor(FIRE_YEAR, 1992:2018)) %>% #workaround to make years with no fire have area burned = 0 %>%
  group_by(FIRE_YEAR, NWCG_CAUSE_CLASSIFICATION) %>%
  count() %>%
  rename(cause = NWCG_CAUSE_CLASSIFICATION)%>%
  ungroup() %>%
  tidyr::complete(FIRE_YEAR, cause, fill=list(n=0))

n_fires_short_isro_by_year_type <-  short_isro %>%
  sf::st_drop_geometry() %>%
  group_by(FIRE_YEAR, NWCG_CAUSE_CLASSIFICATION) %>%
  count() %>%
  rename(cause = NWCG_CAUSE_CLASSIFICATION) %>%
  ungroup() %>%
  tidyr::complete(FIRE_YEAR, cause, fill=list(n=0))

area_burned_short_subset_by_year_type <- short_subset %>%
  sf::st_drop_geometry() %>%
  mutate(FIRE_YEAR = factor(FIRE_YEAR, 1992:2018)) %>% #workaround to make years with no fire have area burned = 0
  group_by(FIRE_YEAR, NWCG_CAUSE_CLASSIFICATION) %>%
  summarise(area_burned = sum(FIRE_SIZE)) %>%
  mutate(FIRE_YEAR = as.numeric(FIRE_YEAR)) %>%
  rename(cause = NWCG_CAUSE_CLASSIFICATION)%>%
  ungroup() %>%
  tidyr::complete(FIRE_YEAR, cause, fill=list(area_burned=0))

area_burned_short_isro_by_year_type <- short_isro %>%
  sf::st_drop_geometry() %>%
  mutate(FIRE_YEAR = factor(FIRE_YEAR, 1992:2018)) %>% #workaround to make years with no fire have area burned = 0
  group_by(FIRE_YEAR, NWCG_CAUSE_CLASSIFICATION) %>%
  summarise(area_burned = sum(FIRE_SIZE)) %>%
  mutate(FIRE_YEAR = as.numeric(FIRE_YEAR)) %>%
  rename(cause = NWCG_CAUSE_CLASSIFICATION)%>%
  ungroup() %>%
  tidyr::complete(FIRE_YEAR, cause, fill=list(area_burned=0))

area_burned_subset <- short_subset %>%
  sf::st_drop_geometry() %>%
  mutate(FIRE_YEAR = factor(FIRE_YEAR, 1992:2018)) %>% #workaround to make years with no fire have area burned = 0
  group_by(FIRE_YEAR) %>%
  summarise(area_burned = sum(FIRE_SIZE)) %>%
  mutate(FIRE_YEAR = as.numeric(FIRE_YEAR))%>%
  ungroup() %>%
  tidyr::complete(FIRE_YEAR, fill=list(area_burned=0))

area_burned_isro <- short_isro %>%
  sf::st_drop_geometry() %>%
  mutate(FIRE_YEAR = factor(FIRE_YEAR, 1992:2018)) %>% #workaround to make years with no fire have area burned = 0
  group_by(FIRE_YEAR) %>%
  summarise(area_burned = sum(FIRE_SIZE)) %>%
  mutate(FIRE_YEAR = as.numeric(FIRE_YEAR))%>%
  ungroup() %>%
  tidyr::complete(FIRE_YEAR, fill=list(area_burned=0))
#-------------------------------------------------------------------------------
#fire occurrence, accidental

hist(fire_summaries$NumberFiresAccidental)
mean(fire_summaries$NumberFiresAccidental)
var(fire_summaries$NumberFiresAccidental)
hist(n_fires_short_subset_by_year_type[n_fires_short_subset_by_year_type$cause == "Human", ]$n * subset_proportion_isro)
mean(n_fires_short_subset_by_year_type[n_fires_short_subset_by_year_type$cause == "Human", ]$n) * subset_proportion_isro
var(n_fires_short_subset_by_year_type[n_fires_short_subset_by_year_type$cause == "Human", ]$n * subset_proportion_isro)
hist(n_fires_short_isro_by_year_type[n_fires_short_isro_by_year_type$cause == "Human", ]$n)
mean(n_fires_short_isro_by_year_type[n_fires_short_isro_by_year_type$cause == "Human", ]$n)
var(n_fires_short_isro_by_year_type[n_fires_short_isro_by_year_type$cause == "Human", ]$n)

vioplot(fire_summaries$NumberFiresAccidental, 
        # n_fires_short_subset_by_year_type[n_fires_short_subset_by_year_type$cause == "Human", ]$n * subset_proportion_isro,
        n_fires_short_isro_by_year_type[n_fires_short_isro_by_year_type$cause == "Human", ]$n)


# TO CALIBRATE:
# Mean number of fires (lambda) is too low -- to increase in the count model 
# in SCRPPLE, find the ratio between observed and desired # of fires, take the log,
# and add that to the intercept. 



# fire occurrence, lightning
hist(fire_summaries$NumberFiresLightning)
mean(fire_summaries$NumberFiresLightning)
var(fire_summaries$NumberFiresLightning)
hist(n_fires_short_subset_by_year_type[n_fires_short_subset_by_year_type$cause == "Natural", ]$n * subset_proportion_isro)
mean(n_fires_short_subset_by_year_type[n_fires_short_subset_by_year_type$cause == "Natural", ]$n * subset_proportion_isro)
var(n_fires_short_subset_by_year_type[n_fires_short_subset_by_year_type$cause == "Natural", ]$n * subset_proportion_isro)
hist(n_fires_short_isro_by_year_type[n_fires_short_isro_by_year_type$cause == "Natural", ]$n)
mean(n_fires_short_isro_by_year_type[n_fires_short_isro_by_year_type$cause == "Natural", ]$n)
var(n_fires_short_isro_by_year_type[n_fires_short_isro_by_year_type$cause == "Natural", ]$n)

vioplot(fire_summaries$NumberFiresLightning,
        n_fires_short_subset_by_year_type[n_fires_short_subset_by_year_type$cause == "Natural", ]$n * subset_proportion_isro,
        n_fires_short_isro_by_year_type[n_fires_short_isro_by_year_type$cause == "Natural", ]$n)

test <- fire_summaries %>% group_by(run_name) %>% summarise(mean = mean(NumberFiresLightning))
mean(test$mean)
log(1/0.605)

vioplot(NumberFiresLightning ~ run_name, data = fire_summaries)
vioplot(NumberFiresAccidental ~ run_name, data = fire_summaries)
vioplot(NumberFiresRx ~ run_name, data = fire_summaries) 

log(2.15/1.64)

#-------------------------------------------------------------------------------
#area burned per year?

#total
boxplot(fire_summaries$TotalBurnedSites*0.36, 
        isro_combined$total,
        area_burned_subset$area_burned / 2.47  * subset_proportion_isro,
        # area_burned_isro$area_burned / 2.47,
        names = c("Simulated", "Historical ISRO", "Region"),
        ylab = "Area burned per year (hectares)")

vioplot(fire_summaries$TotalBurnedSites*0.36,
        isro_combined$total,
        area_burned_subset$area_burned / 2.47  * subset_proportion_isro,
        area_burned_isro$area_burned / 2.47,
        names = c("Simulated", "Observed ISRO", "ISRO (Short)", "Region (Short)"),
        ylab = "Area burned per year (hectares)")

t.test(fire_summaries$TotalBurnedSites*0.36, 
       area_burned_tcsi$area_burned / 2.47  * subset_proportion_isro)


#target: 

hist(fire_summaries$TotalBurnedSitesAccidental*0.36) #convert to ha
mean(fire_summaries$TotalBurnedSitesAccidental*0.36)
fire_summaries %>% group_by(fire_model) %>% summarise(mean = mean(TotalBurnedSitesAccidental))
var(fire_summaries$TotalBurnedSitesAccidental*0.36)
hist(area_burned_short_subset_by_year_type[area_burned_short_subset_by_year_type$cause == "Human", ]$area_burned / 2.47 * subset_proportion_isro) #convert to ha
mean(area_burned_short_subset_by_year_type[area_burned_short_subset_by_year_type$cause == "Human", ]$area_burned / 2.47) * subset_proportion_isro
var(area_burned_short_subset_by_year_type[area_burned_short_subset_by_year_type$cause == "Human", ]$area_burned / 2.47 * subset_proportion_isro)
hist(area_burned_short_isro_by_year_type[area_burned_short_isro_by_year_type$cause == "Human", ]$area_burned) #convert to ha
mean(area_burned_short_isro_by_year_type[area_burned_short_isro_by_year_type$cause == "Human", ]$area_burned / 2.47)
var(area_burned_short_isro_by_year_type[area_burned_short_isro_by_year_type$cause == "Human", ]$area_burned / 2.47) #convert to ha
vioplot(fire_summaries$TotalBurnedSitesAccidental*0.36, 
        area_burned_short_subset_by_year_type[area_burned_short_subset_by_year_type$cause == "Human", ]$area_burned / 2.47  * subset_proportion_isro,
        area_burned_short_isro_by_year_type[area_burned_short_isro_by_year_type$cause == "Human", ]$area_burned / 2.47,
        ylab = "Area burned per year (hectares)")


test <- fire_summaries %>% group_by(run_name) %>% summarise(mean = mean(TotalBurnedSitesAccidental)*0.36)
mean(test$mean)

log(4207/2253)

#lightning fires
#target: 1038

hist(fire_summaries$TotalBurnedSitesLightning*0.36) #convert to ha
mean(fire_summaries$TotalBurnedSitesLightning*0.36)
median(fire_summaries$TotalBurnedSitesLightning*0.36)
fire_summaries %>% group_by(fire_model) %>% summarise(mean = mean(TotalBurnedSitesLightning))
var(fire_summaries$TotalBurnedSitesLightning*0.36)

hist(area_burned_short_subset_by_year_type[area_burned_short_subset_by_year_type$cause == "Natural", ]$area_burned / 2.47 * subset_proportion_isro) #convert to ha
mean(area_burned_short_subset_by_year_type[area_burned_short_subset_by_year_type$cause == "Natural", ]$area_burned / 2.47) * subset_proportion_isro
median(area_burned_short_subset_by_year_type[area_burned_short_subset_by_year_type$cause == "Natural", ]$area_burned / 2.47) * subset_proportion_isro
var(area_burned_short_subset_by_year_type[area_burned_short_subset_by_year_type$cause == "Natural", ]$area_burned / 2.47 * subset_proportion_isro)

hist(area_burned_short_isro_by_year_type[area_burned_short_isro_by_year_type$cause == "Natural", ]$area_burned/2.47) #convert to ha
mean(area_burned_short_isro_by_year_type[area_burned_short_isro_by_year_type$cause == "Natural", ]$area_burned / 2.47)
median(area_burned_short_isro_by_year_type[area_burned_short_isro_by_year_type$cause == "Natural", ]$area_burned / 2.47)
var(area_burned_short_isro_by_year_type[area_burned_short_isro_by_year_type$cause == "Natural", ]$area_burned / 2.47) #convert to ha

vioplot(fire_summaries$TotalBurnedSitesLightning*0.36, 
        area_burned_short_subset_by_year_type$area_burned / 2.47  * subset_proportion_isro,
        area_burned_short_isro_by_year_type$area_burned / 2.47,
        names = c("LANDIS", "Region", "ISRO"),
        ylab = "Area burned per year (hectares)")

test <- fire_summaries %>% group_by(run_name) %>% summarise(mean = mean(TotalBurnedSitesLightning)*0.36) 
# mean(test$mean[5:8])

test <- fire_summaries %>% group_by(run_name) %>% summarise(mean = mean(TotalBurnedSitesRx)*0.36) 


#compare among treatments
vioplot(TotalBurnedSitesLightning * 0.36 ~ run_name, data = fire_summaries)
vioplot(TotalBurnedSitesAccidental ~ run_name, data = fire_summaries)
vioplot(TotalBurnedSitesRx ~ run_name, data = fire_summaries) 

vioplot(TotalBiomassMortalityLightning ~ run_name, data = fire_summaries)
vioplot(TotalBiomassMortalityAccidental ~ run_name, data = fire_summaries)

boxplot(TotalBurnedSitesLightning ~ run_name, data = fire_summaries)
boxplot(TotalBurnedSitesAccidental ~ run_name, data = fire_summaries)
boxplot(TotalBurnedSitesRx ~ run_name, data = fire_summaries)

fire_summaries %>%
  # group_by(climate, mgmt, fire_model) %>%
  group_by(run_name) %>%
  summarise(fire = mean(TotalBurnedSitesLightning) * 0.36) 

fire_summaries %>%
  # group_by(climate, mgmt, fire_model) %>%
  group_by(run_name) %>%
  summarise(fire = mean(TotalBurnedSitesAccidental) *  0.36) 

fire_summaries %>%
  # group_by(climate, mgmt, fire_model) %>%
  group_by(run_name) %>%
  summarise(fire = mean(TotalBurnedSites) *  0.36) 

#MFRI
mfri <- fire_summaries %>%
  group_by(run_name) %>%
  summarise(total_area = sum(TotalBurnedSites * 0.36),
            total_proportion = total_area / (units::drop_units(st_area(poly_bound))/10000),
            fri = 1/(total_proportion/40))

mfri


#-----------------------------------------------------------------------------
#area burned per fire

fire_events_accidental <-fire_events %>%
  filter(IgnitionType == "Accidental")
fire_events_lightning <-fire_events %>%
  filter(IgnitionType == "Lightning")
isro_combined_all <- isro_combined_all[isro_combined_all$FIRE_SIZE > 0.36, ] #remove fires smaller than one cell

#target 3.27

plot(density(log(isro_combined_all$FIRE_SIZE)), col = "blue",
     main = "Fire size distributions",
     ylim = c(0,0.5))
lines(density(log(fire_events_accidental$TotalSitesBurned * 0.36)), col = "red") #convert to ha

mean(log(fire_events_accidental$TotalSitesBurned * 0.36))
mean(log(isro_combined_all$FIRE_SIZE))

hist(log(short_subset[short_subset$NWCG_CAUSE_CLASSIFICATION == "Human", ]$FIRE_SIZE / 2.47))
mean(log(short_subset[short_subset$NWCG_CAUSE_CLASSIFICATION == "Human", ]$FIRE_SIZE / 2.47))
hist(log(short_isro[short_isro$NWCG_CAUSE_CLASSIFICATION == "Human", ]$FIRE_SIZE / 2.47))
mean(log(short_isro[short_isro$NWCG_CAUSE_CLASSIFICATION == "Human", ]$FIRE_SIZE / 2.47))
vioplot(log(fire_events_accidental[fire_events_accidental$fire_model == "original", ]$TotalSitesBurned *0.36), 
        log(short_subset[short_subset$NWCG_CAUSE_CLASSIFICATION == "Human", ]$FIRE_SIZE / 2.47),
        log(short_isro[short_isro$NWCG_CAUSE_CLASSIFICATION == "Human", ]$FIRE_SIZE / 2.47))

fire_events_accidental %>% group_by(run_name) %>% summarise(mean = mean(log(TotalSitesBurned*0.36)))
log(4.54/3.27)

#target 4.02
hist(log(fire_events_lightning$TotalSitesBurned * 0.36)) #convert to ha
mean(log(fire_events_lightning$TotalSitesBurned * 0.36))
hist(log(short_subset[short_subset$NWCG_CAUSE_CLASSIFICATION == "Natural", ]$FIRE_SIZE / 2.47))
mean(log(short_subset[short_subset$NWCG_CAUSE_CLASSIFICATION == "Natural", ]$FIRE_SIZE / 2.47))
hist(log(short_isro[short_isro$NWCG_CAUSE_CLASSIFICATION == "Natural", ]$FIRE_SIZE / 2.47))
mean(log(short_isro[short_isro$NWCG_CAUSE_CLASSIFICATION == "Natural", ]$FIRE_SIZE / 2.47))
vioplot(log(fire_events_lightning$TotalSitesBurned*0.36), 
        log(short_subset[short_subset$NWCG_CAUSE_CLASSIFICATION == "Natural", ]$FIRE_SIZE / 2.47),
        log(short_isro[short_isro$NWCG_CAUSE_CLASSIFICATION == "Natural", ]$FIRE_SIZE / 2.47))


fire_events_lightning %>% group_by(run_name) %>% summarise(mean = mean(log(TotalSitesBurned*0.36)))
4.28/4.02

#time of year of fires
#accidental ignitions
hist(fire_events_accidental$InitialDayOfYear)
hist(short_subset[short_subset$NWCG_CAUSE_CLASSIFICATION == "Human", ]$DISCOVERY_DOY)
vioplot(fire_events_accidental$InitialDayOfYear, 
        short_subset[short_subset$NWCG_CAUSE_CLASSIFICATION == "Human", ]$DISCOVERY_DOY,
        short_isro[short_isro$NWCG_CAUSE_CLASSIFICATION == "Human", ]$DISCOVERY_DOY)
      

#lightning igntions
hist(fire_events_lightning$InitialDayOfYear)
hist(short_subset[short_subset$NWCG_CAUSE_CLASSIFICATION == "Natural", ]$DISCOVERY_DOY)
vioplot(fire_events_lightning$InitialDayOfYear, 
        short_subset[short_subset$NWCG_CAUSE_CLASSIFICATION == "Natural", ]$DISCOVERY_DOY,
        short_isro[short_isro$NWCG_CAUSE_CLASSIFICATION == "Natural", ]$DISCOVERY_DOY)

#-------------------------------------------------------------------------------
# Fire over time

plot(fire_summaries$TotalBurnedSitesAccidental ~ fire_summaries$SimulationYear)

fire_summaries$Year <- fire_summaries$SimulationYear + 1999

fire_summaries$TotalBurnedAcresRx <- fire_summaries$TotalBurnedSitesRx * 8

ggplot(data = fire_summaries, mapping = aes(x = Year, y = TotalBurnedAcresRx)) + 
  geom_point(color="steelblue") + 
  labs(title = "Prescribed burn area",
       subtitle = "by management scenario and climate scenario",
       y = "Area burned (acres)", x = "Year") + 
  geom_smooth( color = "black") + 
  facet_wrap(~climate)

ggplot(data = fire_summaries, mapping = aes(x = Year, y = TotalBurnedSitesAccidental * 0.36)) + 
  geom_point(color="steelblue") + 
  labs(title = "Accidental burn area",
       subtitle = "by management scenario and climate scenario",
       y = "Area burned (ha)", x = "Year") + 
  geom_smooth( color = "black") + 
  facet_wrap(~ climate)

ggplot(data = fire_summaries, mapping = aes(x = Year, y = TotalBurnedSitesLightning * 0.36)) + 
  geom_point(color="steelblue") + 
  labs(title = "Lightning burn area",
       subtitle = "by management scenario and climate scenario",
       y = "Area burned (ha)", x = "Year") + 
  geom_smooth( color = "black") + 
  facet_wrap(~ climate)

ggplot(data = fire_summaries, mapping = aes(x = Year, y = TotalBurnedSites * 0.36)) + 
  geom_point(color="steelblue") + 
  labs(title = "Total burn area",
       # subtitle = "by management scenario and climate scenario",
       y = "Area burned (ha)", x = "Year") + 
  geom_smooth( color = "black")  +
  geom_abline(intercept = mean(isro_historical_records$total), slope = 0, linetype = 2) +
  geom_abline(intercept = mean(isro_historical_records[-1, ]$total), slope = 0, linetype = 1) +
  facet_wrap(~ fire_model + climate)
 


ggplot(short_isro_by_year, mapping = aes(x = as.numeric(as.character(FIRE_YEAR)), y = total)) +
  geom_point() +
  geom_smooth(color = "black")

ggplot(short_subset_by_year, mapping = aes(x = as.numeric(as.character(FIRE_YEAR)), y = total * subset_proportion_isro)) +
  geom_point() +
  geom_smooth(color = "black")

#-------------------------------------------------------------------------------
# High-intensity fire


ggplot(data = fire_summaries, mapping = aes(x = Year, y = TotalSitesHighIntensity * 0.36)) + 
  geom_point(color="steelblue") + 
  labs(title = "Areaburned at high intensity",
       subtitle = "by management scenario and climate scenario",
       y = "Area burned (ha)", x = "Year") + 
  geom_smooth( color = "black") + 
  facet_wrap(~ mgmt + climate)
mean(fire_summaries[fire_summaries$climate == "Historical", ]$TotalSitesHighIntensity * 0.36)

ggplot(data = filter(fire_summaries, TotalBurnedSites > 0), mapping = aes(x = Year, y = TotalSitesHighIntensity/TotalBurnedSites)) + 
  geom_point(color="steelblue") + 
  labs(title = "Proportion of sites burned at high intensity",
       subtitle = "by management scenario and climate scenario",
       y = "Proportion high intensity", x = "Year") + 
  geom_smooth(method = "lm", color = "black") + 
  facet_wrap(~ climate)

events_sum <- fire_events %>%
  group_by(SimulationYear, run_name) %>%
  summarise(mean_dnbr = weighted.mean(MeanDNBR, TotalSitesBurned),
            mgmt = mgmt[1],
            climate = climate[1],
            fire_model = fire_model[1],
            total_sites = sum(TotalSitesBurned))

ggplot(data = events_sum, mapping = aes(x = SimulationYear, y = mean_dnbr)) + 
  geom_point(color="steelblue") + 
  labs(title = "Mean DNBR - LANDIS runs") + 
  geom_smooth( color = "black") + 
  facet_wrap(~ run_name)

ggplot(data = mtbs_dnbr_year, mapping = aes(x = year, y = mean_dnbr)) + 
  geom_point(color="steelblue") + 
  labs(title = "Mean DNBR",
       subtitle = "from MTBS records",
       y = "DNBR", x = "Year") + 
  geom_smooth( color = "black") 



#mean dnbr -- target is 306
weighted.mean(fire_events$MeanDNBR, fire_events$TotalSitesBurned)
mean(fire_events$MeanDNBR)
weighted.mean(fire_events$MeanFWI, fire_events$TotalSitesBurned)
weighted.mean(fire_events$MeanEffectiveWindSpeed, fire_events$TotalSitesBurned)
weighted.mean(fire_events$MeanLadderFuels, fire_events$TotalSitesBurned)
weighted.mean(fire_events$MeanEffectiveWindSpeed)
hist(fire_events$MeanDNBR)
hist(fire_events$MeanEffectiveWindSpeed)
mean(fire_events$MeanEffectiveWindSpeed)
hist(fire_events$MeanFWI)
mean(fire_events$MeanFWI)
hist(fire_events$MeanLadderFuels)
mean(fire_summaries$TotalSitesHighIntensity/fire_summaries$TotalBurnedSites, na.rm = TRUE)

plot(fire_events$MeanDNBR ~ fire_events$MeanEffectiveWindSpeed)
abline(lm(fire_events$MeanDNBR ~ fire_events$MeanEffectiveWindSpeed))
summary(lm(fire_events$MeanDNBR ~ fire_events$MeanEffectiveWindSpeed))
plot(fire_events$MeanDNBR ~ fire_events$MeanFWI)
abline(lm(fire_events$MeanDNBR ~ fire_events$MeanFWI))
summary(lm(fire_events$MeanDNBR ~ fire_events$MeanFWI))
plot(fire_events$MeanDNBR ~ fire_events$MeanLadderFuels)
summary(lm(fire_events$MeanDNBR ~ fire_events$MeanLadderFuels))

summary(lm(MeanDNBR ~ MeanEffectiveWindSpeed + MeanFWI + MeanLadderFuels, data = fire_events))


#-------------------------------------------------------------------------------
# Fire over time -- biomass burned

fire_summaries <- fire_summaries %>%
  group_by(run_name) %>%
  mutate(CumBiomassMort = cumsum(TotalBiomassMortalityAccidental)) %>%
  ungroup()

fire_summaries %>% 
  filter(Year == 2100, climate == "Historical", mgmt == "Scenario6") %>% 
  select(CumBiomassMort)  %>%
  unlist() %>%
  mean()

fire_summaries %>% 
  filter(Year == 2100, climate == "Historical", mgmt == "Scenario7") %>% 
  select(CumBiomassMort)  %>%
  unlist() %>%
  mean()


ggplot(data = fire_summaries, mapping = aes(x = Year, y = TotalBiomassMortalityRx)) + 
  geom_point(color="steelblue") + 
  labs(title = "Prescribed burn area",
       subtitle = "by management scenario and climate scenario",
       y = "Biomass burned (units)", x = "Year") + 
  geom_smooth( color = "black") +
  facet_wrap(~ mgmt + climate)
# facet_wrap(~ mgmt + climate, nrow = 3, ncol = 3)

ggplot(data = fire_summaries, mapping = aes(x = Year, y = TotalBiomassMortalityAccidental)) + 
  geom_point(color="steelblue") + 
  labs(title = "Biomass killed by acciental fire",
       subtitle = "by management scenario and climate scenario",
       y = "Biomass killed (g m-2)", x = "Year") + 
  geom_smooth( color = "black") + 
  facet_wrap(~ climate + mgmt)

ggplot(data = fire_summaries, mapping = aes(x = Year, y = TotalBiomassMortalityLightning)) + 
  geom_point(color="steelblue") + 
  labs(title = "Biomass killed by natural fires",
       subtitle = "by management scenario and climate scenario",
       y = "Area burned (acres)", x = "Year") + 
  geom_smooth( color = "black") + 
  facet_wrap(~ mgmt + climate, nrow = 3, ncol = 3)

ggplot(data = fire_summaries, mapping = aes(x = Year, y = CumBiomassMort*(120*120)/1000/1000)) + 
  geom_point(color="steelblue") + 
  labs(title = "Cumulative biomass burned",
       subtitle = "by management scenario and climate scenario",
       y = "Biomass (Mg)", x = "Year") + 
  geom_smooth( color = "black") + 
  facet_wrap(~ mgmt + climate)
# facet_wrap(~ mgmt + climate, nrow = 2, ncol = 3, dir = "v")

### compare fire and beetles
#bda_summaries comes from process_bda_tables.R

combined <- fire_summaries %>%
  left_join(dplyr::select(bda_summaries2, !c("mgmt", "climate")),  by = c("run_name", "Year")) %>%
  mutate((across(c("TotalBiomassKilled", "TotalSitesAffected"), ~replace(., is.na(.), 0)))) %>%
  mutate(TotalBiomassWildfire = TotalBiomassMortalityAccidental + TotalBiomassMortalityLightning,
         TotalBurnedSites = TotalBurnedSitesAccidental + TotalBurnedSitesLightning) %>%
  filter(climate != "CNRM")

ggplot() + 
  labs(title = "Area affected by fire and beetles",
       subtitle = "by management scenario and climate scenario",
       y = "Area affected (acres)", x = "Year") + 
  geom_smooth(color="steelblue", data = combined[combined$climate == "Historical", ], mapping = aes(x = Year, y = TotalBurnedSites * 8)) + 
  geom_smooth(color="green", data = combined[combined$climate == "Historical", ], mapping = aes(x = Year, y = TotalSitesAffected * 8)) + 
  facet_wrap(~ mgmt + climate, dir = "v") + 
  scale_color_manual(name = "", values = c("TotalBurnedSites" = "steelblue", "TotalSitesAffected" = "green"))

ggplot() + 
  labs(title = "Biomass killed by fire and beetles",
       subtitle = "by management scenario and climate scenario",
       y = "Biomass killed (Mg)", x = "Year") + 
  geom_smooth(color="steelblue", data = combined[combined$climate == "Historical", ], mapping = aes(x = Year, y = TotalBiomassWildfire*(120*120)/1000/1000)) + 
  geom_smooth(color="green", data = combined[combined$climate == "Historical", ], mapping = aes(x = Year, y = TotalBiomassKilled*(120*120)/1000/1000)) +
  facet_wrap(~ mgmt + climate, dir = "v")

#-------------------------------------------------------------------------------
# Get fires which occur after a harvest

# Match harvests and fires -- overlap and time since treatment
# Get harvest intensity and fire intensity

