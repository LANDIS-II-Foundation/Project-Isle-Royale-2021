##---------------------------------------------------------------------------------
# Fit statistical model to number of ignitions ~ FWI 
library("tidyverse")
library("tidyselect")
library("sf")
library("cffdrs")
library("lubridate")
library("smoothr")
library("progress")
library("terra")
# library("ncdf4")


ecoregions <- terra::rast("./Models/LANDIS inputs/input rasters/ecoregions_inv.tif")
ecoregion_size <- table(values(ecoregions))

poly_bound <- sf::st_read("./Parameterization/Parameterization data/isle_royale_boundary_buffer/isle_royale_boundary_buffer.shp")%>%
  sf::st_union()%>%
  sf::st_transform(crs = crs(ecoregions))

#geomac data are in EPSG:4269 or EPSG:4326
region <- sf::st_read("D:/Data/epa_ecoregions/us_eco_l3/us_eco_l3.shp") %>%
  filter(US_L3CODE %in% c(50,51)) %>%
  sf::st_union()%>%
  sf::st_transform(crs = crs(ecoregions))

#short dataset already extracted for epa regions 50 and 51
short_subset <- sf::st_read("./Parameterization/Parameterization data/short/short_region.gpkg")%>%
  # filter(FIRE_SIZE >= 0.6) %>%
  sf::st_transform(crs = crs(ecoregions))

short_isro <- short_subset %>%
  sf::st_filter(poly_bound)
plot(short_isro)

fwi_landis <- read.csv("./Parameterization/Parameterization data/Climate-future-input-log.csv")
#add a date column
fwi_landis$date <- parse_date_time(as.character(fwi_landis$Year), orders = "y")
yday(fwi_landis$date) <- fwi_landis$Timestep

#get weighted mean of FWI for the whole study area
fwi_isro <- fwi_landis %>%
  dplyr::select(c("date", "EcoregionIndex", "FWI")) %>%
  pivot_wider(names_from = "EcoregionIndex", values_from = "FWI")
fwi_isro$fwi <- apply(fwi_isro[, -1], 1, FUN = function(x) mean(x))

table(short_subset$FIRE_YEAR) #first year is 1992

short_subset$date_clean <- lubridate::parse_date_time(as.Date(short_subset$DISCOVERY_DATE), orders = "ymd")

short_by_day <- aggregate(short_subset, by = list(short_subset$date_clean, short_subset$NWCG_CAUSE_CLASSIFICATION), FUN = length)[, c(1:3)] %>%
  rename(date = Group.1, cause = Group.2, n.fires = FOD_ID)

short_isro$date_clean <- lubridate::parse_date_time(as.Date(short_isro$DISCOVERY_DATE), order = "ymd")

short_isro_by_day <- aggregate(short_isro, by = list(short_isro$date_clean, short_isro$NWCG_CAUSE_CLASSIFICATION), FUN = length)[, c(1:3)] %>%
rename(date = Group.1, cause = Group.2, n.fires = FOD_ID)

all_fwi_data_merge_lightning <- dplyr::left_join(fwi_isro[, c("date", "fwi")], subset(short_by_day, cause == "Natural")) %>%
  dplyr::mutate(n.fires = replace_na(n.fires, 0)) %>%
  dplyr::filter(date > as.Date("1992-01-01"))
all_fwi_data_merge_accidental <- dplyr::left_join(fwi_isro[, c("date", "fwi")], subset(short_by_day, cause == "Human"))%>%
  dplyr::mutate(n.fires = replace_na(n.fires, 0))%>%
  dplyr::filter(date > as.Date("1992-01-01"))

isro_fwi_data_merge_lightning <- dplyr::left_join(fwi_isro[, c("date", "fwi")], subset(short_isro_by_day, cause == "Natural")) %>%
  dplyr::mutate(n.fires = replace_na(n.fires, 0)) %>%
  dplyr::filter(date > as.Date("1992-01-01"))
isro_fwi_data_merge_accidental <- dplyr::left_join(fwi_isro[, c("date", "fwi")], subset(short_isro_by_day, cause == "Human"))%>%
  dplyr::mutate(n.fires = replace_na(n.fires, 0))%>%
  dplyr::filter(date > as.Date("1992-01-01"))

plot(n.fires ~ fwi, data = all_fwi_data_merge_lightning)
hist(all_fwi_data_merge_lightning$fwi)
plot(n.fires ~ fwi, data = all_fwi_data_merge_accidental)

area_region <- sf::st_area(region)
area_isro <- sf::st_area(poly_bound)
scale_region_to_isro <- as.numeric(area_isro/ area_region)
#-------------------------------------------------------------------------------
# fitting  models
library(pscl)

lightning_model <- zeroinfl(as.numeric(n.fires)~as.numeric(fwi), data=all_fwi_data_merge_lightning, dist="poisson")
summary(lightning_model)
plot(predict(lightning_model) ~ as.numeric(all_fwi_data_merge_lightning$n.fires)) #not a great fit, but it's daily
scaled_coef <- coef(lightning_model)[1] + log(scale_region_to_isro)

accidental_model <- zeroinfl(as.numeric(n.fires)~as.numeric(fwi), data=all_fwi_data_merge_accidental, dist="poisson")
summary(accidental_model)
plot(predict(accidental_model) ~ as.numeric(all_fwi_data_merge_accidental$n.fires))
scaled_coef <- coef(accidental_model)[1] + log(scale_region_to_isro)

lightning_isro <- zeroinfl(as.numeric(n.fires)~as.numeric(fwi), data=isro_fwi_data_merge_lightning, dist="poisson")
summary(lightning_isro)
plot(predict(lightning_isro) ~ as.numeric(isro_fwi_data_merge_lightning$n.fires)) #not a great fit, but it's daily

accidental_isro <- zeroinfl(as.numeric(n.fires)~as.numeric(fwi), data=isro_fwi_data_merge_accidental, dist="poisson")
summary(accidental_isro)
plot(predict(lightning_isro) ~ as.numeric(isro_fwi_data_merge_lightning$n.fires)) #not a great fit, but it's daily


#models fail to capture really crazy fire-years

lightning_isro_poisson <- glm(as.numeric(n.fires)~as.numeric(fwi), data=isro_fwi_data_merge_lightning, family = poisson(link = "log"))
summary(lightning_isro_poisson)
accidental_isro_poisson <- glm(as.numeric(n.fires)~as.numeric(fwi), data=isro_fwi_data_merge_accidental, family = poisson(link = "log"))
summary(accidental_isro_poisson)

lightning_poisson <- glm(as.numeric(n.fires)~as.numeric(fwi), data=all_fwi_data_merge_lightning, family = poisson(link = "log"))
summary(lightning_poisson)
accidental_poisson <- glm(as.numeric(n.fires)~as.numeric(fwi), data=all_fwi_data_merge_accidental, family = poisson(link = "log"))
summary(accidental_poisson)

#reduce the intercept for the poisson models to modify regional model to isro
coef(lightning_poisson)[1] + log(scale_region_to_isro)
coef(accidental_poisson)[1] + log(scale_region_to_isro)

newdata <- data.frame(fwi = seq(0, 60, length.out = 200))

#zero-inflated allows for a "plateau" in ignitions
#poisson is always increasing
#all of them miss the peak in ignitions around fwi = 40
plot(as.numeric(n.fires)~as.numeric(fwi), data=isro_fwi_data_merge_lightning)
lines(predict(lightning_model, newdata = newdata) ~ newdata$fwi, type = "l") #zinfl
lines(predict(lightning_poisson, newdata = newdata, type = "response")  ~ newdata$fwi) #poisson
lines(predict(lightning_isro, newdata = newdata) / scale_region_to_isro  ~ newdata$fwi) #zinfl for isro
lines(predict(lightning_isro_poisson, newdata = newdata, type = "response") / scale_region_to_isro  ~ newdata$fwi) #poisson for isro

plot(as.numeric(n.fires)~as.numeric(fwi), data=isro_fwi_data_merge_accidental)
lines(predict(accidental_model, newdata = newdata) ~ newdata$fwi)
lines(predict(accidental_poisson, newdata = newdata, type = "response") ~ newdata$fwi)
lines(predict(accidental_isro, newdata = newdata) / scale_region_to_isro ~ newdata$fwi)
lines(predict(accidental_isro_poisson, newdata = newdata, type = "response") / scale_region_to_isro ~ newdata$fwi)


#----------------------------------
#Compare across an average year
#really nice fit here for the whole Sierra region
#almost no difference between the poisson and 

average_year_lightning <- all_fwi_data_merge_lightning %>%
  mutate(jday = format(date, "%j")) %>%
  group_by(jday) %>%
  summarise(mean_ignitions = mean(n.fires),
            mean_fwi = mean(fwi))

plot(mean_fwi ~ jday, data = average_year_lightning,
     type = "l",
     axes = FALSE, xlab = "", ylab = "")
par(new = TRUE)                             # Add new plot
plot(mean_ignitions ~ jday, data = average_year_lightning)
lines(predict(lightning_model, newdata = data.frame(fwi = average_year_lightning$mean_fwi)) ~ average_year_lightning$jday)
lines(predict(lightning_poisson, newdata = data.frame(fwi = average_year_lightning$mean_fwi), type = "response") ~ 
        average_year_lightning$jday)
sum(average_year_lightning$mean_ignitions)
sum(predict(lightning_model, newdata = data.frame(fwi = average_year_lightning$mean_fwi))) #scale number of ignitions?
log(sum(average_year_lightning$mean_ignitions)/sum(predict(lightning_model, newdata = data.frame(fwi = average_year_lightning$mean_fwi))))
#add xx to intercept



average_year_accidental <- all_fwi_data_merge_accidental %>%
  mutate(jday = format(date, "%j")) %>%
  group_by(jday) %>%
  summarise(mean_ignitions = mean(n.fires),
            mean_fwi = mean(fwi))


plot(mean_fwi ~ jday, data = average_year_accidental,
     type = "l",
     axes = FALSE, xlab = "", ylab = "")
par(new = TRUE)                             # Add new plot
plot(mean_ignitions ~ jday, data = average_year_accidental)
lines(predict(accidental_model, newdata = data.frame(fwi = average_year_accidental$mean_fwi)) ~ average_year_accidental$jday)
lines(predict(accidental_poisson, newdata = data.frame(fwi = average_year_accidental$mean_fwi), type = "response") ~ 
        average_year_accidental$jday)
sum(average_year_accidental$mean_ignitions)
sum(predict(accidental_model, newdata = data.frame(fwi = average_year_accidental$mean_fwi))) #scale number of ignitions?

#----------------
# just for isro region
# not enough ignitions for a good fit here

average_year_lightning <- isro_fwi_data_merge_lightning %>%
  mutate(jday = format(date, "%j")) %>%
  group_by(jday) %>%
  summarise(mean_ignitions = mean(n.fires),
            mean_fwi = mean(fwi))

plot(mean_fwi ~ jday, data = average_year_lightning,
     type = "l",
     axes = FALSE, xlab = "", ylab = "")
par(new = TRUE)                             # Add new plot
plot(mean_ignitions ~ jday, data = average_year_lightning)
lines(predict(lightning_isro, newdata = data.frame(fwi = average_year_lightning$mean_fwi)) ~ average_year_lightning$jday)
lines(predict(lightning_isro_poisson, newdata = data.frame(fwi = average_year_lightning$mean_fwi), type = "response") ~ 
        average_year_lightning$jday)

average_year_accidental <- isro_fwi_data_merge_accidental %>%
  mutate(jday = format(date, "%j")) %>%
  group_by(jday) %>%
  summarise(mean_ignitions = mean(n.fires),
            mean_fwi = mean(fwi))


plot(mean_fwi ~ jday, data = average_year_accidental,
     type = "l",
     axes = FALSE, xlab = "", ylab = "")
par(new = TRUE)                             # Add new plot
plot(mean_ignitions ~ jday, data = average_year_accidental)
lines(predict(accidental_isro, newdata = data.frame(fwi = average_year_accidental$mean_fwi)) ~ average_year_accidental$jday)
lines(predict(accidental_isro_poisson, newdata = data.frame(fwi = average_year_accidental$mean_fwi), type = "response") ~ 
        average_year_accidental$jday)


plot(n.fires ~ fwi, data = all_fwi_data_merge_lightning)
plot(predict(lightning_model) ~ all_fwi_data_merge_lightning_test$fwi)
plot(n.fires ~ fwi, data = all_fwi_data_merge_accidental)
plot(predict(accidental_model) ~ all_fwi_data_merge_accidental$fwi)

# comparisons
#what day is the mean fire?
mean(yday(short_by_day$date))

