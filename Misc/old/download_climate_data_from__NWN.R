# download and process climate data
# use geoknife, guide found here: https://cran.r-project.org/web/packages/geoknife/vignettes/geoknife.html

library("geoknife")
library("sf")
library("sp")
library("tidyverse")

#shapefile for study area
isro_boundary <- sf::st_read("./Parameterization/Parameterization data/isle_royale_boundary_buffer/isle_royale_boundary_buffer.shp") %>%
  sf::st_buffer(dist = 10000) %>% #the study area was too small to calculate variance! 
  # Try adding a buffer if you're getting NaN values for variance or st_dev
  sf::st_transform(crs = "+proj=longlat +datum=WGS84") #reproject to CRS that geoknife needs

# study_area <- basemaps::draw_ext()
study_area <- isro_boundary


#"stencil" is what geoknife uses for the extent of the data
stencil <- simplegeom(as(study_area, Class = "Spatial"))

# the "fabric" is what gets "cut out" in the stencil area, in this case a raster of climate data.
# find the URL for the data from THREDDS catalog: https://cida.usgs.gov/thredds/catalog.html
# find the data you want, then find the URL under the OPENDAP link
# or you can use webdatasets = query('webdata') to get a list of all the datasets, and filter from there

# e.g.
# webdatasets = query('webdata')
# grep("prism", webdatasets@group)
#here's some PRISM data for baseline climate
# webdatasets[1]
#we can assign that directly to the fabric
# fabric <- webdata(webdatasets[1])

# here's the MACAv2-METDATA downscaled climate data, URL from the catalog
# fabric <- webdata(url='https://cida.usgs.gov/thredds/dodsC/macav2metdata_daily_future')

#this works a little differently from the USGS portal. Instead of one URL with
#several variables, we have a URL for each variable. 
vars_url <- c("pr", "tasmax", "tasmin", "uas", "vas", "rhsmax", "rhsmin")
mod_url <- "CNRM-CM5"
urls <- paste0("http://thredds.northwestknowledge.net:8080/thredds/dodsC/agg_macav2metdata_", vars_url,"_", mod_url, "_r1i1p1_rcp45_2006_2099_CONUS_daily.nc")

# see what variables are available and get the variable name 
# for MACA data, variables are identified by a concatenation of 
# climate variable, model, and rcp
fabric <- webdata(url = urls[6])
query(fabric, 'variables')

vars_long <- c("precipitation", "air_temperature", 
               "air_temperature", "eastward_wind", 
               "northward_wind", "relative_humidity",
               "relative_humidity")



#what statistics do we want? These three are what we need for the LANDIS Climate Library
summary_stats <- c("MEAN", "VARIANCE", "STD_DEV")

# set up the "knife" which tells the GeoData Portal what to do with the 
# subset data. We want the mean, variance, and std_dev (specified above), 
# averaged across the study area, 
# and there are a few other arguments to give to the remote server, specified 
# by the "knife" object:
# wait = TRUE has R wait while job is processed
# email =TRUE emails you when process is done. 
knife <- webprocess(wait = TRUE)
query(knife, 'algorithms')

# area grid statistics are the default, but we can change it if we  (we don't)
algorithm(knife) <- list('Area Grid Statistics (weighted)' = 
                           "gov.usgs.cida.gdp.wps.algorithm.FeatureWeightedGridStatisticsAlgorithm")

#I think this is the best way to set it? The geoknife documentation isn't clear on
# if there's a better way; might be a feature in development
knife@processInputs$STATISTICS <- summary_stats #what statistics do we want?

#-------------------------------------------------------------------------------
# now to run it for the full set of data that we need

# I had the job fail when I did all variables at once, so let's split them up into separate jobs
# we can't submit several jobs at the same time, so we'll put them in a loop and
# wait until one job is done before starting the next one.
# This takes a variable amount of time -- sometimes 10 minutes or sometimes an hour-ish

job_results <- list()

for(i in 1:length(vars_long)){
  #set the fabric for a new variable, but keep everything else the same (i.e. the stencil and knife)
  fabric <- webdata(url = urls[i])
  variables(fabric) <- vars_long[i]
  print(vars_long[i])
  job <- geoknife(stencil, fabric, knife)
  if(error(job)){
    break
    check(job)
  }
  
  job_results[[i]] <- result(job)
}


#save your work!
# saveRDS(job_results, file = "climate_raw_historical.RDS")

#The data are in a long format -- not quite what we want
str(job_results[[1]]) 

#check on one of our datasets
ppt <- job_results[[1]]
ppt$year <- format(ppt$DateTime, "%Y")
test <- ppt %>% 
  filter(statistic == "MEAN") %>%
  group_by(year) %>%
  summarise(MAP = sum(`1`)) #precip is in mm

t <- job_results[2] # temps are in K

#reshape the data into the format we need
job_results_reform <- job_results %>% 
  #widen data format
  map(., function(x) tidyr::pivot_wider(data = x,
                                        names_from = "statistic",
                                        values_from = "1")) %>%
  #trim end dates -- this would be better done when selecting the fabric
  map(., function(x) dplyr::filter(x, DateTime < as.POSIXct("2019-12-31", format = "%Y-%m-%d"))) %>%
  #create a TIMESTEP column and add some formatting junk to the end of the date. 
  # TODO figure out a better way to do this
  map(., function(x) dplyr::mutate(x, TIMESTEP = paste0(as.character(DateTime), "T00:00:00Z")))

# convert from cm to mm
# job_results_reform[[1]]$MEAN <- job_results_reform[[1]]$MEAN * 10
# job_results_reform[[1]]$VARIANCE <- job_results_reform[[1]]$VARIANCE * 100
# job_results_reform[[1]]$STD_DEV <- job_results_reform[[1]]$STD_DEV * 10

# job_results_reform[[2]]$MEAN <- job_results_reform[[2]]$MEAN - 273.15 #convert from kelvin to celsius
# job_results_reform[[3]]$MEAN <- job_results_reform[[3]]$MEAN - 273.15 #convert from kelvin to celsius


#now we need to wrangle this list of data into the format needed by the climate library
# I haven't figured this out in an elegant way yet TODO

vars_long #remind us what the original var names were

#rewrite variables in the format the climate library needs
# this is sort of difficult using data.frames or tibbles, because 
# there are different kinds of data in each column -- so we'll do everything
# as character vectors then glue it together at the end.
var_rows <- c("#ppt",
              "#Tmax",
              "#Tmin",
              "#windspeed",
              "#winddirection")

units_means <- c("mm/d",
                 "C",
                 "C",
                 "m/s",
                 "deg")
units_variance <- c("mm/d^2",
                    "C^2",
                    "C^2",
                    "m/s^2",
                    "deg^2")


ppt <- job_results_reform[[1]]

# sorry, you're on your own from here. 
# TODO automate this better
TIMESTEP <- character()
means <- character()
variances <- character()
stdev <- character()
for(i in 1:length(var_rows)){
  #the first column has timesteps but also the headers for each variable followed by a blank cell
  TIMESTEP <- c(TIMESTEP, var_rows[i], "", "TIMESTEP", job_results_reform[[i]]$TIMESTEP)
  
  #make each column separately, to avoid having to rbind a bunch of garbage
  #each column grows in length for each variable
  #if you had more ecoregions this would be a bad way to do this
  means <- c(means, "", "eco1", paste0("MEAN(", units_means[i], ")"), job_results_reform[[i]]$MEAN)
  variances <- c(variances, "", "eco1", paste0("VARIANCE(", units_variance[i], ")"), job_results_reform[[i]]$VARIANCE)
  stdev <- c(stdev, "", "eco1", paste0("STD_DEV(", units_means[i], ")"), job_results_reform[[i]]$STD_DEV)
}

output_data <- cbind(TIMESTEP, means, variances, stdev)

write.table(output_data,               # Write CSV file without header
            "./LANDIS inputs/NECN files/historical_gridmet.csv",
            sep = ",",
            col.names = FALSE,
            row.names = FALSE,
            quote = FALSE) # quote = false is important! Otherwise the CL can't read the file, 
# but it won't be apparent looking at the data in Excel
