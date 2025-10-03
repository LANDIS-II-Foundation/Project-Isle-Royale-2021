#download netcdfs from NKN server
#get URLS from https://climate.northwestknowledge.net/MACA/data_portal.php

#GFDL-ESM2M 4.5 -- least change
#CANESM 8.5
#MRI-CGM 8.5
model <- "MRI-CGCM3"
rcp <- "85"
vars <- c("pr", 
          "rhsmax",
          "rhsmin",
          "tasmin",
          "tasmax",
          "uas",
          "vas")
vars_long <- c("precipitation",
               "relative_humidity",
               "relative_humidity",
               "air_temperature",
               "air_temperature",
               "eastward_wind",
               "northward_wind")

urls <- paste0("http://thredds.northwestknowledge.net:8080/thredds/ncss/grid/agg_macav2metdata_", vars, "_", model, "_r1i1p1_rcp", rcp, "_2006_2099_CONUS_daily.nc?&var=", vars_long, "&north=48.2000&south=47.8000&west=-89.3000&east=-88.4000&temporal=all&accept=netcdf&point=false")
filenames <- paste0(model, "-", vars)
destfiles <- paste0("D:/Data/maca/", filenames)

options(timeout = max(600, getOption("timeout")))

for(i in 1:length(urls)){
  download.file(urls[i], destfile = destfiles[i], method = "auto", 
                quiet = FALSE, mode = "wb",
                cacheOK = TRUE,
                extra = getOption("download.file.extra"),
                headers = NULL)
}


#--------------------------------
# processing
library(terra)
ecoregions <- terra::rast("")

