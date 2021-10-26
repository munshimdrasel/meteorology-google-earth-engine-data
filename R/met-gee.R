# https://github.com/r-spatial/rgee

# Tutorial that I followed:
# http://www.css.cornell.edu/faculty/dgr2/_static/files/R_html/ex_rgee.html#2_Initializing_the_GEE_interface
#data source: https://developers.google.com/earth-engine/datasets/catalog/IDAHO_EPSCOR_GRIDMET#description

library(tidyverse)
library(fst)
library(rgee)
library(sf)
library(ggplot2)
library(lubridate)
ee_Initialize()

setwd("/Volumes/GoogleDrive/My Drive/R/meteorology-google-earth-engine-data")


#getting facility locations
ampd_monthly_emission <- (read.fst ("/Volumes/GoogleDrive/My Drive/R/ampd-raw-data-processing/data/ampd_monthly_all.fst"))
# ampd_raw <- (read.fst ("/projects/HAQ_LAB/mrasel/R/ampd-raw-data-processing/data/ampd_monthly_all.fst"))

facility_locations <- na.omit(unique(ampd_monthly_emission %>%
                                       dplyr::select(ORISPL_CODE, Facility.Latitude, Facility.Longitude)))


facility_locations <- facility_locations %>%
  st_as_sf(., coords = c("Facility.Longitude","Facility.Latitude"), crs=4326)

#converting into facility locations into .shp file
# st_write(facility_locations, "data/facility_locations.shp", quiet = T)
facility <- st_read("data/facility_locations.shp")

facility <-rename(facility, ORISPL_CODE=ORISPL_)
#getting precipitation
gridmet.pr <- ee$ImageCollection("IDAHO_EPSCOR/GRIDMET") %>%
  ee$ImageCollection$filterDate("2010-01-01", "2011-01-01") %>%
  ee$ImageCollection$map(function(x) x$select("pr")) %>% # Select only precipitation bands
  ee$ImageCollection$toBands() %>% # from imagecollection to image
  ee$Image$rename(sprintf("PP_%03d",1:365))

#max temperature
gridmet.tmmx <- ee$ImageCollection("IDAHO_EPSCOR/GRIDMET") %>%
  ee$ImageCollection$filterDate("2010-01-01", "2011-01-01") %>%
  ee$ImageCollection$map(function(x) x$select("tmmx")) %>% # Select only precipitation bands
  ee$ImageCollection$toBands() %>% # from imagecollection to image
  ee$Image$rename(sprintf("tmmx_%03d",1:365))

#max humidity
gridmet.rmax <- ee$ImageCollection("IDAHO_EPSCOR/GRIDMET") %>%
  ee$ImageCollection$filterDate("2010-01-01", "2011-01-01") %>%
  ee$ImageCollection$map(function(x) x$select("rmax")) %>% # Select only precipitation bands
  ee$ImageCollection$toBands() %>% # from imagecollection to image
  ee$Image$rename(sprintf("rmax_%03d",1:365))
#wind speed
gridmet.vs <- ee$ImageCollection("IDAHO_EPSCOR/GRIDMET") %>%
  ee$ImageCollection$filterDate("2010-01-01", "2011-01-01") %>%
  ee$ImageCollection$map(function(x) x$select("vs")) %>% # Select only precipitation bands
  ee$ImageCollection$toBands() %>% # from imagecollection to image
  ee$Image$rename(sprintf("vs_%03d",1:365))

#wind direction
gridmet.th <- ee$ImageCollection("IDAHO_EPSCOR/GRIDMET") %>%
  ee$ImageCollection$filterDate("2010-01-01", "2011-01-01") %>%
  ee$ImageCollection$map(function(x) x$select("th")) %>% # Select only precipitation bands
  ee$ImageCollection$toBands() %>% # from imagecollection to image
  ee$Image$rename(sprintf("th_%03d",1:365))

ee_pr <- ee_extract(x = gridmet.pr, y =facility['ORISPL_CODE'], sf = FALSE)
ee_tmmx <- ee_extract(x = gridmet.tmmx, y = facility['ORISPL_CODE'], sf = FALSE)
ee_rmax <- ee_extract(x = gridmet.rmax, y = facility['ORISPL_CODE'], sf = FALSE)
ee_vs<- ee_extract(x = gridmet.vs, y = facility['ORISPL_CODE'], sf = FALSE)
ee_th <- ee_extract(x = gridmet.th, y = facility['ORISPL_CODE'], sf = FALSE)

#wider data to longer data
ee_pr_long <- ee_pr %>%
  pivot_longer(-ORISPL_, names_to = "day", values_to = "pr") %>% mutate(day, day=gsub("PP_", "", day))

# ee_nc_pr %>% pivot_longer(cols = starts_with("PP"), names_to = "day", values_to = "pr") %>% mutate(day, day=gsub("PP_", "", day))
ee_tmmx_long <- ee_tmmx %>%
  pivot_longer(-ORISPL_, names_to = "day", values_to = "tmmx") %>% mutate(day, day=gsub("tmmx_", "", day))
ee_rmax_long <- ee_rmax %>%
  pivot_longer(-ORISPL_, names_to = "day", values_to = "rmax") %>% mutate(day, day=gsub("rmax_", "", day))
ee_vs_long <- ee_vs %>%
  pivot_longer(-ORISPL_, names_to = "day", values_to = "vs") %>% mutate(day, day=gsub("vs_", "", day))
ee_th_long <- ee_th %>%
  pivot_longer(-ORISPL_, names_to = "day", values_to = "th") %>% mutate(day, day=gsub("th_", "", day))


met_2010 <- Reduce(function(x, y) merge(x, y, all=TRUE),
                   list(ee_pr_long, ee_tmmx_long, ee_rmax_long, ee_vs_long, ee_th_long))
#converting day to date
met_2010$year <- 2010
met_2010$day <- as.numeric(met_2010$day)

day <- as.vector(unique(met_2010$day))
year <- as.vector(rep(2010, 365))

date.create <- data.frame(day, year)
date.create$origin <- as.Date(paste0(date.create$year, "-01-01"),tz = "UTC") - days(1)
date.create$date <- as.Date(date.create$day, origin = date.create$origin, tz = "UTC")
date.create <- date.create %>% dplyr::select(-origin)

met_2010 <- merge(met_2010, date.create, by= c("day", "year")) %>% dplyr::select(-day, -year)
# met_2010 <- met_2010 %>% rename (ORISPL_CODE=ORISPL_) %>% dplyr::select(-day, -year)


