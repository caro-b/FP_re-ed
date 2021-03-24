# Purpose of script:
# Author: Caroline Busse
# Date: April, 2021
# R version and packages:

# Load required packages
library(dplyr)
library(ggplot2)
library(RStoolbox)
library(raster)
library(rgdal)
library(tidyverse)
library(sf)


#### DATA INPUT ####

# ## OSM
# # camp locations & their area
# library(osmdata)
# 
# # bounding box for the area of Xinjiang, using polygon shaped output
# bb <- getbb("Xinjiang", format_out = "polygon")
# 
# # download camps as features
# # convert bb to an overpass query object (API)
# camps_osm <- opq(bb) %>%
#   add_osm_feature(key = "prison_camp", value = "re-education") %>%
#   # output as simple features object (or R spatial (sp) - osmdata_sp()) - advantage of sf: use geom_sf() for ggplot2
#   osmdata_sf()
# 
# 

# plotting
library(ggmap)

# background map
map <- get_map(getbb("Xinjiang"), source = "osm", color = "color", maptype="satellite")

# naturalearthdata
library(rnaturalearth)
raster_10m <- ne_download(scale = 10, type = 'NE1_HR_LC', category = 'raster')
plot(raster_10m)

ggmap(map) +
 # geom_raster(raster_10m) +
  geom_sf(data = camps_osm$osm_polygons,
          # indicates that the aesthetic mappings of the spatial object osm_polygons has to be used
          inherit.aes = FALSE,
          colour = "red",
          fill = "red",
          alpha = 0.5) +
  labs(x = "", y = "") +
  coord_sf(xlim = c(75.00, 100.00), ylim = c(36.00, 48.00))


# vector data on top of raster data
# download vector data of china
china <- getData("GADM", country = "CHN", level = 1)
china$NAME_1

# filter on extent of Xianjiang
xianjiang <- china[china$NAME_1 == "Xinjiang Uygur",]

# crop raster to extent of vector data
raster_xianjiang <- crop(raster_10m, xianjiang)
# reproject
raster_utm <- projectRaster(raster_xianjiang, crs = crs(td_camps))
xianjiang_utm <- spTransform(xianjiang, crs(td_camps))


# ggR(raster_xianjiang) +
#   geom_polygon(data=xianjiang, aes(x=long, y=lat), alpha=0.2, col = "pink", fill ="pink") +
#   geom_sf(data=camps_osm$osm_polygons, aes(fill="red"), col = "red", size = 2)

# make interactive map with leaflet

ggR(raster_utm) +
  geom_polygon(data=xianjiang_utm, aes(x=long, y=lat), alpha=0.2, col = "pink", fill ="pink") +
  geom_sf(data=td_camps_sf, aes(fill="red"), col = "red", size = 2)

# test with long lat
td_camps_longlat <- st_transform(td_camps_sf, crs(raster_xianjiang))
ggR(raster_xianjiang) +
  geom_polygon(data=xianjiang, aes(x=long, y=lat), alpha=0.2, col = "pink", fill ="pink") +
  geom_sf(data=td_camps_longlat, aes(fill="red"), col = "red", size = 2)



## CAMP DATA
# https://xjdp.aspi.org.au/data/?tab=datasets#resources;xinjiangs-detention-facilities
# v1 from 24.09.2020
campdata <- read_csv("data/CampDataset_v1.csv")

# join with OSM area data

# training data
td_camps <- readOGR("data/td_camps_.shp")
td_camps_sf <- st_read("data/td_camps_.shp")


## SENTINEL 2 DATA
# from sentinel hub: cloud cover less than 1%
# dates: march 2017 - before construction started, march 2021 - most recent data

# import multi-band raster stack as rasterbrick
sen2017 <- brick("data/S2_L2A_2017_03_02_stack_B2348_UTM.tif")
sen2021 <- brick("data/S2_L2A_2021_03_06_stack_B2348.tif")

## STRM DEM
# import rasterlayer
dem <- raster("data/SRTM_N43E088.tif")

plot(dem)



#### REPROJECTION ####
# reproject raster data to CRS WGS84 UTM
sen2017_utm <- projectRaster(sen2017, crs = crs(td_camps))
sen2021_utm <- projectRaster(sen2021, crs = crs(td_camps))

# quick plotting
plot(sen2017_utm)
plot(sen2021_utm)

# check raster values
vals <- getValues(sen2021_utm)
hist(vals)

# reproject dem
dem_utm <- projectRaster(dem, crs = crs(td_camps))



#### CROPPING ####
# crop dem to same extent as raster data
dem_crop <- crop(dem_utm, extent(sen2021_utm))

# resample dem to 10m spatial resolution of sentinel data
dem_10m <- resample(dem_crop, sen2021_utm)

plot(dem_10m)

# idea: mask out all high slopes



#### DATA CLEANING ####
## td_camps
typeof(td_camps$area)
td_camps$area <- as.numeric(td_camps$area)
sort(td_camps$area)

# select biggest camp as study area
td_aoi <- td_camps_sf[td_camps_sf$area == max(td_camps_sf$area),]


## campdata 
# show NAs across columns
sort(sapply(campdata, function(x) sum(is.na(x))))

# delete columns with all NAs
campdata <- dplyr::select(campdata, -c(Photos, Videos))

str(campdata)

campdata_v1 <- campdata

# delete irrelevant columns: numbering in other dataset (shawn), distance to country centre, distance to pre-school, notes on security/recreational features, visitors, evidence, footage, decomissioning, highlight, desecuritisation
campdata <- dplyr::select(campdata, -c(7:8, 10, 15:19, 21:27, 37, 41:42))

# explanation of tier variable
# tier 1: lower security re‑education camps
# tier 2: dedicated re‑education camps
# tier 3: detention centres
# tier 4: maximum security prisons
# see metadata: https://xjdp.aspi.org.au/resources/#resources;documenting-xinjiangs-detention-system

# check correct types
str(campdata)

# distances as numeric
campdata$`Distance to Industrial Park` <- as.numeric(campdata$`Distance to Industrial Park`)
campdata$`Distance to residential buildings` <- as.numeric(campdata$`Distance to residential buildings`)

# number of buldings as numeric
campdata$`Number of Buildings in 2017` <- as.numeric(campdata$`Number of Buildings in 2017`)
campdata$`Number of Buildings in 2018` <- as.numeric(campdata$`Number of Buildings in 2018`)
campdata$`Number of Buildings in 2019` <- as.numeric(campdata$`Number of Buildings in 2019`)
campdata$`Number of Buildings in 2020` <- as.numeric(campdata$`Number of Buildings in 2020`)

# date
library(zoo)
# use as.yearmon function as as.Date requires a day
campdata$`Date of Latest Sat Imagery` <- as.yearmon(campdata$`Date of Latest Sat Imagery`, format ="%y-%B")



### EXPLORATORY ANALYSIS ####

# spatially join non-spatial ASPI campdata with spatial camp polygons
sp::merge(td_camps, campdata, by.x = )



#### PLOTTING ####
plotRGB(sen2017_utm, 3, 2, 1, stretch = "lin")
plotRGB(sen2021_utm, 3, 2, 1, stretch = "lin")

ggR(sen2017) +
 # geom_polygon(data=xianjiang_utm, aes(x=long, y=lat), alpha=0.2, col = "pink", fill ="pink") +
  geom_sf(data=td_aoi, aes(fill="red"), col = "red", size = 2)



#### spectral indices ####
ndvi_2021 <- spectralIndices(sen2021_utm, red = 3, nir = 4, indices = "NDVI")
plot(ndvi_2021)
# idea: mask out all high NDVI values

# calculate terrain features
dem_slope <- terrain(dem_10m, opt = "slope")
dem_aspect <- terrain(dem_10m, opt = "aspect")
dem_roughness <- terrain(dem_10m, opt = "roughness")

plot(dem_slope)



#### classification ####
# unsupervised classification
unsup_2021 <- unsuperClass(sen2021_utm, nClasses = 4)
plot(unsup_2021$map)

raster_stack <- stack(sen2021_utm, ndvi_2021, dem_slope, dem_aspect, dem_roughness)

# include additional environmental info into classification
unsup_2021_stack <- unsuperClass(raster_stack, nClasses = 4)

par(mfrow = c(2,1))
plot(unsup_2021$map)
plot(unsup_2021_stack$map)

# idea: Threshold Based Raster Classification
# http://neonscience.github.io/neon-data-institute-2016//R/classify-by-threshold-R/


#### OBIA ####
# segmentation
# divide image into superpixels - group of pixels which are similar in color and other low level properties
library(OpenImageR)
# https://www.r-bloggers.com/2020/03/analyzing-remote-sensing-data-using-image-segmentation/


#### clustering ####
# kmeans
# https://rspatial.org/raster/rs/4-unsupclassification.html
# https://www.r-exercises.com/2018/02/28/advanced-techniques-with-raster-data-part-1-unsupervised-classification/?__cf_chl_jschl_tk__=e0d64a516348541e9eefb7f45b2fefca5e2a3a45-1616286273-0-Aaf42iAazWGAz8M2krel_3yvK1a0LIG80ZUpKO9Nz9GjUHebcOua39Gj7RCYn7gx3nZbKkYJn2Rr-Wud24X3b3e1aLpF8YvCjmyvy77-iKtIhC2cfxpoKjuugGhLiHKlat61Tlwq3rZVjYsXrQ5KNW6qnDk87aG5slxp3CYB3KzRcHiD_lmx95TalI5AdZh9OEocSWYK6_jI__UwYi_vd7YNEG_4say-21_RRTpSPfrGpzVaKI-97f9HoatzjcW3lL26UOt5cp44ooXL9LJ66xYH8fqapGpzZhC1EJior3hMZQvNJc9kGKU6Kr0AYx8fUQ7K3wO-CO1SqQxvxE2o-2wClL-Lq4jtHH8E5vhrRY4I2w_BkNVrn0otwhloQwa-FB8sGoyiRGMyp1IUFjP0FU47jLGOaFQU8zG76i3G8ZY9O8ev-Gp--3Wv_ws8dmV7nQ
# http://remote-sensing.org/unsupervised-classification-with-r/
  
  

#### masking ####
# idea: flag image with probability that pixel belongs to a camp, according to certain variables: slope, ndvi, high change between 2016 and 2018
# alternatively via thresholding:
# http://neonscience.github.io/neon-data-institute-2016//R/mask-raster-threshold-R/


#### change detection ####


#### validation ####
# osm data 



#### visualization ####
# idea: animation with raster images on how camp was built
# jpg images from sentinel hub
library(purrr) 
library(magick)

# make gif to visualize construction of camp
con_gif <- list.files(path = "data/gif_images/", pattern = "*.jpg", full.names = T) %>% 
  map(image_read) %>% 
  image_join() %>% 
  #image_crop("500x500") %>%
  image_annotate("Dabancheng, Ürümqi, Xinjiang (China): March 2017 - March 2020", location = "+10+10", size = 20, color = "white") %>%
  image_animate(fps=2) %>% 
  image_write("construction.gif")


#### statistics ####
# idea: first do statistical analysis on data to see most common features of camps


#### time series ####
# https://www.neonscience.org/resources/learning-hub/tutorials/dc-raster-time-series-r