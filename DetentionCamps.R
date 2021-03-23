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


#### DATA DOWNLOAD ####

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
# # plotting
# library(ggmap)
# 
# # background map
# map <- get_map(getbb("Xinjiang"), source = "osm", color = "color", maptype="satellite")
# 
# # naturalearthdata
# library(rnaturalearth)
# raster_10m <- ne_download(scale = 10, type = 'NE1_HR_LC', category = 'raster')
# plot(raster_10m)
# 
# ggmap(map) +
#  # geom_raster(raster_10m) +
#   geom_sf(data = camps_osm$osm_polygons, 
#           # indicates that the aesthetic mappings of the spatial object osm_polygons has to be used
#           inherit.aes = FALSE,
#           colour = "red",
#           fill = "red",
#           alpha = 0.5) +
#   labs(x = "", y = "") +
#   coord_sf(xlim = c(75.00, 100.00), ylim = c(36.00, 48.00))
# 
# 
# # vector data on top of raster data
# # download vector data of china 
# china <- getData("GADM", country = "CHN", level = 1)
# china$NAME_1
# 
# # filter on extent of Xianjiang
# xianjiang <- china[china$NAME_1 == "Xinjiang Uygur",]
# 
# # crop raster to extent of vector data
# raster_china <- crop(raster_10m, china)
# 
# ggR(raster_china) +
#   geom_polygon(data=xianjiang, aes(x=long, y=lat), alpha=0.2, col = "pink", fill ="pink") +
#   geom_sf(data=camps_osm$osm_polygons, aes(fill="red"), col = "red", size = 2) 
#   
# 
# # make interactive map with leaflet
# 


## CAMP DATA
library(tidyverse)
# https://docs.google.com/spreadsheets/d/e/2PACX-1vR48u6lKYD21gv6mqM-2dV2lL8axuJ3yG5QJr2KNfG6bZNhy2dXDib_ZyFl9QKwvTRP0EBKZPYczwp9/pubhtml#
# v1 from 24.09.2020
campdata <- read_csv("data/CampDataset_v1.csv")

# join with OSM area data

# training data
td_camps <- readOGR("data/td_camps.shp")


## SENTINEL 2 DATA
# from sentinel hub: cloud cover less than 1%
# dates: march 2017 - before construction started, march 2021 - most recent data

# import multi-band raster stack as rasterbrick
sen2017 <- brick("data/S2_L2A_2017_03_02_stack_B2348.tif")
sen2021 <- brick("data/S2_L2A_2021_03_06_stack_B2348.tif")

## STRM DEM
# import rasterlayer
dem <- raster("data/SRTM_N43E088.tif")

plot(dem)



#### REPROJECTION ####
# reproject raster data to CRS WGS84 UTM
sen2017_utm <- projectRaster(sen2017, crs = "+proj=utm +zone=22 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")
sen2021_utm <- projectRaster(sen2021, crs = "+proj=utm +zone=22 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")

# quick plotting
plot(sen2017_utm)
plot(sen2021_utm)

# check raster values
vals <- getValues(sen2021_utm)
hist(vals)

# reproject dem
dem_utm <- projectRaster(dem, crs = crs(sen2021_utm))

# reproject vector data
td_camps_utm <- spTransform(td_camps, crs(sen2021_utm))


#### CROPPING ####
# crop dem to same extent as raster data
dem_crop <- crop(dem_utm, extent(sen2021_utm))

# resample dem to 10m spatial resolution of sentinel data
dem_10m <- resample(dem_crop, sen2021_utm)

plot(dem_10m)

# idea: mask out all high slopes



#### DATA CLEANING ####
typeof(td_camps$area)
td_camps$area <- as.numeric(td_camps$area)
sort(td_camps$area)

# select biggest camp as study area
td_aoi <- td_camps[td_camps$area == max(td_camps$area),]



#### PLOTTING ####
plotRGB(sen2017_utm, 3, 2, 1, stretch = "lin")
plotRGB(sen2021_utm, 3, 2, 1, stretch = "lin")




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