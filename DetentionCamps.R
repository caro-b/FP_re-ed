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
library(sp)


#### DATA INPUT ####

# ## OSM
# # camp locations & their area
# 
# 
# # bounding box for the area of Xinjiang, using polygon shaped output
 bb <- getbb("Xinjiang", format_out = "polygon")
# 
# # download camps as features
# # convert bb to an overpass query object (API)
# camps_osm <- opq(bb) %>%
#   add_osm_feature(key = "prison_camp", value = "re-education") %>%
#   # output as simple flibrary(osmdata)eatures object (or R spatial (sp) - osmdata_sp()) - advantage of sf: use geom_sf() for ggplot2
#   osmdata_sf()

# area() for osm


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


# ggR(raster_xianjiang) +
#   geom_polygon(data=xianjiang, aes(x=long, y=lat), alpha=0.2, col = "pink", fill ="pink") +
#   geom_sf(data=camps_osm$osm_polygons, aes(fill="red"), col = "red", size = 2)

# make interactive map with leaflet

ggR(raster_xianjiang) +
  geom_polygon(data=xianjiang, aes(x=long, y=lat), alpha=0.2, col = "pink", fill ="pink") +
  geom_point(data=td_camps, aes(x=long, y=lat), col = "red", size = 2)
compareCRS(raster_xianjiang, td_camps)


## CAMP DATA
# https://xjdp.aspi.org.au/data/?tab=datasets#resources;xinjiangs-detention-facilities
# v1 from 24.09.2020
campdata <- read_csv("data/CampDataset_v1.csv")

# training data
td_camps<- readOGR("data/td_camps_original.shp")
td_camps_sf <- st_read("data/td_camps_original.shp")


## SENTINEL 2 DATA
# from sentinel hub: cloud cover less than 1%
# dates: march 2017 - before construction started, march 2021 - most recent data

# import multi-band raster stack as rasterbrick
sen2017 <- brick("data/S2_L2A_2017_06_10_stack_B2348.tif")
# rather take summer month to show cropland/ vegetated areas for land cover distinction ??
sen2020 <- brick("data/S2_L2A_2020_06_22_stack_B2348.tif")

## STRM DEM
# import rasterlayer
dem <- raster("data/SRTM_N43E088.tif")

plot(dem)



#### DATA CLEANING ####
## td_camps
typeof(td_camps$area)
td_camps$area <- as.numeric(td_camps$area)
sort(td_camps$area)

# calculate area (double check with QGIS calculation) & google maps
td_camps$area_sqm <- area(td_camps)

str(td_camps@data)
glimpse(td_camps@data)
td_camps_v1 <- td_camps

# drop not needed chinese name columns
drop <- c("name","name_zh.Ha","name_zh._1","alt_name","wikidata","addr_stree")
td_camps <- td_camps[,!(names(td_camps) %in% drop)]


## campdata 
# show NAs across columns
sort(sapply(campdata, function(x) sum(is.na(x))))

# delete columns with all NAs
campdata <- dplyr::select(campdata, -c(Photos, Videos))

# check for duplicates
duplicated(campdata)
duplicated(campdata[c("Name_Code","Lat","Long")])

str(campdata)

campdata_v1 <- campdata

# delete irrelevant columns: numbering in other dataset (shawn), distance to country centre, distance to pre-school, notes on security/recreational features, visitors, evidence, footage, decomissioning, highlight, desecuritisation
campdata <- dplyr::select(campdata, -c(7:8, 10, 15:19, 21:27, 37, 41:42))

# check correct types
str(campdata)

# distances as numeric
campdata$`Distance to Industrial Park` <- as.numeric(campdata$`Distance to Industrial Park`)
campdata$`Distance to residential buildings` <- as.numeric(campdata$`Distance to residential buildings`)
table(campdata$`Distance to Industrial Park`, useNA = "ifany")
table(campdata$`Distance to residential buildings`, useNA = "ifany")
# combine distances - distance to built-up area
# use smallest distance
campdata$DistanceBuiltup <- pmax(campdata$`Distance to Industrial Park`, campdata$`Distance to residential buildings`, na.rm = T)
table(campdata$DistanceBuiltup, useNA = "ifany")

# number of buldings as numeric
campdata$`Number of Buildings in 2017` <- as.numeric(campdata$`Number of Buildings in 2017`)
campdata$`Number of Buildings in 2018` <- as.numeric(campdata$`Number of Buildings in 2018`)
campdata$`Number of Buildings in 2019` <- as.numeric(campdata$`Number of Buildings in 2019`)
campdata$`Number of Buildings in 2020` <- as.numeric(campdata$`Number of Buildings in 2020`)

# date
library(zoo)
# use as.yearmon function as as.Date requires a day
campdata$`Date of Latest Sat Imagery` <- as.yearmon(campdata$`Date of Latest Sat Imagery`, format ="%y-%B")

# factors - categorical data
campdata$Tier <- as.factor(campdata$Tier)

# usage prior to 2017
unique(campdata$`Usage prior to 2017`)
campdata$`Usage prior to 2017`[campdata$`Usage prior to 2017` %in% c("?","N/A")] <- NA
campdata$`Usage prior to 2017`[campdata$`Usage prior to 2017` == "School?"] <- "School"
campdata$`Usage prior to 2017`[campdata$`Usage prior to 2017` %in% c("Old prison?","Prison?")] <- "Prison"
campdata$`Usage prior to 2017`[campdata$`Usage prior to 2017` %in% c("Residential?","Residential buildings","Residential Buildings","Incomplete housing project")] <- "Residential"
campdata$`Usage prior to 2017`[campdata$`Usage prior to 2017` %in% c("Industrial park","Factory")] <- "Industrial"
# combine park & barren into one class as 10m resolution probably won't allow distinction
campdata$`Usage prior to 2017`[campdata$`Usage prior to 2017` %in% c("Park","Barren","This looked like some sort of ruins","Very weird ramp below ground level")] <- "Park/Barren"
# new column with land cover classes distinguishable in satellite data - built-up, barren (little vegetation) & farmland
# combine different buildings into class built-up as 10m resolution probably won't allow distinction
campdata$PriorUsage <- campdata$`Usage prior to 2017` 
campdata$PriorUsage[campdata$`Usage prior to 2017` %in% c("Four suspicious buildings","Government building","Industrial","Residential","School","Sports facility","Greenhouses","Camp","Prison")] <- "Built-up"
campdata$PriorUsage[campdata$`Usage prior to 2017` == "Desert"] <- "Park/Barren"
table(campdata$PriorUsage)



### EXPLORATORY ANALYSIS ####

## spatially join campdata & camp td
# convert campdata into Spatial points df
nc <- ncol(campdata)
campdata_sp <- SpatialPointsDataFrame(coords= campdata[c(4,3)], data= campdata[c(1,2,5:nc)], proj4string = crs(td_camps))

# # spatial join - points of campdata located in td polygons
# camps_join <- over(campdata_sp,td_camps,returnList = TRUE)

# convert to sf
campdata_sf <- st_as_sf(campdata_sp)
td_camps_sf <- st_as_sf(td_camps)
# inner join
camps_join <- st_join(campdata_sf,td_camps_sf, left = F)

str(camps_join)
glimpse(camps_join)

## detect relevant features of camps ##

## categorical

# Prior usage (land use)
ggplot(data = camps_join) +
  # sort values by occurence
  geom_bar(mapping = aes(x = forcats::fct_infreq(`Usage prior to 2017`)))

# Prior usage (land cover)
ggplot(data = camps_join) +
  # sort values by occurence
  geom_bar(mapping = aes(x = forcats::fct_infreq(PriorUsage))) # mostly barren land


## continuous

# (nearest) distance to built-up areas
ggplot(data = camps_join) +
  geom_histogram(mapping = aes(x = DistanceBuiltup))

# add median line
ggplot(data = camps_join) +
  geom_density(mapping = aes(x = DistanceBuiltup)) +
  geom_vline(aes(xintercept = median(camps_join$DistanceBuiltup, na.rm = T)), color = "blue", linetype = 4, size = 1) +
  geom_text(aes(x= median(camps_join$DistanceBuiltup, na.rm = T), label= "median", y=0.31), colour="red", text=element_text(size=11)) +
  geom_text(aes(x= median(camps_join$DistanceBuiltup, na.rm = T), label= median(camps_join$DistanceBuiltup, na.rm = T), y=0.3), colour="red", text=element_text(size=11))

# # quantile
# probs <- c(0.25, 0.5, 0.75, 1)
# quantiles <- quantile(camps_join$DistanceBuiltup, prob=probs, na.rm = T)
# camps_joina$quant <- factor(findInterval(camps_join$DistanceBuiltup, quantiles))
# 

# area
ggplot(data = camps_join) +
  geom_histogram(mapping = aes(x = area)) +
  geom_vline(aes(xintercept = median(camps_join$area, na.rm = T)), color = "red", linetype = 4, size = 1) +
  geom_text(aes(x= median(camps_join$area, na.rm = T), label= "median", y=30), colour="red", text=element_text(size=11)) +
  geom_text(aes(x= median(camps_join$area, na.rm = T), label= median(camps_join$area, na.rm = T), y=28), colour="red", text=element_text(size=11))
  

## select biggest camp as study area
td_aoi <- td_camps[td_camps$area == max(td_camps$area),]
td_aoi_sf <- td_camps_sf[td_camps_sf$area == max(td_camps_sf$area),]

# # join with campdata where long lat lies in extent of aoi
# campdata[(campdata$Long >= extent(td_aoi)[1,] & campdata$Long <= extent(td_aoi)[2,]) & 
#            (campdata$Lat >= extent(td_aoi)[3,] & campdata$Lat <= extent(td_aoi)[4,]),]
# 



# #### PREPROCESSING ####
# library(sen2r)
# # correct Sentinel2 L1C data from 2016
# sen2cor()
# 
# # download archive data (as scihub archive download isn't working)
# write_scihub_login('cbEO', 'van0305GB')
# time_window <- as.Date(c("2016-08-01", "2016-08-04"))
# 
# s2_data <- s2_list(
#   spatial_extent = td_aoi_sf,
#   time_interval = time_window
#   #tile = "45TXJ",
#  # orbit = "5836"
# )
# 
# s2_download(
#   s2_prodlist = s2_data
# )
# 
# s2_order(
#   s2_prodlist = s2_data
# )



#### REPROJECTION ####
# reproject raster data to CRS WGS84 UTM
# sen2017_utm <- projectRaster(sen2017, crs = crs(td_camps))
# sen2020_utm <- projectRaster(sen2020, crs = crs(sen2017))

# reproject td to raster CRS WGS84 UTM 45
# td_aoi_utm <- spTransform(td_aoi, crs(sen2020_utm))
# td_aoi_sf_utm <- st_transform(td_aoi_sf, crs(sen2020_utm))

# quick plotting
plot(sen2017)
plot(sen2020)

# check raster values
vals <- getValues(sen2020)
hist(vals)

# # reproject dem
# dem_utm <- projectRaster(dem, crs = crs(sen2020))



#### CROPPING ####
# crop dem to same extent as raster data
dem_crop <- crop(dem, extent(sen2020))

# resample dem to 10m spatial resolution of sentinel data
dem_10m <- resample(dem_crop, sen2020)

plot(dem_10m)




#### PLOTTING ####
plotRGB(sen2017, 3, 2, 1, stretch = "lin")
plotRGB(sen2020, 3, 2, 1, stretch = "lin")

# plot AOI 
ggRGB(sen2020, stretch = "sqrt") +
  geom_sf(data=td_aoi_sf, aes(alpha = 0.2), fill = "red")



#### spectral indices ####
ndvi_2021 <- spectralIndices(sen2020_utm, red = 3, nir = 4, indices = "NDVI")
plot(ndvi_2021)
# idea: mask out all high NDVI values

# calculate terrain features
dem_slope <- terrain(dem_10m, opt = "slope")
dem_aspect <- terrain(dem_10m, opt = "aspect")
dem_roughness <- terrain(dem_10m, opt = "roughness")

plot(dem_slope)



#### Pixel-based classification ####
# unsupervised classification
unsup_2021 <- unsuperClass(sen2020_utm, nClasses = 4)
plot(unsup_2021$map)

raster_stack <- stack(sen2020_utm, ndvi_2021, dem_slope, dem_aspect, dem_roughness)

# include additional environmental info into classification
unsup_2021_stack <- unsuperClass(raster_stack, nClasses = 4)

par(mfrow = c(2,1))
plot(unsup_2021$map)
plot(unsup_2021_stack$map)

# idea: Threshold Based Raster Classification
# http://neonscience.github.io/neon-data-institute-2016//R/classify-by-threshold-R/

# Supervised classification - RF (most common)



#### OBIA ####
# segmentation
# divide image into superpixels - group of pixels which are similar in color and other low level properties
library(OpenImageR)
# https://www.r-bloggers.com/2020/03/analyzing-remote-sensing-data-using-image-segmentation/

# https://fickse.wordpress.com/2015/06/18/quick-and-dirty-object-based-segmentation-in-r/
# segment image & use properties of pixel groups (average color, or variablity, or texture) as classification input


# simple: threshold of one band
# green band
plot(sen2020[[2]])
plot(sen2020[[2]] < 10000) 

# red band
plot(sen2020[[3]])
plot(sen2020[[3]] < 11000)

# dem
plot(dem_10m)
plot(dem_10m < 1200)

#### TODO #### account for super high reflectance of metal roof?

# PCA to reduce the data
#pca <- rasterPCA(sen2020)

i <- sample(1:ncell(sen2020), size = 900000)
pca <- prcomp(sen2020[i]) 
px <- predict(sen2020, pca, index = 1:4)

# PC 1, 2 & 3 most interesting
plot(px)

# # PC 1 & 2 most important
# plot(pca$map)
# summary(pca$model)


# low pass (low frequency remains) filter on first 2 PCs
p1 <- focal(px[[1]], w = matrix(1/81, 51,51), pad = TRUE, padValue = 0)
p2 <- focal(px[[2]], w = matrix(1/81, 51,51), pad = TRUE, padValue = 0)
p3 <- focal(px[[3]], w = matrix(1/81, 9,9), pad = TRUE, padValue = 0)

plot(p1)
plot(p2)

xy <- xyFromCell(p1,1:ncell(p1))

# fast clustering algorithm (kmeans) for initial pixel groupings
k <- kmeans(cbind(p1[i],p2[i]), 100)
plot(k)

library(FNN) # for assigning each pixel to its K
K <- knnx.index(k$centers, cbind(p1[],p2[]),1)

pk <- px[[1]] # template for raster
pk[] <- k$cluster
# another low-pass filter that preserves edges
pf <- focal(pk, w = matrix(1,3, 3),fun = median, pad = TRUE, padValue = 0)

plot(pf)




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