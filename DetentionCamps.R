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
td_camps <- readOGR("data/td_camps_original.shp")
td_camps_sf <- st_read("data/td_camps_original.shp")


## SENTINEL 2 DATA
# from sentinel hub: cloud cover less than 1%
# dates: march 2017 - before construction started, march 2021 - most recent data

# import multi-band raster stack as rasterbrick
sen2017 <- brick("data/S2_L2A_2017_06_10_stack_allbands.tif")

# rather take summer month to show cropland/ vegetated areas for land cover distinction ??
sen2020 <- brick("data/S2_L2A_2020_06_02_stack_allbands.tif")

# rename bands - only bands with spatial resolution of 10m or 20m
names(sen2017) <- c("B1","B2","B3","B4","B5","B6","B7","B8","B8a","B9","B11","B12")
names(sen2020) <- c("B1","B2","B3","B4","B5","B6","B7","B8","B8a","B9","B11","B12")


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



#### REPROJECTION ####

# quick plotting
plot(sen2017)
plot(sen2020)

# check raster values
vals <- getValues(sen2020)
hist(vals)

# reproject dem
# dem_utm <- projectRaster(dem, crs = crs(sen2020))



#### CROPPING ####
# crop dem to same extent as raster data
dem_crop <- crop(dem, extent(sen2020))

# resample dem to 10m spatial resolution of sentinel data
dem_10m <- resample(dem_crop, sen2020)

plot(dem_10m)

sen2020 <- crop(sen2020, extent(dem_crop))
sen2017 <- crop(sen2017, extent(dem_crop))



#### PLOTTING ####
plotRGB(sen2017, 3, 2, 1, stretch = "lin")
plotRGB(sen2020, 3, 2, 1, stretch = "lin")

# false color
plotRGB(sen2020, 4, 3, 2, stretch = "lin")


# plot AOI 
ggRGB(sen2020, stretch = "sqrt") +
  geom_sf(data=td_aoi_sf, aes(alpha = 0.2), fill = "red")



#### spectral indices ####
# NDVI: -1 to 1
ndvi_2020 <- spectralIndices(sen2020, red = 3, nir = 4, indices = "NDVI")
plot(ndvi_2020)
ndvi_2016 <- spectralIndices(sen2017, red = 3, nir = 4, indices = "NDVI")
# idea: mask out all high NDVI values

# SAVI
savi_2020 <- spectralIndices(sen2020, red = 3, nir = 4, indices = "SAVI")
plot(savi_2020)

# MSAVI
msavi_2020 <- spectralIndices(sen2020, red = 3, nir = 4, indices = "MSAVI")
plot(msavi_2020)

## urban indices
# NDBI (Normalized Difference Built-Up Index): SWIR(Band11)-NIR(Band8)/ SWIR(Band11)+NIR(Band8)
ndbi <- (sen2020$B12 - sen2020$B8) / (sen2020$B12 + sen2020$B8)


# calculate terrain features
dem_slope <- terrain(dem_10m, opt = "slope")
dem_aspect <- terrain(dem_10m, opt = "aspect")
dem_roughness <- terrain(dem_10m, opt = "roughness")

plot(dem_slope)



#### supervised classification ####
# Random Forest
sc_td <- readOGR("data/trainingdata.gpkg")

plot(sen2020[[1]])
plot(sc_td, add = T)

sc <- superClass(sen2020, trainData = sc_td, responseCol = "name")
plot(sc$map)
plot(sc_td, add = T)




#### Unsupervised classification / clustering ####
# how different input parameters & masking (water, mountain) improve classification
# classification scheme: 3 classes - built-up, soil, grass/agricultural fields (vegetation)
# in camps only soil & built up

#### TODO: clara ####
# https://www.r-exercises.com/2018/02/28/advanced-techniques-with-raster-data-part-1-unsupervised-classification/?__cf_chl_jschl_tk__=e0d64a516348541e9eefb7f45b2fefca5e2a3a45-1616286273-0-Aaf42iAazWGAz8M2krel_3yvK1a0LIG80ZUpKO9Nz9GjUHebcOua39Gj7RCYn7gx3nZbKkYJn2Rr-Wud24X3b3e1aLpF8YvCjmyvy77-iKtIhC2cfxpoKjuugGhLiHKlat61Tlwq3rZVjYsXrQ5KNW6qnDk87aG5slxp3CYB3KzRcHiD_lmx95TalI5AdZh9OEocSWYK6_jI__UwYi_vd7YNEG_4say-21_RRTpSPfrGpzVaKI-97f9HoatzjcW3lL26UOt5cp44ooXL9LJ66xYH8fqapGpzZhC1EJior3hMZQvNJc9kGKU6Kr0AYx8fUQ7K3wO-CO1SqQxvxE2o-2wClL-Lq4jtHH8E5vhrRY4I2w_BkNVrn0otwhloQwa-FB8sGoyiRGMyp1IUFjP0FU47jLGOaFQU8zG76i3G8ZY9O8ev-Gp--3Wv_ws8dmV7nQ

# https://rspatial.org/raster/rs/4-unsupclassification.html
# http://remote-sensing.org/unsupervised-classification-with-r/

## k means
# 2017
set.seed(1)
uc_17 <- unsuperClass(sen2017_stretch, nClasses = 3, nStarts = 50, nSamples = 10000)
plot(uc_17$map)

# 2020
set.seed(2)
uc_20 <- unsuperClass(sen2020_stretch, nClasses = 3, nStarts = 50, nSamples = 10000)
plot(uc_20$map)


## k means (2)
# 1. only Sentinel-2 raster
set.seed(11)
uc <- unsuperClass(sen2020, nClasses = 3, nStarts = 50, nSamples = 10000)
plot(uc$map)

# 2. additional NDVI as input for classification
# stack data before running classification
stack_ndvi <- stack(sen2020, ndvi_2020)
set.seed(22)
uc_ndvi <- unsuperClass(stack_ndvi, nClasses = 3, nStarts = 50, nSamples = 10000)
plot(uc_ndvi$map)

# 3a.DEM
stack_dem <- stack(sen2020, dem_10m)
set.seed(33)
uc_dem <- unsuperClass(stack_dem, nClasses = 3, nStarts = 50, nSamples = 10000)
plot(uc_dem$map)

# 3b. additional DEM features as input for classification
stack_dem_feat <- stack(sen2020, dem_slope, dem_aspect, dem_roughness)
set.seed(44)
uc_dem_feat <- unsuperClass(stack_dem_feat, nClasses = 3, nStarts = 50, nSamples = 10000)
plot(uc_dem_feat$map)

# 4. textural metrics as input
stack_glcm <- stack(sen2020, glcm$glcm_contrast)
set.seed(55)
uc_glcm <- unsuperClass(glcm$glcm_contrast, nClasses = 3, nStarts = 50, nSamples = 10000, norm = T)
plot(uc_glcm$map)



# for multiple features with different scales - normalize data (substract mean & divide by standard deviation)
stack_dem_feat <- stack(sen2020, dem_10m)
set.seed(55)
uc <- unsuperClass(sen2020, nClasses = 3, nStarts = 50, nSamples = 10000, norm = T)


# stack classifications & plot for comparison
class_stack <- stack(uc$map, uc_ndvi$map, uc_dem$map, uc_dem_feat$map, uc_glcm$map)
names(class_stack) <- c("only raster","raster + ndvi","raster + dem","raster + dem features", "raster + contrast metric")

plot(class_stack)

#### TODO idea: aggregate pixels to objects - via majority filter? ####


#### Validation ####
# compare accuracy of different classifications
# check if classification overlays with camp training data
# validateMap(uc$map, td_aoi, responseCol = , nSamples = 500, mode = "classification", classMapping = NULL)

# crop classification output to aoi
uc_aoi <- crop(uc$map, extent(td_aoi_sf))
uc_ndvi_aoi <- crop(uc_ndvi$map, extent(td_aoi_sf))
uc_dem_aoi <- crop(uc_dem$map, extent(td_aoi_sf))
uc_dem_feat_aoi <- crop(uc_dem_feat$map, extent(td_aoi_sf))
sc_aoi <- crop(sc$map, extent(td_aoi_sf))

uc_aoi_stack <- stack(uc_aoi, uc_ndvi_aoi, uc_dem_aoi, uc_dem_feat_aoi, sc_aoi)
names(uc_aoi_stack) <- c("UC Sen2020","UC Sen2020 + NDVI", "UC Sen2020 + DEM", "UC Sen2020 + DEM features", "SC")
plot(uc_aoi_stack)


#### TODO####
# hierarchical clustering
# first: classify urban, soil & vegetated areas
# mask out other classes than urban
# further classify urban


# idea: Threshold Based Raster Classification
# http://neonscience.github.io/neon-data-institute-2016//R/classify-by-threshold-R/



#### OBIA ####
# segmentation
# divide image into superpixels - group of pixels which are similar in color and other low level properties
library(OpenImageR)
# https://www.r-bloggers.com/2020/03/analyzing-remote-sensing-data-using-image-segmentation/

# https://fickse.wordpress.com/2015/06/18/quick-and-dirty-object-based-segmentation-in-r/
# segment image & use properties of pixel groups (average color, or variablity, or texture) as classification input


# simple: threshold of one band
# blue band - best results
plot(sen2020[[1]])
plot(sen2020[[1]] >= 10000) 

# green band
plot(sen2020[[2]])
plot(sen2020[[2]] >= 11000) 

# red band
plot(sen2020[[3]])
plot(sen2020[[3]] >= 14000)

# NIR band
plot(sen2020[[4]])
plot(sen2020[[4]] >= 17000)

# dem
plot(dem_10m)
plot(dem_10m < 1200)

#### TODO #### account for super high reflectance of metal roof? - illumination correction? radcor "illu"


## image segmentation
# do segmentation in QGIS ?

# https://www.r-bloggers.com/2020/03/analyzing-remote-sensing-data-using-image-segmentation/
# superpixels - group of pixels similar to each other in color & other low level properties
# simple linear iterative clustering
library(OpenImageR)

# use bands green,red & NIR
array_sen2020 <- raster::as.array(sen2020[[2:4]])
region_slic <- superpixels(input_image = array_sen2020, method = "slic", superpixel = 80,
                          compactness = 30, return_slic_data = TRUE,
                          return_labels = TRUE, write_slic = "",
                          verbose = FALSE)

sen2020_jpg <- readImage("data/gif_images/Sentinel-2 L1C image on 2020-05-15.jpg")
region_slic <- superpixels(input_image = sen2020_jpg, method = "slic", superpixel = 60,
                           compactness = 3, return_slic_data = TRUE,
                           return_labels = TRUE, write_slic = "",
                           verbose = FALSE)

imageShow(region_slic$slic_data)
plot(region_slic$slic_data)

str(ndvi_2020)

# convert ndvi data into matrix
ndvi_mat <- matrix(ndvi_2020@data@values,
                   nrow = ndvi_2020@nrows,
                   ncol = ndvi_2020@ncols, byrow = TRUE)






#### masking ####
# idea: flag image with probability that pixel belongs to a camp, according to certain variables: slope, ndvi, high change between 2016 and 2018
# alternatively via thresholding:
# http://neonscience.github.io/neon-data-institute-2016//R/mask-raster-threshold-R/


#### Hierarchical classification ####
# based on threshold masking for multiple metrics
# search for areas with low ndvi, low height/slope, high change & high textural variation

## 1. ndvi - filter out vegetated areas and water
# create mask - filter out high NDVI values (vegetation) & low NDVI values (water)
ndvi_mask <- ndvi_2020
ndvi_mask[((ndvi_mask > 0.5) )] <- NA # vegetated areas
ndvi_mask[(ndvi_mask < -0.1)] <- NA # water
plot(ndvi_mask)

# filter raster data according to NDVI mask
sen2020_ndvi_mask <- mask(sen2020, ndvi_mask)
plot(sen2020_ndvi_mask)


## 2. dem - height values
# create mask - filter out high height values via DEM
plot(dem_10m)
hist(dem_10m)

dem_mask <- dem_10m
dem_mask[dem_mask > 1170] <- NA
plot(dem_mask)

# filter raster data according to DEM mask
sen2020_dem_mask <- mask(sen2020, dem_mask)
plot(sen2020_dem_mask)


## 3. CVA - filter out areas with little change between 2017 and 2020
# different values ranges of DN values for 2017 & 2020 image - stretch images to value range of 0-255 (8 bit)
# sen2017_stretch <- stretch(sen2017, minv=0, maxv=255, minq=0, maxq=1)
# sen2020_stretch <- stretch(sen2020, minv=0, maxv=255, minq=0, maxq=1)

# as difference between band 1 (blue) and 4 (NIR) highest for urban
cva <- rasterCVA(sen2017[[c(1,4)]], sen2020[[c(1,4)]])
plot(cva)

hist(cva$angle)
hist(cva$magnitude)

# mask areas with high change angle & magnitude, filter out areas with low change
cva_mask <- cva
cva_mask[cva_mask$angle < 50] <- NA
cva_mask[cva_mask$magnitude < 4000] <- NA
plot(cva_mask)

sen2020_cva_mask <- mask(sen2020, cva_mask)
plot(sen2020_cva_mask)


## 4. texture
## PCA
pca_20 <- rasterPCA(sen2020)$map

# 3 x 3 moving window (30 x 30 m) (as spatial resolution (=pixel size) of 10m)
# 90 m2 as minimum building size?
# or minimum camp size? - 29 * 29
min(td_camps$area) # 9510 m²

## Focal 
window <- matrix(1, nrow = 91, ncol = 91)
# variance within each window
sen2020_pca_var <- focal(pca_20[[1]], w = window, fun = var)
plot(pca_20[[1]])
plot(sen2020_pca_var)


## GLCM metrics
library(glcm)
glcm <- glcm(pca_20[[1]], window = c(3,3), statistics = c("mean", "variance", "homogeneity", "contrast",
                                                       "dissimilarity", "entropy", "second_moment", "correlation"))
plot(glcm) # metric contrast shows highest variation for camps

# mask areas with low homogeneity & high contrast
plot(glcm$glcm_contrast)
hist(glcm$glcm_contrast)

# create mask - filter out high areas
glcm_mask <- glcm$glcm_contrast
glcm_mask[glcm_mask < 2] <- NA
plot(glcm_mask)

sen2020_glcm_mask <- mask(sen2020, glcm_mask)
plot(sen2020_glcm_mask)


## final classification output
# 1. NDVI
sen2020_class <- sen2020_ndvi_mask

# 2. DEM
sen2020_class_dem <- mask(sen2020_class, dem_mask)

# 3. CVA
sen2020_class_cva <- mask(sen2020_class_dem, cva_mask)

# 4. texture
sen2020_class_tex <- mask(sen2020_class_cva, glcm_mask)

# plot classification output
plot(sen2020_class_tex)
plotRGB(sen2020_class_tex, 3, 2, 1, stretch = "lin")



# #### final classification output ####
# # identify mountainous areas as class 1
# sen2020_class <- dem_mask
# sen2020_class[is.na(sen2020_class)] <- 1
# sen2020_class[sen2020_class != 1] <- NA
# 
# ## search for areas with low height/slope, high reflectance, high contrast, high change & low NDVI
# # slope/height
# sen2020_class <- sen2020
# sen2020_class[dem_10m > 1170] <- NA



#### Change Detection ####

## CVA with masked out mountains
pca_mount <- rasterPCA(sen2020_dem_mask)$map
cva_mount <- rasterCVA(pca_mount[[1:2]], pca_mount[[3:4]])
plot(cva_mount)



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