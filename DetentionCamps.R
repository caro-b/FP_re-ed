# Purpose of the script:
# analysis of re-education camps in the Xinjiang region where the Chinese government is interning the Moslem minorities
# until now the camp detection is mainly done manually therefore this script offers methods to detect camps in a more automatic way
# this analysis focuses on the biggest known re-education camp in Dabancheng, but may be enhanced & applied to other & bigger study areas
# Data sources: Natural Earth, OSM, Sentinel2 (ESA), SRTM (NASA), VIIRS (NASA)
# Author: Caroline Busse
# Date: April, 2021
# R version: 4.0.5


#### SETUP ####
## Packages
# install required packages if needed
packagelist <- c("dplyr","glcm","ggmap","ggplot2","magick","osmdata","purrr","raster","readr","rgdal","rgee","rnaturalearth","RStoolbox","sf","sp","tidyverse","zoo")
new.packages <- packagelist[!(packagelist %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# Load required packages
lapply(packagelist, require, character.only = TRUE)


## setup connection with Google Earth Engine
# run this part individually per line (don't run it as full script)
# need to setup a GEE account 
ee_install()
# restart R session afterwards
ee_Initialize()



#### DATA DOWNLOAD ####

## OSM - camp data
# bounding box for the area of Xinjiang, using polygon shaped output
bb <- getbb("Xinjiang", format_out = "polygon")

# download camp areas for validation
# convert bb to an overpass query object (API)
camps_osm <- opq(bb) %>%
add_osm_feature(key = "prison_camp", value = "re-education") %>%  
# output as simple features object (or R spatial (sp) - osmdata_sp()) - advantage of sf: use geom_sf() for ggplot2
osmdata_sp()

# extract polygons to get camp area
camps_osm_pol <- camps_osm$osm_polygons

# camp area in m²
camps_osm_pol$area <- area(camps_osm_pol)

# background map
map <- get_map(getbb("Xinjiang"), source = "osm", color = "color", maptype="satellite")


## Natural Earth 
# raster data
raster_10m <- ne_download(scale = 10, type = 'NE1_HR_LC', category = 'raster')


## GADM - vector data of china
china <- getData("GADM", country = "CHN", level = 1)
china$NAME_1

# filter on extent of Xianjiang
xianjiang <- china[china$NAME_1 == "Xinjiang Uygur",]

# crop raster to extent of vector data
raster_xianjiang <- crop(raster_10m, xianjiang)

compareCRS(raster_xianjiang, camps_osm_pol)

# vector data on top of raster data
ggR(raster_xianjiang) +
  geom_polygon(data=xianjiang, aes(x=long, y=lat), alpha=0.2, col = "pink", fill ="pink") +
  geom_point(data=camps_osm_pol, aes(x=long, y=lat), col = "red", size = 2)


## CAMP DATA
# https://xjdp.aspi.org.au/data/?tab=datasets#resources;xinjiangs-detention-facilities
# v1 from 24.09.2020
campdata <- read_csv("data/CampDataset_v1.csv")


## SENTINEL 2 DATA
# from sentinel hub: cloud cover less than 1%
# dates: june 2017 - before construction started, june 2020 - most construction finished
# summer month to show cropland/ vegetated areas for land cover distinction
sen2017 <- brick("data/sen2017_06_02_cropped.tif")

files20 <- list.files("data/Sen2_2020_06_02", full.names = T, ignore.case = T, pattern = "tiff")
sen2020_stack <- stack(files20)
sen2020 <- brick(sen2020_stack)

# rename bands - only bands with spatial resolution of 10m or 20m
names(sen2017) <- c("B1","B2","B3","B4","B5","B6","B7","B8","B9","B11","B12")
names(sen2020) <- c("B1","B2","B3","B4","B5","B6","B7","B8","B9","B11","B12")


## STRM DEM (digital elevation model)
srtm <- getData('SRTM', lon=88, lat=43)
plot(srtm)


## VIIRS - Nightlights data 
# to map built-up areas
# download from Google Earth Engine

# finding images
point <- ee$Geometry$Point(88.2924,43.3827)
start <- ee$Date("2020-06-01")
finish <- ee$Date("2020-06-30")

filteredCollection <- ee$ImageCollection("NOAA/VIIRS/DNB/MONTHLY_V1/VCMSLCFG")$
  filterBounds(point)$
  filterDate(start, finish)$
  sort("CLOUD_COVER", TRUE)

first <- filteredCollection$first()$select("avg_rad")

# metadata
ee_print(first) 

# Define visualization parameters
vizParams <- list(
  min = 0,
  max = 10,
  bands = "avg_rad"
)

Map$setCenter(88.2924,43.3827, 3)
Map$addLayer(first, vizParams, "VIIRS 2020")

# crop to Xianjiang extent
extent(xianjiang)
# geometry
geometry <- ee$Geometry$Rectangle(
  coords = c(73.5577, 34.33627, 96.36517, 49.17501),
  proj = "EPSG:4326",
  geodesic = FALSE
)

# ee as raster, crop to extent of xinjiang
viirs_raster <- ee_as_raster(first, region=geometry) # also downloads raster as tiff



#### DATA CLEANING ####
## osm camp data
typeof(camps_osm_pol$area)
# round
camps_osm_pol$area <- floor(camps_osm_pol$area)

sort(camps_osm_pol$area)

str(camps_osm_pol@data)
glimpse(camps_osm_pol@data)
camps_osm_pol_v1 <- camps_osm_pol

# drop not needed columns e.g. chinese name
drop <- c("name","addr:street","alt_name","name:zh-Hans","name:zh-Hant","note","ref","note","ref:aspi","ref:etnam","ref:shawn","wikidata")
osm_camps <- camps_osm_pol[,!(names(camps_osm_pol) %in% drop)]


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
# convert campdata into Spatial points data frame
nc <- ncol(campdata)
campdata_sp <- SpatialPointsDataFrame(coords= campdata[c(4,3)], data= campdata[c(1,2,5:nc)], proj4string = crs(osm_camps))

# convert to sf
campdata_sf <- st_as_sf(campdata_sp)
osm_camps_sf <- st_as_sf(osm_camps)

# inner join
camps_join <- st_join(campdata_sf,osm_camps_sf, left = F)

str(camps_join)
glimpse(camps_join)


## detect relevant features of camps ##

# 1. categorical
# Prior usage (land use)
ggplot(data = camps_join) +
  # sort values by occurence
  geom_bar(mapping = aes(x = forcats::fct_infreq(`Usage prior to 2017`)))

# Prior usage (land cover)
ggplot(data = camps_join) +
  # sort values by occurence
  geom_bar(mapping = aes(x = forcats::fct_infreq(PriorUsage))) # mostly barren land

# 2. continuous
# (nearest) distance to built-up areas
ggplot(data = camps_join) +
  geom_histogram(mapping = aes(x = DistanceBuiltup))

# add median line
ggplot(data = camps_join) +
  geom_density(mapping = aes(x = DistanceBuiltup)) +
  geom_vline(aes(xintercept = median(DistanceBuiltup, na.rm = T)), color = "blue", linetype = 4, size = 1) +
  geom_text(aes(x= median(DistanceBuiltup, na.rm = T), label= "median", y=0.31), colour="red", text=element_text(size=11)) +
  geom_text(aes(x= median(DistanceBuiltup, na.rm = T), label= median(DistanceBuiltup, na.rm = T), y=0.3), colour="red", text=element_text(size=11))

# area
ggplot(data = camps_join) +
  geom_histogram(mapping = aes(x = area)) +
  geom_vline(aes(xintercept = median(area, na.rm = T)), color = "red", linetype = 4, size = 1) +
  geom_text(aes(x= median(area, na.rm = T), label= "median", y=30), colour="red", text=element_text(size=11)) +
  geom_text(aes(x= median(area, na.rm = T), label= median(area, na.rm = T), y=28), colour="red", text=element_text(size=11))
  


#### REPROJECTION ####
# check projection
crs(sen2020)
crs(sen2020)
crs(srtm)
crs(viirs_raster)
crs(osm_camps)

# reproject crs
crs(osm_camps) <- crs(sen2020)


## select biggest camp as study area
osm_aoi <- osm_camps[osm_camps$area == max(osm_camps$area),]
osm_aoi_sf <- osm_camps_sf[osm_camps_sf$area == max(osm_camps_sf$area),]



#### CROPPING ####
sen2020 <- crop(sen2020, extent(sen2017))

# crop dem to same extent as raster data
srtm_crop <- crop(srtm, extent(sen2020))
plot(srtm_crop)

# resample srtm to 10m spatial resolution of sentinel data
srtm_10m <- resample(srtm_crop, sen2020)

# crop to extent of raster data
viirs <- crop(viirs_raster, extent(sen2020))
plot(viirs)

# resample srtm to 10m spatial resolution of sentinel data
viirs_10m <- resample(viirs, sen2020)
plot(viirs_10m)



#### PLOTTING ####
plotRGB(sen2017, 4, 3, 2, stretch = "lin")
plotRGB(sen2020, 4, 3, 2, stretch = "lin")

# plot AOI 
ggRGB(sen2020, r=4, g=3, b=2, stretch = "lin") +
  geom_sf(data= osm_aoi_sf, aes(alpha = 0.2), fill = "red")


## animation with raster images on how camp was built
# jpg images from sentinel hub, less than 1% cloud cover
# make gif to visualize construction of camp
con_gif <- list.files(path = "data/gif_images/", pattern = "*.jpg", full.names = T) %>% 
  map(image_read) %>% 
  image_join() %>% 
  #image_crop("500x500") %>%
  image_annotate("Dabancheng, Ürümqi, Xinjiang (China): March 2017 - March 2021", location = "+10+10", size = 20, color = "white") %>%
  image_animate(fps=2) %>% 
  image_write("construction.gif")



#### SPECTRAL INDICES ####
## Vegetation Indices
# 1. NDVI (Normalized Difference Vegetation Index): range frome -1 to 1
ndvi_2020 <- spectralIndices(sen2020, red = 4, nir = 5, indices = "NDVI")
plot(ndvi_2020)
ndvi_2017 <- spectralIndices(sen2017, red = 4, nir = 5, indices = "NDVI")
plot(ndvi_2017)

# 2. SAVI (Soil Adjusted Vegetation Index)
# account for soil background in desert area of Xinjiang
savi_2020 <- spectralIndices(sen2020, red = 4, nir = 5, indices = "SAVI")
plot(savi_2020)
savi_2017 <- spectralIndices(sen2017, red = 4, nir = 5, indices = "SAVI")
plot(savi_2017)

# 3. MSAVI (Modified Soil Adjusted Vegetation Index))
msavi_2020 <- spectralIndices(sen2020, red = 4, nir = 5, indices = "MSAVI")
plot(msavi_2020) # doesn't return better result as camp area detected as slightly vegetated


## Urban Indices
# 1. NDBI (Normalized Difference Built-Up Index): SWIR(Band11)-NIR(Band8) / SWIR(Band11)+NIR(Band8)
ndbi_17 <- (sen2017$B11 - sen2017$B8) / (sen2017$B11 + sen2017$B8)
ndbi_20 <- (sen2020$B11 - sen2020$B8) / (sen2020$B11 + sen2020$B8)
plot(ndbi_20)
plot(ndbi_17)

# 2. NDTI (Normalized Difference Tillage Index): (SWIR1−SWIR2) / (SWIR1+SWIR2)
ndti_20 <- (sen2020$B11 - sen2020$B9) / (sen2020$B11 + sen2020$B9)
plot(ndti_20)
# urban indices fail to clearly identify built-up areas, mountainous areas mixed up


## Terrain Features
srtm_slope <- terrain(srtm_10m, opt = "slope")
srtm_aspect <- terrain(srtm_10m, opt = "aspect")
srtm_roughness <- terrain(srtm_10m, opt = "roughness")

plot(srtm_slope)


## Texture metrics
# PCA (Principal Component Analysis) - reduce dimensionality in data
pca_20 <- rasterPCA(sen2020)$map
plot(pca_20) 

# glcm_pca (grey-level co-occurrence matrices)
glcm_pca <- glcm(pca_20[[1]], window = c(5,5), shift = c(1, 1),
             statistics = c("mean", "variance", "homogeneity", "contrast",
                            "dissimilarity", "entropy", "second_moment", "correlation"))
plot(glcm_pca) 



#### CLASSIFICATION ####
# classification scheme: 4 classes - urban (built-up), soil, grass/agricultural fields (vegetation), water
# in camps only soil & urban

## Supervised classification
# Random Forest
sc_td <- readOGR("data/trainingdata.gpkg")
sc_td_crop <- crop(sc_td, extent(sen2020))

plot(sen2020[[1]])
plot(sc_td_crop, add = T)

sc_20 <- superClass(sen2020, trainData = sc_td_crop, responseCol = "name")
plot(sc_20$map)


## Unsupervised classification / clustering
# k-means
# how different input parameters & prior masking (water, mountain) improve classification

# 1. only Sentinel-2 raster
set.seed(11)
uc <- unsuperClass(sen2020, nClasses = 4, nStarts = 50, nSamples = 10000)
plot(uc$map, main = 'Unsupervised Classification 2020')

# 2. additional NDVI as input for classification
# stack data before running classification
stack_ndvi <- stack(sen2020, ndvi_2020)
set.seed(22)
# for multiple features with different scales - normalize data (substract mean & divide by standard deviation)
uc_ndvi <- unsuperClass(stack_ndvi, nClasses = 4, nStarts = 50, nSamples = 10000, norm = T)
plot(uc_ndvi$map)

# 3a.DEM
stack_dem <- stack(sen2020, srtm_10m)
set.seed(33)
uc_dem <- unsuperClass(stack_dem, nClasses = 4, nStarts = 50, nSamples = 10000, norm = T)
plot(uc_dem$map)

# 3b. additional DEM features as input for classification
stack_dem_feat <- stack(sen2020, srtm_slope, srtm_aspect, srtm_roughness)
set.seed(44)
uc_dem_feat <- unsuperClass(stack_dem_feat, nClasses = 4, nStarts = 50, nSamples = 10000, norm = T)
plot(uc_dem_feat$map)

# 4. textural metrics as input
stack_glcm_pca <- stack(sen2020, glcm_pca$glcm_variance)
set.seed(55)
uc_glcm_pca <- unsuperClass(glcm_pca$glcm_variance, nClasses = 4, nStarts = 50, nSamples = 10000, norm = T)
plot(uc_glcm_pca$map)

# 5. combination of all features
stack_dem_feat <- stack(sen2020, ndvi_2020, srtm_10m, glcm_pca$glcm_variance)
set.seed(66)
uc <- unsuperClass(sen2020, nClasses = 4, nStarts = 50, nSamples = 10000, norm = T)
plot(uc$map)


## Validation
# stack classifications & plot for comparison
class_stack <- stack(uc$map, uc_ndvi$map, uc_dem$map, uc_dem_feat$map, uc_glcm_pca$map, uc$map)
names(class_stack) <- c("only raster","raster + ndvi","raster + dem","raster + dem features", "raster + mean metric", "all features")
plot(class_stack)

# check if classification overlays with camp training data
# crop classification output to aoi
uc_aoi <- crop(uc$map, extent(osm_aoi_sf))
uc_ndvi_aoi <- crop(uc_ndvi$map, extent(osm_aoi_sf))
uc_dem_aoi <- crop(uc_dem$map, extent(osm_aoi_sf))
uc_dem_feat_aoi <- crop(uc_dem_feat$map, extent(osm_aoi_sf))
sc_aoi <- crop(sc_20$map, extent(osm_aoi_sf))

uc_aoi_stack <- stack(uc_aoi, uc_ndvi_aoi, uc_dem_aoi, uc_dem_feat_aoi, sc_aoi)
names(uc_aoi_stack) <- c("UC Sen2020","UC Sen2020 + NDVI", "UC Sen2020 + DEM", "UC Sen2020 + DEM features", "SC")
plot(uc_aoi_stack) # camp difficult to identify due to high spectral variability in scene



#### HIERARCHICAL CLASSIFICATION / MASKING ####
# based on threshold masking for multiple metrics
# search for areas with low ndvi, low height/slope, high change & high textural variation

## 1. VIIRS - identify built-up areas
plot(viirs_10m)
hist(viirs_10m)
viirs_mask <- viirs_10m

viirsq <- raster::quantile(viirs_10m, probs = 0.3)
viirs_mask[viirs_mask < viirsq] <- NA
plot(viirs_mask)

# check if AOI still in mask
ggR(viirs_mask) + geom_sf(data= osm_aoi_sf, aes(alpha = 0.2), fill = "red")

# filter raster data according to viirs mask
sen2020_viirs_mask <- mask(sen2020, viirs_mask)
plot(sen2020_viirs_mask)


## 2. DEM - height values
# create mask - filter out high height values via DEM
plot(srtm_10m)
hist(srtm_10m)
dem_mask <- srtm_10m

demq <- raster::quantile(srtm_10m, probs = 0.85)
dem_mask[dem_mask > demq] <- NA
plot(dem_mask)

# filter raster data according to DEM mask
sen2020_dem_mask <- mask(sen2020, dem_mask)
plot(sen2020_dem_mask)


## 3. MSAVI - better for soily areas in Xianjiang
msavi_mask <- msavi_2020
hist(msavi_mask)
plot(msavi_mask)

# mask according to 0.8 quantile
msaviq <- raster::quantile(msavi_2020, probs = 0.7)

msavi_mask[((msavi_mask > msaviq) )] <- NA # vegetated areas
plot(msavi_mask)

# aggregate by a factor of 3, with "modal" (to reduce speckle effect)
m <- raster::aggregate(msavi_mask, fact =3, fun = modal, na.rm = TRUE, expand = F)
plot(m)
# resample to get same extent as original raster
# bilinear performs better than nearest neighbor
me <- resample(m, sen2020, method = "bilinear")

# filter raster data according to NDVI mask
sen2020_msavi_mask <- mask(sen2020, me)
plot(sen2020_msavi_mask)


## 4. CVA - filter out areas with little change between 2017 and 2020
# analyze bands with highest difference
cva <- rasterCVA(sen2017[[c(2,11)]], sen2020[[c(2,11)]])
plot(cva)

hist(cva$angle)
hist(cva$magnitude)

cva_mask <- cva
# mask areas with high change angle & magnitude, filter out areas with low change
# mask according to 0.9 quantile
cvaq <- raster::quantile(cva$magnitude, probs = 0.9)

cva_mask[cva_mask$magnitude < cvaq] <- NA
plot(cva_mask)

# aggregate by a factor of 3, with "modal" (to reduce speckle effect)
c <- raster::aggregate(cva_mask, fact = 9, fun = modal, na.rm = TRUE, expand = F)
plot(c)
# resample to get same extent as original raster
# bilinear performs better than nearest neighbor
ce <- resample(c, sen2020, method = "bilinear")

sen2020_cva_mask <- mask(sen2020, ce$magnitude)
plot(sen2020_cva_mask)


## 5. texture
# mask areas with low variance (as camps have high variability due to roofing, multiple buildings of different types & sizes, fences etc.)
plot(glcm_pca) # glcm_pca variance differentiates camps & fields best
hist(glcm_pca$glcm_variance)
plot(glcm_pca$glcm_variance)

glcm_pca_mask <- glcm_pca$glcm_variance
# create mask - filter out high areas
glcm_pcaq <- raster::quantile(glcm_pca_mask, probs = 0.3)
glcm_pca_mask[glcm_pca_mask < glcm_pcaq] <- NA
plot(glcm_pca_mask)

# aggregate by a factor of 3, with "modal" (to reduce speckle effect)
g <- raster::aggregate(glcm_pca_mask, fact = 3, fun = modal, na.rm = TRUE, expand = F)
plot(g)
# resample to get same extent as original raster
# bilinear performs better than nearest neighbor
ge <- resample(g, sen2020, method = "bilinear")

# filter raster data according to NDVI mask
sen2020_glcm_mask <- mask(sen2020, ge)
plot(sen2020_glcm_mask)


## final classification output
# 1. VIIRS
sen2020_class <- sen2020_viirs_mask

# 2. DEM
sen2020_class_dem <- mask(sen2020_class, dem_mask)

# 3. MSAVI
sen2020_class_msavi <- mask(sen2020_class_dem, me)

# 4. CVA
sen2020_class_cva <- mask(sen2020_class_msavi, ce$magnitude)

# 5. texture - mean (as camps have high reflectance values e.g. due to metal roofing)
sen2020_class_tex <- mask(sen2020_class_cva, ge)

# plot classification output
plot(sen2020_class_tex)
plotRGB(sen2020_class_tex, 4, 3, 2, stretch = "lin")


# form patches of pixels with 8-neighbor rule
sen2020_class_patch <- clump(sen2020_class_tex$B1, directions=8)

# vectorize output raster
class_vector <- rasterToPolygons(sen2020_class_patch, dissolve = T)

# calculate area of polygons
class_vector$area <- area(class_vector)

# drop smallest vectors
max(class_vector$area)
min(class_vector$area)
hist(class_vector$area)

class_area <- class_vector
plot(class_vector)
areamin <- min(osm_camps$area) # 6985 m²
# # for analysis of smaller camps only filter out areas bigger than min. camp size
# class_area <- class_area[class_area$area > areamin,] 
#areaq <- raster::quantile(class_area$area, probs = 0.9)
class_area <- class_area[class_area$area >= max(class_vector$area),] 
plot(class_area)

class_area_sf <- st_as_sf(class_area)

# plot final masking output with AOI 
ggRGB(sen2020, r=4, g=3, b=2, stretch = "lin") +
  geom_sf(data = class_area_sf , aes(alpha = 0.2), fill = "red")
