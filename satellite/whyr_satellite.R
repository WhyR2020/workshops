#Satellite imagery analysis in R @ Why R? 2020

install.packages("devtools")
library(devtools)
devtools::install_github("16EAGLE/getSpatialData")
install.packages("sen2r")
#two more packages to install (with our ML methods for classification): randomForest and kernlab

library(getSpatialData)
library(sen2r)
library(raster)
library(RStoolbox)
library(tidyr)
library(dplyr)
library(rlang)
library(ggplot2)
library(RColorBrewer)
library(viridis)
library(caret)

#PART1 - DOWNLOADING & PRE-PROCESSING DATA ------------------------------------------------------------------------------------

#getSpatialData package

get_products() #list of available products

set_aoi() #set and view the area of interest
view_aoi()
get_aoi()

time_range =  c("2020-08-30", "2020-09-30") #set time range
platform = "Sentinel-2" #choose platform
login_CopHub(username = "ewagrabska") #login to Esa SCI-HUB


#get a Sentinel-2 query with specified parameters 
?getSentinel_records
query = getSentinel_records(time_range, "Sentinel-2")

str(query)

#examine query dataframe
query$cloudcov
query$tile_id

query10 = query[query$cloudcov < 10 & query$tile_id == "T34UCA" & query$level == "Level-1C",]
query10$record_id

#view query
plot_records(query10)
records = get_previews(query10) #does not working?
view_previews(records[2,])

#set archive and download data
set_archive("C:/04_R")
getSentinel_data(records)

#similarly you can search and download Landsat imagery (then you need to sign in at https://earthexplorer.usgs.gov/)
#getLandsat_records()
#getLandsat_data(time_range, platform)
#login_USGS(username = "egrabska")

#some interesting tools from sen2r package
?s2_translate
?s2_calcindices
?s2_mask

compareRasters()

#PART2: READING AND VISUALIZATION USING RASTER PACKAGE ------------------------------------------------------------------------


setwd("C:/04_R/preliminary_analysis")
list.files()

r1 = raster("C:/04_R/preliminary_analysis/20190825_crop.tif")
r1
plot(r1)
band4 = raster("20190825_crop.tif", band = 4)

s1 = stack("20190825_crop.tif") #this is an image composed of 6 bands in order: visible blue, visible green, visible red, NIR, SWIR1, SWIR2


#print information about rasters
r1
band4
s1

names(s1) = c("blue", "green", "red", "nir", "swir1", "swir2") #change band names in a raster stack 

plot(r1)
plot(s1)

#extract one element (band) from the raster stack 
s1[[1]]
s1$blue
band1 = s1[[1]]

plot(s1[[1]]) #and plot single band from stack

#compositions
plotRGB(s1, stretch = "lin")

plotRGB(s1, r = 3, g = 2, b = 1, stretch = "lin")


#compare different compositions
?par
par(mfrow = c(1,3))
plotRGB(s1, r = 3, g = 2, b = 1, stretch = "lin") #true-color compositions

plotRGB(s1, r = 4, g = 3, b = 2, stretch = "lin") #false color compositions

plotRGB(s1, r = 5, g = 4, b = 3, stretch = "lin") #SWIR false-color

plotRGB(s1, r = 6, g = 5, b = 4, stretch = "lin") #two SWIR false-color

dev.off()

#another option to plot images
image(s1)
?image

#PART3: PROCESSING DATA -----------------------------------------------------------

names(s1)
names(s1) = c("blue", "green", "red", "nir", "swir1", "swir2")

extent(s1)
e = extent(360000, 380000, 7800000, 7810000)
s1_crop = crop(s1, e)

plot(s1_crop)
plotRGB(s1_crop, r=6, g=4, b =2, stretch = "lin")

#writing data
writeRaster(s1_crop, "20190805_crop2.acii")
writeFormats()

#checking values distribution 
dev.off()
hist(s1_crop)
pairs(s1_crop, maxpixels = 5000)
pairs(s1_crop[[c(4,6)]], maxpixels = 10000)



#mathematical operations 
s2 = stack("20190805_crop.tif")
e = extent(360000, 380000, 7800000, 7810000)
s2_crop = crop(s2, e)
plotRGB(s2_crop, r = 4, g= 3, b = 2, stretch = "lin")


ndvi = (s2_crop[[4]] - s2_crop[[3]])/(s2_crop[[4]] + s2_crop[[3]])
plot(ndvi)
plot(ndvi, col=brewer.pal(n = 6, name = "PiYG"))

mndwi = (s2_crop[[2]] - s2_crop[[5]])/(s2_crop[[2]] + s2_crop[[5]])
plot(mndwi, col=brewer.pal(n = 10, name = "RdBu"))

ind_stack = stack(mndwi, ndvi)
plot(ind_stack)
pairs(ind_stack, maxpixels = 1000)


#exercise - pre-fire and post-fire NBR

s3 = stack("20190830_crop.tif") #image from after fire 
s3_crop = crop(s3, e)
plotRGB(s3_crop, r = 3, g = 2, b = 1, stretch = "lin")

nbr_pre = (s2_crop[[4]] - s2_crop[[6]])/(s2_crop[[4]] + s2_crop[[6]])
nbr_post = (s3_crop[[4]] - s3_crop[[6]])/(s3_crop[[4]] + s3_crop[[6]])

plot(nbr_pre, col=brewer.pal(n = 10, name = "YlOrRd"))

dev.off()
delta_nbr = nbr_pre - nbr_post
hist(delta_nbr, col = "red")
plot(delta_nbr, col=brewer.pal(n = 6, name = "YlOrRd"))

#determine areas with high severity burn 
burnt = reclassify(delta_nbr, c(-1, 0.1, 0, 0.1, 0.27, 1, 0.27, 0.44, 2, 0.44, 0.66, 3, 0.66, 1, 4))
plot(burnt, col=brewer.pal(n = 5, name = "YlOrRd"))


#PART 4 - classification --------------------------------------------------------------------------------
warsaw = stack("C:/04_R/classification/warsaw.tif")
plotRGB(warsaw, r =3, g = 2, b=1, stretch = "lin")
warsaw

names(warsaw) = c("blue", "green", "red", "re1", "re2", "re3", "nir1", "nir2", "swir1", "swir2")
pairs(warsaw, maxpixels = 1000)

setwd("C:/04_R/classification")
ref = shapefile("reference_utm.shp")
ref

unique(ref$class)
plot(ref, add =TRUE, col = "red")

names(warsaw)


ref_values = raster::extract(warsaw, ref, fun = "mean") %>% as.data.frame()
ref_values$class = ref$class 


ggplot(ref_values, aes(green, re2, color = class))+
  geom_point(size = 2)+
  stat_ellipse()


#prepare the data to create spectral curves - there are many tools in r which can be used, melt and dcast functions from reshape2, functions from dplyr, 
#tidyr; aggregate function etc.

mean_spectra = group_by(ref_values, class) %>%
  summarise_all(mean) %>%
  gather(key, value, -class)


mean_spectra$key = factor(mean_spectra$key, levels=c("blue", "green", "red", "re1", "re2", "re3", "nir1", "nir2", "swir1", "swir2"))

ggplot(mean_spectra, aes(key, value, color = class, group = class))+
  geom_point()+
  geom_line(size = 1.8, alpha = 0.6)


#unsupervised classification

class1 = unsuperClass(warsaw, nSamples = 100, nclasses = 5)
class1$map
plot(class1$map, col = rainbow(5))

#supervised classification
?superClass

warsaw_mndwi = (warsaw[[2]] - warsaw[[9]])/(warsaw[[2]] + warsaw[[9]])
warsaw_ndvi = (warsaw[[7]] - warsaw[[3]])/(warsaw[[7]] + warsaw[[3]])
warsaw_all = stack(warsaw, warsaw_ndvi, warsaw_mndwi)

plot(warsaw_ndvi)
classification_rf = superClass(warsaw_all, ref, set.seed(5), trainPartition = 0.7, responseCol = "class",
                                   model = "rf", mode = "classification", tuneLength = 5, kfold = 10)

classification_svm = superClass(warsaw_all, ref, set.seed(5), trainPartition = 0.7, responseCol = "class",
                               model = "svmLinear", mode = "classification", tuneLength = 5, kfold = 10)

classification_svm

#check the importance of particular bands
varImp_rf = varImp(classification_rf$model)
varImp_svm = varImp(classification_svm$model)

plot(varImp_rf)
varImp_rf
varImp_svm

#use only the important bands as input

classification_rf = superClass(warsaw_all[[c(9,10,11, 7)]], ref, set.seed(5), trainPartition = 0.7, responseCol = "class",
                               model = "rf", mode = "classification", tuneLength = 5, kfold = 10)

classification_rf

#VI using RFE - with ref_values again


ref_values$class = as.factor(ref_values$class)

control = rfeControl(functions=rfFuncs, method="cv", number=10)
results = rfe(ref_values[,1:10], ref_values[,11], sizes=c(1:10), rfeControl=control)

results
plot(results, type = "l")
predictors(results)



#or reduce space
?rasterPCA

warsaw_pca = rasterPCA(warsaw, nComp = 3) #usually the first 2-3 components have the most infromation; tak some time  

warsaw_pca$map
plot(warsaw_pca$map)
plotRGB(warsaw_pca$map, r=3, g=2, b = 1, stretch = "lin")


class_PCA = superClass(warsaw_pca$map, ref, set.seed(5), trainPartition = 0.7, responseCol = "class",
                          model = "rf", mode = "classification", tuneLength = 5, kfold = 10)

class_PCA

# visualisation of classified map
plot(classification_rf$map, col=c("darkgreen", "brown3","chartreuse4", "chartreuse", "yellow", "cadetblue3"))

#MULTI-TEMPORAL ANALYSIS

setwd("C:/04_R/multi_temporal")
stacklist = lapply(list.files(pattern = "*.tif$"), stack)

ref = shapefile("ref_crops.shp")

start_time = Sys.time()
ref_values = lapply(stacklist, raster::extract, ref, fun = "mean") %>% as.data.frame() #6 minutes
end_time = Sys.time()
end_time - start_time

ref_values$class = ref$class

colnames(ref_values) = sub("X", "", colnames(ref_values))

band = select(ref_values, ends_with(".8"))
band$class = band$class

means = blue %>% 
  gather(key, value, -class) %>%
  as.data.frame

means$key = as.Date(means$key, format = "%Y%m%d")


ggplot(means, aes(key, value, color = class, group = class))+
  geom_point()+
  geom_line(size = 2, alpha = 0.6)



