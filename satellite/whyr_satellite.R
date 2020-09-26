#Satellite imagery analysis in R @ Why R? 2020

#install.packages("devtools")
#library(devtools)
#devtools::install_github("16EAGLE/getSpatialData")
#install.packages("sen2r")
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
query$tile_id # 1 tile = 100x100km2

#you can also specify level of processing - level 1C is before correction, 2A after correction
query10 = query[query$cloudcov < 10 & query$tile_id == "T34UCA" & query$level == "Level-1C",] 
query10$record_id


#set archive, view previewand download data
set_archive("C:/04_R")
plot_records(query10)
records = get_previews(query10)
view_previews(records[2,])
getSentinel_data(records)

#similarly you can search and download Landsat imagery (then you need to sign in at https://earthexplorer.usgs.gov/)
#getLandsat_records()
#getLandsat_data(time_range, platform)
#login_USGS(username = "egrabska")

#some interesting tools from sen2r package - very good package for processing Sentinel-2 images; also tools for downloading data
?s2_translate #enables to create one stack (one, multi-band, geotiff image)
?s2_calcindices #many many indices 
?s2_mask #masking clouds


#PART2: READING AND VISUALIZATION USING RASTER PACKAGE ------------------------------------------------------------------------

#Sentinel-2 acquires data in 13 spectral bands, however, we will not use all of them, 
#as not all of them are designed for analysis of land areas. There are 10 bands for land applications - 
#3 visible bands, 3 red-edge bands (located at the edge between red light and infrared) two near-infrared (NIR), 
#and we also have two short-wave infrared bands (SWIR).

setwd("C:/04_R/preliminary_analysis")
list.files()

#I already prepared one .tif file which is a multi-band image, now you can read bands seperately or in one stack

r1 = raster("C:/04_R/preliminary_analysis/20190825_crop.tif") #only single band will be read (the first one)
band4 = raster("20190825_crop.tif", band = 4) #here you can specify which band do you want to read
s1 = stack("20190825_crop.tif") #this is an image composed of 6 bands in order: visible blue, visible green, visible red, NIR, SWIR1, SWIR2

#print information about rasters
r1
band4
s1

names(s1) = c("blue", "green", "red", "nir", "swir1", "swir2") #change band names in a raster stack 

plot(r1) 
plot(s1) #all bands plotted seperately 

#extracting one element (band) from the raster stack 
s1[[1]]
s1$blue
blue_band = s1[[1]]

plot(s1[[1]]) #and plot single band from stack

#compositions - plot bands in color compostion 
plotRGB(s1, stretch = "lin") #default one (1,2,3)
plotRGB(s1, r = 3, g = 2, b = 1, stretch = "lin")


#compare different compositions
par(mfrow = c(1,4)) #plot 4 compositions at once
plotRGB(s1, r = 3, g = 2, b = 1, stretch = "lin") #true-color compositions
plotRGB(s1, r = 4, g = 3, b = 2, stretch = "lin") #false color compositions
plotRGB(s1, r = 5, g = 4, b = 3, stretch = "lin") #SWIR false-color
plotRGB(s1, r = 6, g = 5, b = 4, stretch = "lin") #two SWIR false-color

dev.off() #remove all plots

#another option to plot images
image(s1)


#PART3: PROCESSING DATA -----------------------------------------------------------

#cropping image to smaller extent
extent(s1)
e = extent(360000, 380000, 7800000, 7810000)
s1_crop = crop(s1, e)

plot(s1_crop)
plotRGB(s1_crop, r=6, g=4, b =2, stretch = "lin")

#writing data
writeRaster(s1_crop, "20190805_crop2.tif")
writeFormats()

#checking values distribution - histograms, scatterplots and correlations
dev.off()
hist(s1_crop)
pairs(s1_crop, maxpixels = 5000) #remember to use maxpixels values because if you take all of the pixel values it will take a lot of time to produce
pairs(s1_crop[[c(4,6)]], maxpixels = 10000) #note how high values the burning areas have in SWIR



#mathematical operations 
s2 = stack("20190805_crop.tif")
e = extent(360000, 380000, 7800000, 7810000)
s2_crop = crop(s2, e)
plotRGB(s2_crop, r = 4, g= 3, b = 2, stretch = "lin")


ndvi = (s2_crop[[4]] - s2_crop[[3]])/(s2_crop[[4]] + s2_crop[[3]]) #normalized difference vegetation index - it uses NIR and visible red bands
plot(ndvi)
plot(ndvi, col=brewer.pal(n = 6, name = "PiYG"))

mndwi = (s2_crop[[2]] - s2_crop[[5]])/(s2_crop[[2]] + s2_crop[[5]]) #modified normalized difference water index - uses visible green and swir
plot(mndwi, col=brewer.pal(n = 10, name = "RdBu"))

ind_stack = stack(mndwi, ndvi) #you can create a stack of indices, you can also create a stack of bands and indices
plot(ind_stack)
pairs(ind_stack, maxpixels = 1000)


#exercise - pre-fire and post-fire NBR (Normalized Burn Ratio)

s3 = stack("20190830_crop.tif") #image from after fire 
s3_crop = crop(s3, e)
plotRGB(s3_crop, r = 3, g = 2, b = 1, stretch = "lin")

#Typically, Tto estimate the severity of burnt areas, delta NBR is calculated – the difference between pre-fire and post-fire NBR. 
nbr_pre = (s2_crop[[4]] - s2_crop[[6]])/(s2_crop[[4]] + s2_crop[[6]])
nbr_post = (s3_crop[[4]] - s3_crop[[6]])/(s3_crop[[4]] + s3_crop[[6]])


dev.off()
delta_nbr = nbr_pre - nbr_post
hist(delta_nbr, col = "red")
plot(delta_nbr, col=brewer.pal(n = 6, name = "YlOrRd"))

#determine areas with high severity burn - e.g. areas with values of delta NBR > 0.66 are high severity burnt areas
burnt = reclassify(delta_nbr, c(-1, 0.1, 0, 0.1, 0.27, 1, 0.27, 0.44, 2, 0.44, 0.66, 3, 0.66, 1, 4))
plot(burnt, col=brewer.pal(n = 5, name = "YlOrRd"))


#PART 4 - classification --------------------------------------------------------------------------------
warsaw = stack("C:/04_R/classification/warsaw.tif")
plotRGB(warsaw, r =3, g = 2, b=1, stretch = "lin")
warsaw #note that this image is composed of all 10 bands (not only 6 as in Amazon case)


#we will take the "full" spectrum of Sentinel-2 to get more information for automatic classification
names(warsaw) = c("blue", "green", "red", "re1", "re2", "re3", "nir1", "nir2", "swir1", "swir2") #change the names again
pairs(warsaw, maxpixels = 1000)

setwd("C:/04_R/classification")
ref = shapefile("reference_utm.shp") #reading file with reference (training) samples 

unique(ref$class) #how many land cover classes it represents
plotRGB(warsaw, r =3, g = 2, b=1, stretch = "lin") #visualize rgb composition again and...
plot(ref, add =TRUE, col = "red") #the location of the reference samples

#before classification, in order to analyze spectral properties of land cover classes, 
#we firstly extract values from the image to sample polygons (mean values for each polygon)

#you can use extract function from raster package but it's extremely slow 
ref_values = raster::extract(warsaw, ref, fun = "mean") %>% as.data.frame()
ref_values$class = ref$class #add class attribute to a dataframe

#better choice for larger datasets is: 
#library(exactextractr)
#and the function called exact_extract :)
#the thing here is that exact_extract needs a sf object as an input so you have to read shapefile with st_read() function from sf package instead of shapefile() function

#some visualization with ggplot2 package - scatterplots:
ggplot(ref_values, aes(green, re2, color = class))+
  geom_point(size = 2)+
  stat_ellipse()


#we need to prepare the data to create spectral curves - there are many tools in r which can be used, 
#e.g. melt and dcast functions from reshape2, functions from dplyr, 
#tidyr; aggregate function etc. we can try this:

mean_spectra = group_by(ref_values, class) %>% #we group ref_values by class
  summarise_all(mean) %>% #calculate mean value for each class
  gather(key, value, -class) #transform the df to "long" format 

#we also need to specify order of bands: (if not they will be plotted in alphabetical order)
mean_spectra$key = factor(mean_spectra$key, levels=c("blue", "green", "red", "re1", "re2", "re3", "nir1", "nir2", "swir1", "swir2"))

#and plot sepctral curves:
ggplot(mean_spectra, aes(key, value, color = class, group = class))+
  geom_point()+
  geom_line(size = 1.8, alpha = 0.6)

#Image classification - there are 2 types of classification – unsupervised and supervised. 
#in unsupervised classification we don’t use reference data (training data), all pixels are grouped into clusters using for example k-means algorithm. 
#in supervised classification we use reference, training samples. For these training samples the land cover class and exact location is known. 
#we will use classification tools form RStoolbox package

#unsupervised classification
class1 = unsuperClass(warsaw, nSamples = 100, nclasses = 5)
class1$map
plot(class1$map, col = rainbow(5))

#supervised classification
?superClass

#additional calculation of two indices:
warsaw_mndwi = (warsaw[[2]] - warsaw[[9]])/(warsaw[[2]] + warsaw[[9]])
warsaw_ndvi = (warsaw[[7]] - warsaw[[3]])/(warsaw[[7]] + warsaw[[3]])
warsaw_all = stack(warsaw, warsaw_ndvi, warsaw_mndwi) #you can classify one stack with 10 bands and 2 indices and then check the variable importance!



#The function called superClass train the model and then validate it (we have to provide both training and validation datasets). 
#We will use the reference polygons and split them into train and validation samples with proportion of 70% for training, 30% for validation. 
#We can split the samples inside the superclass function. we can put set.seed() function inside to always get the same random partition. 
#remeber that two perform reliable classification you have to follow some rules regarding obtaining training and validation data (e.g. they should not be close to each other in order to avoid spatial autocorrelation)


classification_rf = superClass(warsaw_all, ref, set.seed(5), trainPartition = 0.7, responseCol = "class", #random forest classification
                                   model = "rf", mode = "classification", tuneLength = 5, kfold = 10)

classification_svm = superClass(warsaw_all, ref, set.seed(5), trainPartition = 0.7, responseCol = "class", #support vector machines classification
                               model = "svmLinear", mode = "classification", tuneLength = 5, kfold = 10)

classification_svm #the result is a list and it includes classification_svm$map that you can plot and save on your disc using writeRaster()
#the accuracy assessment is also available - if you print the classification result object, the first element is validation, 
#two most important measures of the classification are – confusion matrix bewteen reference and prediction and overall accuracy. 

#check the importance of particular bands
varImp_rf = varImp(classification_rf$model)
varImp_svm = varImp(classification_svm$model)
 
#and print/plot the results of VI 
plot(varImp_rf)
varImp_rf
varImp_svm

#use only the important bands as input - for example:
classification_rf = superClass(warsaw_all[[c(9,10,11, 7)]], ref, set.seed(5), trainPartition = 0.7, responseCol = "class",
                               model = "rf", mode = "classification", tuneLength = 5, kfold = 10)

classification_rf

#VI using RFE (Recursive Feature Elimination) - with ref_values again (of course to do that in "proper" way you would need another dataset)
#RFE is a simple backwards selection, searching for the optimal subset of variables by performing optimization algorithms

ref_values$class = as.factor(ref_values$class) #we need class variable as a factor
control = rfeControl(functions=rfFuncs, method="cv", number=10) #create control object 
results = rfe(ref_values[,1:10], ref_values[,11], sizes=c(1:10), rfeControl=control) #run RFE algorithm

#print/plot results 
results
plot(results, type = "l") #line plot. as you can see, we not necessary need all of the bands to achieve high accuracy; as seen in scatterplots, 
#the correlation between some of the bands is very high and therefore they are redundant 
predictors(results) #the most important predictors


#Another way of avoiding redudnancy is to reduce space - for example using very popular PCA (Principal Component Analysis)

#there is a tool rasterPCA in RStoolbox package:
?rasterPCA
warsaw_pca = rasterPCA(warsaw, nComp = 3) #usually the first 2-3 components have the most infromation  

#look at the results (we have a list again)
warsaw_pca$map
plot(warsaw_pca$map)
plotRGB(warsaw_pca$map, r=3, g=2, b = 1, stretch = "lin") #we can plot is as a color composite as well

#and perform classification on reduced space: 
class_PCA = superClass(warsaw_pca$map, ref, set.seed(5), trainPartition = 0.7, responseCol = "class",
                          model = "rf", mode = "classification", tuneLength = 5, kfold = 10)

class_PCA

# visualisation of classified map
plot(classification_rf$map, col=c("darkgreen", "brown3","chartreuse4", "chartreuse", "yellow", "cadetblue3"))

#PART 5: MULTI-TEMPORAL ANALYSIS----------------------------------------------------------------------------
#In the last part we will analyze multi-temporal imagery – i.e. dense time series of images from the same year. 
#Dense time series are used particularly in vegetation monitoring, for example in mapping small forest disturbances, or in crop monitoring. 
#In these part we will also analyze the vegetation - how the different species/types of vegetation reflectance changes during the growing season. 
#Again, there are some already prepared reference data and 17 cropped images from Senitnel-2. 


setwd("C:/04_R/multi_temporal")
stacklist = lapply(list.files(pattern = "*.tif$"), stack) #use lapply() function to read all of the images at once - i.e. all of the images with given pattern (.tif format)
#the result is a list of 17 stacks 

ref = shapefile("ref_crops.shp")

#extract data again; use lapply
ref_values = lapply(stacklist, raster::extract, ref, fun = "mean") %>% as.data.frame() #it takes some time... 
ref_values$class = ref$class

colnames(ref_values) = sub("X", "", colnames(ref_values)) #removing unnecessary strings 
band = select(ref_values, ends_with(".7")) #select the band (e.g. 7 - NIR1)
band$class = ref$class #and add the class column again

means = band %>% #similarly as in previous part, we will calculate mean values for each class
  gather(key, value, -class) %>%
  as.data.frame

means$key = as.Date(means$key, format = "%Y%m%d") #change the key, i.e. a variable with date to date format

#and plot it:
ggplot(means, aes(key, value, color = class, group = class))+
  geom_point()+
  geom_line(size = 2, alpha = 0.6)

#Some simple conlcusion form the NIR time series analysis are:
#In NIR region, healthy vegetation has a very high values (it is sensitive to scattering surfaces, such as leaves – Leaf Area Index). 
#Crops typically have the highest NIR values
#Conifers have lower values than broad-leaved forests, and they are relatively stable, as most of the conifers are evergreen species, 
  #but there are also some seasonal variations 
#RE1 region – lower values = more chlorophyll
#Rapeseed is a specific crop as it blooms intensively,here we can see that in April it starts to growth, while at the beginning of May the intensive  bloom starts, 
  #there is a peak in RE1 on May 

#Similarly, you can analyze other bands or calculate indices and analyze their trajectories during the growing season.

#THANK YOU!!! :)

