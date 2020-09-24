# Satellite imagery analysis in R

Authors: [Ewa Grabska](https://www.researchgate.net/profile/Ewa_Grabska2)

### Description

Satellite imagery, such as freely available data from Sentinel-2 mission, enable us to monitor the Earth's surface frequently (every 5 days), and with a high spatial resolution (10-20 meters). Furthermore, Sentinel-2 sensors, including 13 spectral bands in the visible and infrared wavelengths, provide very valuable information which can be used to automatically perform tasks such as classify crop types, assess forest changes, or monitor build-up area development. This is particularly important now, in the era of rapid changes in the environment related to climate change. In R, there are plenty of tools and packages which can be used for satellite images such as pre-processing, analyzing, and visualizing data in a simple and efficient way. Also, the variety of methods, such as machine learning algorithms, are available in R and can be applied in the analysis of satellite imagery. I would like to show the framework for acquiring, pre-processing and preliminary analysis of the Sentinel-2 time series in R. It includes the spectral indices calculation, the use of the machine learning algorithms in the classification of land cover, and, the analysis of time series of imagery, i.e. determining the changes in environment based on the spectral trajectories of pixels.

### Before the workshop

* #### Please create an account on the ESA sci-hub website: https://scihub.copernicus.eu/dhus/#/home

* #### Recquired packages: 

    * `sen2r`

    * `getSpatialData` (hosted on github so you also need `devtools` package, after that use: `devtools::install_github("16EAGLE/getSpatialData"`)

    * `raster`

    * `RStoolbox` 

    * `dplyr`

    * `tidyr`

    * `rlang`

    * `caret`

    * `ggplot2`

    * `RColorBrewer`

    * `viridis`


* #### Download materials:

    * https://www.dropbox.com/s/pss5sto3wb3z4ny/whyr_satellite.zip?dl=0


