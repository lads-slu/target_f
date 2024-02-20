#clean environment
rm(list=ls())

#set working directory
if(!'rstudioapi' %in% installed.packages())install.packages('rstudioapi')
wd<-dirname(dirname(rstudioapi::getSourceEditorContext()$path)) #now it automatically finds the parent folder to folder where this script is saved
setwd(wd)

#install required packages that are not yet installed
pkgs <- c("terra", "sf")
sel <- !pkgs %in% rownames(installed.packages())
if(any(sel)){install.packages(pkgs[sel])}
invisible(lapply(X=pkgs, FUN=require, character.only = TRUE))

#define useful functions
source("scripts\\2_define_functions.r")

#set graphical parameters
par(mfrow=c(3,3))

#define object
##paths
combinefile<-"in\\ymaps21\\oats21\\2_epsg3006\\oats_2021_test_1.shp" #path, point shape file (combine registrations)
fieldfile<-"in\\ymaps21\\oats21\\2_epsg3006\\block_test_1.shp" #path, polygon shapefile (field boundary)
outdir<-"out"
##column names
yieldname<-"YIELD" #name of yield column
moisturename<-"MOISTURE" #name of moisture column (character value or NA)
timename1<-NA #one to avoid confusion with a function (character value or NA), currently not used.
distancename<-'DISTANCE'  #name of distance column (character value or NA)
headingname<-NA  #name of heading column (character value or NA)
widthname<-'WIDTH'  #name of swathwidth column (character value or NA)
##projections
prj<-"epsg: 3006" # coordinate system onto which data shall be projected (if it not already is)
res<- 20 #resolution of created yield raster

##filter settings
turnfilter<-c(0.05, 0.95) #lower and upper quantiles of turn index to keep (the extremes will be removed)
gapfilter<-c(0.05, 0.95) #lower and upper quantiles of gap index to keep (the extremes will be removed)
densityfilter<-c(0.05, 0.95) #lower and upper quantiles of density index to keep (the extremes will be removed)
moisturefilter<-c(0.025, 0.975) #quantiles of moisture hampel filter(outliers will be removed)
yieldfilter<-c(0.025, 0.975) #quantiles of yield hampel filter (outliers will be removed)
widthfilter<-0.95 ##lower quantile of swath width to keep (lower values will be removed)
edgefilter<-30 #width of buffer zone inside field boundary edge in which registrations will be removed
focalfilter<-1 #degree of smothing (positive integer). 0= no smoothing, 3=strong smoothing

#import and prepare data
source("scripts\\3_import_data.r")

#compute the geometric indices that will be used for filtering
source("scripts\\4_compute_indices.r")

#filter yield data
source("scripts\\5_filter_yield_data.r")

#rasterize_yield maps
source("scripts\\6_rasterize_yield_maps.r")

#export data
source("scripts\\7_export_data.r")

