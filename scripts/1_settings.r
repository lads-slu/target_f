#clean environment
rm(list=ls())

#set working directory
if(!'rstudioapi' %in% installed.packages())install.packages('rstudioapi')
wd<-dirname(dirname(rstudioapi::getSourceEditorContext()$path)) #now it automatically finds the parent folder to folder where this script is saved
setwd(wd)

#install required packages that are not yet installed
pkgs <- c("terra")
sel <- !pkgs %in% rownames(installed.packages())
if(any(sel)){install.packages(pkgs[sel])}
invisible(lapply(X=pkgs, FUN=require, character.only = TRUE))

#define useful functions
source("r\\2_define_functions.r")

#set graphical parameters
par(mfrow=c(3,3))

#define object
filter=T
combinefile<-"in\\ymaps21\\oats21\\2_epsg3006\\oats_2021_test_1.shp" #path, point shape file (combine registrations)
blockfile<-"in\\ymaps21\\oats21\\2_epsg3006\\block_oats21_v3.shp" #path, polygon shapefile (field boundary)
yieldname<-"YIELD" #name of yield column
moisturename<-"MOISTURE" #name of moisture column (character value or NA)
timename1<-NA #one to avoid confusion with a function (character value or NA)
distancename<-'DISTANCE'  #name of distance column (character value or NA)
headingname<-NA  #name of heading column (character value or NA)
widthname<-'WIDTH'  #name of width column (character value or NA)
prj<-"epsg: 3006" #projection of inpt data

#import and prepare data (and save it in a temporary folder)
source("r\\3_import_data.r")

#project and export yield data
#source("r\\4_project_and_export_yield_data") #run only when yield data is not in epsg: 3006.

#filter yield data (and save it in a temporary folder)
fname<-file.path(cleandir,'sall_cleaned.shp')
if(filter){
  for(i in ff){source("r\\5_filter_yield_data.r")}
  writeVector(x=sall, filename=fname, overwrite=TRUE)
} else{
  sall<-vect(fname)
}

#rasterize_yield maps
for(i in ff){source("r\\6_rasterize_yield_maps.r")}

#export data
source("r\\7_export_data.r")

