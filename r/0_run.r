#set working directory
if(!'rstudioapi' %in% installed.packages())install.packages('rstudioapi')
wd<-dirname(dirname(rstudioapi::getSourceEditorContext()$path)) #now it automatically finds the parent folder to folder where this script is saved
setwd(wd)

#load packages
require('terra')
require('sf')

#clean environment
rm(list=ls())

#set case
case<-'oats21'
#case<-'oats19'
#case<-'wheat17'
#case<-'barley17'
filter=T

#define useful functions
source("r\\1_useful.r")

#read settings
source(paste0("r\\2_settings_", case, ".r"))

#import and prepare data (and save it in a temporary folder)
source("r\\3_import_and_prepare_data.r")

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

