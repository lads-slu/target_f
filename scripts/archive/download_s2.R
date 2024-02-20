#set working directory
if(!'rstudioapi' %in% installed.packages())install.packages('rstudioapi')
wd<-dirname(dirname(rstudioapi::getSourceEditorContext()$path)) #now it automatically finds the parent folder to folder where this script is saved
setwd(wd)

#load packages
library(terra)
library(sf)
library(sen2r)

#set graphical parameters
par(mfrow=c(1,3))

#define objects
blockname<-"data\\ymaps19\\oats19\\3_epsg3006_cleaned\\block_small.shp"
tt<-as.Date(c("2019-06-25", "2019-07-18"))
cc<-'epsg:32633' #epsg of blockfile (utm33n)
uu<-username<-'kristin.persson'
pp<-password<-'j.s2.2123'
  
#import aoi
b<-vect(blockname, crs=cc)
b<-project(b, 'epsg:4326')

#import and prepare aoi
bb<-crds(as.polygons(ext(b)))
for (i in 1:4){assign(paste0('pt', i), st_point(bb[i,]))}
aoi<-st_sfc(pt1, pt2, pt3, pt4, crs=4326)

#create permanent and temporary directories
if(!dir.exists('s2tmp')){dir.create('s2tmp')}
if(!dir.exists('s2')){dir.create('s2')}
if(!dir.exists('s2\\dwn')){dir.create('s2\\dwn')}

#create credentials
write_scihub_login(uu, pp) 

sl<-s2_list(
  spatial_extent = aoi,
  tile = NULL,
  orbit = NULL,
  time_interval = tt,
  time_period = "full",
  level = "L2A",
  server = "scihub",
  apihub = NA,
  service = "apihub",
  max_cloud = 100,
  availability="check",
  tmpdir = 's2temp',
  output_type = "deprecated"
)

save(list=ls(), file='s2\\s2.RData')

s2_download(
  s2_prodlist = sl[1],
  downloader = "builtin",
  service = "apihub",
  outdir = file.path(getwd(),'s2\\dwn'),
  order_lta = TRUE,
  abort = TRUE,
  overwrite = FALSE
)

safe_is_online("C:/Users/piikki/AppData/Local/Temp/Rtmpuuwvz6/lta_orders/lta_20230329_110927.json")

unlink('s2tmp')

