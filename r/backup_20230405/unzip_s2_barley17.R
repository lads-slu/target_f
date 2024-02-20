#set working directory
if(!'rstudioapi' %in% installed.packages())install.packages('rstudioapi')
wd<-dirname(dirname(rstudioapi::getSourceEditorContext()$path)) #now it automatically finds the parent folder to folder where this script is saved
setwd(wd)

#load packages
library(plyr)
library(terra)
library(sf)

#sett graphical parameters
par(mfrow=c(1,3))

#define objects
s2dir<-"s2\\scania17"
ydir<-"data\\ymaps17\\barley17\\3_epsg3006_cleaned"
ptsname<-file.path(ydir, 'sall_cleaned.shp')
blockname<-file.path(ydir, 'block.shp')
datetouse<-'20170619'

#read s2 data
zz <- list.files(path = s2dir, pattern = "*.zip", full.names = TRUE, recursive = TRUE) #list zip.files
for(i in zz){unzip(zipfile=i, exdir = s2dir)}# unzip 
ff<-list.files(pattern = ".jp2", recursive = TRUE) #select jp2-files
sel<-grepl(datetouse, ff); ff<-ff[sel] #select date
sel<-grepl('B07_20m', ff); fname<-ff[sel] #select b7-file
b7<-rast(fname) #read b7-file
sel<-grepl('B05_20m', ff); fname<-ff[sel] #select b5-file
b5<-rast(fname) #read b5-file
r<-(b7-b5)/(b7+b5) #compute index

#read poly data
pts<-vect(ptsname)
b<-vect(blockname)
aoi<-buffer(as.polygons((ext(b))),1000)
pts<-crop(pts, r)

#buffer polygons
bsmall<-buffer(x=b, width=-30)

#project and crop s2-data
r<-crop(r, aoi)
r<-mask(r, b)

#keep only cludfree, fields
pts<-mask(pts, b)

for (i in unique(pts$fname)){
  #subset points for field i 
  sel<-pts$fname==i; pi<-pts[sel,]
  #subset block for field i
  bi<-b[pi,]
  #subset raster for field i
  ri<-crop(r, bi); ri<-mask(ri, bi)
  
  #convert raster cells to polygons
  ripoly<-as.polygons(ri,dissolve=FALSE)
  
  #compute median for points for each raster cell (include only point to keep)
  sel<-pi$omit==0&pi$buffer==0; pi<-pi[sel,]
  pir<-rasterize(x= pi,  y=ri, field= 'yield', fun='mean')
  
  #create a mask to keep only homogeneous cells
  pir_sd<-rasterize(x= pi,  y=ri, field= 'yield', fun='sd')
  homomask<-pir_sd<=300
  homomask[!homomask]<-NA
  
  #stack rasters
  rr<-c(ri, pir)  
  rr<-focal(rr, 3, 'median', na.rm=T)
  names(rr)<-c('s2', 'combine')
  
  #mask rasters
  bismall<-buffer(x=bi, width=-30)
  bismallr<-rasterize(bismall, ri) #needed for the masking to b slim
  rrsmall<-mask(rr, bismallr)
  #rrsmall<-mask(rrsmall, homomask)
  
  #plot results
  plot(rrsmall$s2)
  plot(bismall, add=T)
  plot(rrsmall$combine)
  plot(bismall, add=T)
  df<-as.data.frame(rrsmall)
  df<-df[complete.cases(df),]
  names(df)<-c('s2', 'combine')
  r2<-round((cor(df$combine, df$s2))^2,2)
  tt<-paste0('r = ', r2)
  plot(df$combine, df$s2, main=tt)
}
