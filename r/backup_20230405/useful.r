#create directory if it does not alreadey exist
wdif<-function(x){
  if(!dir.exists(x))dir.create(x)
}

txt<-function(x, path, fname){
  a<-paste0(path,'\\' ,fname)
  write.table(x, file = a, sep = "\t", row.names = F)
}
shp<-function(x, path, fname){
  a<-paste0(path,'\\' ,fname)
  raster::shapefile(x, a, overwrite=T)
}

tif<-function(x, path, fname){
  a<-paste0(path,'\\' ,fname)
  raster::writeRaster(x, a, overwrite=T, format='GTiff')
}

##vegetation indices
msavi2<-function (nir, red) {
  a<-0.5*(2*nir+1-sqrt((2*nir+1)^2-8*(nir-red)))
  return(a)
}

ci<-function (nir, rededge) {
  a<-nir/rededge-1
  return(a)
}

ndvi<-function (nir, red) {
  a<-(nir-red)/(nir+red)
  return(a)
}


## to object if it exists, else create that object
concatif<-function(x, y){
  if(exists(x)) x<-c(x,y) else x<-y
  return(x)
}

rbindif<-function(x, y){
  if(exists(x)) x<-rbind(x,y) else x<-y
  return(x)
}

cbindif<-function(x, y){
  if(exists(x)) x<-cbind(x,y) else x<-y
  return(x)
}

