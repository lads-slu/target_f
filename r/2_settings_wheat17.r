#set graphical parameters
par(mfrow=c(3,3))

#yield data specifications
shpdir<-"data\\ymaps17\\wheat17\\2_epsg3006" #folder with yield and block shapefiles(epsg 3006)
blockname<-"block_with_yield.shp" #polygon shapefile (epsg 3006)
s2name<-'s2_170619' #column in block file indicating which polygons have OK s2 data (e.g. no clouds)
yieldname<-"WetMass" #column in point shapefile with yield data
moisturename<-"Moisture" #column in point shapefile with moisture data
widthname<-'SWATHWIDTH' #column in point shapefile with swathwidth data

#Sentinel-2 (s2) data specifications
s2dir<-"data\\s2\\scania17"
datetouse<-'20170619'