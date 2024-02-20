#set graphical parameters
par(mfrow=c(3,3))

#yield data specifications
shpdir<-"data\\ymaps21\\oats21\\2_epsg3006" #folder with yield and block shapefiles(epsg 3006)
blockname<-"block_with_yield.shp"  #polygon shapefile (epsg 3006)
s2name<-'s2_210703' #column in block file indicating which polygons have OK s2 data (e.g. no clouds)
moisturename<-"MOISTURE" #column in point shapefile with moisture data
yieldname<-"YIELD" #column in point shapefile with yield data
widthname<-'WIDTH' #column in point shapefile with swathwidth data

#Sentinel-2 (s2) data specifications
s2dir<-"data\\s2\\vg21"
datetouse<-'20210703'

#other
testdir<-"data\\ymaps21\\oats21\\3_epsg3006_cleaned\\testdata" #folder to which data for target_Y testing will be exported
