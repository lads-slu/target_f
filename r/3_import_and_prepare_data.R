#list yield shape.files
ff<-list.files(shpdir)
sel1<-grepl(x=ff, pattern=".shp")
sel2<-grepl(x=ff, pattern=".shp.")
sel3<-grepl(x=ff, pattern="block")
ff<-ff[sel1&!sel2&!sel3]

#import blockdata
b <-vect(file.path(shpdir,blockname))

#create area of interest (aoi) polygon
aoi<-buffer(as.polygons((ext(b))),1000)

#buffer blockdata
bsmall<-buffer(x=b, width=-30)

#import s2 data
#zz <- list.files(path = s2dir, pattern = "*.zip", full.names = TRUE, recursive = TRUE) #list zip.files
#for(i in zz){unzip(zipfile=i, exdir = s2dir)}# unzip 
rr<-list.files(s2dir,pattern = ".jp2", recursive = TRUE) #select jp2-files
sel<-grepl(datetouse, rr); rr<-rr[sel] #select date
sel<-grepl('B07_20m', rr); fname<-file.path(s2dir, rr[sel]) #select b7-file
b7<-rast(fname) #read b7-file
sel<-grepl('B05_20m', rr); fname<-file.path(s2dir, rr[sel]) #select b5-file
b5<-rast(fname) #read b5-file

#compute index
r<-(b7-b5)/(b7+b5) 

#crop s2-data
r<-crop(r, aoi)

#create output directories, if they do not already exist
cleandir<-file.path(dirname(shpdir),'3_epsg3006_cleaned'); wdif(cleandir)
testdir<-file.path(dirname(shpdir),'4_epsg3006_test'); wdif(testdir)
