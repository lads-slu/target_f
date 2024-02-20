#import polygon data
b <-vect(file.path(shpdir,blockname))

#create area of interest (aoi) polygon
aoi<-buffer(as.polygons((ext(b))),1000)

#import combine data
s <- vect(file.path(shpdir,i))
s$id<-1:nrow(s)
a<-as.data.frame(s)
#sel<-complete.cases(a); s<-s[sel,]



#buffer polygon
bsmall<-buffer(x=b, width=-30)

#create output directories, if they do not already exist
wdif(outdir)