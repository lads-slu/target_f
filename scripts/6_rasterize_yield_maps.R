focalfilter<-1

#create pretty raster
bb<-buffer(b, 100)
e<-c(xmin(bb), xmax(bb), ymin(bb), ymax(bb))
e<-100*round(0.01*e)
r<-rast(xmin =e[1], xmax=e[2],	ymin=e[3], ymax=e[4], res=res, crs=prj)
r<-rasterize(x=s[s$o_any==0,], y=r, field="yield", fun=mean)

#run focal filter
if(focalfilter>0){
  for (i in 1:focalfilter){
    r<-disagg(r, fact=2, "bilinear")
    r<-aggregate(r, fact=2, "mean")
  }
}


#pad raster to fill polygon
r<-extend(r, 3)
f<-focal(r, w=c(3,3), fun="mean", na.rm=T, pad=T)  
f<-mask(f, r, inverse=T) 
r<-sum(f, r, na.rm=TRUE)

#crop and mask
r<-crop(r, b, mask=TRUE)

#rename
names(r)<-"yield"