#subset points for field i 
sel<-sall$fname==i; pts<-sall[sel,]

#subset block for field i
bi<-b[pts,]

#do only proceed if block is cloud free
test<-mean(unlist(values(bi[,s2name]), use.names=FALSE))<1
if(test) next

#subset raster for field i
ri<-crop(r, bi); ri<-mask(ri, bi)

#convert raster cells to polygons
ripoly<-as.polygons(ri,dissolve=FALSE)

#compute median for points for each raster cell (include only point to keep)
sel<-pts$omit==0&pts$buffer==0; pts<-pts[sel,]
ptsr<-rasterize(x= pts,  y=ri, field= 'yield', fun='mean')

#create a mask to keep only homogeneous cells
ptsr_sd<-rasterize(x= pts,  y=ri, field= 'yield', fun='sd')
homomask<-ptsr_sd<=300
homomask[!homomask]<-NA

#stack rasters
rr<-c(ri, ptsr)  
rr<-focal(rr, 3, 'median', na.rm=T)
names(rr)<-c('s2', 'combine')

#mask rasters
bismall<-buffer(x=bi, width=-30)
bismallr<-rasterize(bismall, ri) #needed for the masking to b slim
rrsmall<-mask(rr, bismallr)
#rrsmall<-mask(rrsmall, homomask)

#plot results
tt<-paste0('a')
plot(bi)
plot(rrsmall$s2, add=T)
plot(bismall, add=T)
plot(bi)
plot(rrsmall$combine, add=T)
plot(bismall, add=T)
df<-as.data.frame(rrsmall)
df<-df[complete.cases(df),]
names(df)<-c('s2', 'combine')
r2<-round((cor(df$combine, df$s2))^2,2)
tt<-paste0('r = ', r2)
plot(df$combine, df$s2, main=tt)

#extract raster data for points
sall$ndre75<-rr$s2[sall,]

#export data for testing
field<-strsplit(i, '.shp')
wdif(file.path(testdir,field))
fname<-file.path(testdir, field, "ndre75.tif")
writeRaster(x=rr$s2, filename=fname, datatype="FLT4S", names=names(rr), overwrite=TRUE)
fname<-file.path(testdir, field, "combine.tif")
writeRaster(x=rr$combine, filename=fname, datatype="FLT4S", names=names(rr), overwrite=TRUE)
fname<-file.path(testdir, field, "mean.txt")
mn<-mean(values(rr$combine), na.rm=T)
write.table(x=mn, file = fname, sep = "\t", row.names = F)
fname<-file.path(testdir, field, "b.shp")
writeVector(x=bi, filename=fname,overwrite=TRUE)
fname<-file.path(testdir, field, "pts.shp")
writeVector(x=pts, filename=fname,overwrite=TRUE)

