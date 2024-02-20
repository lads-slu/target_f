#create output directories, if they do not already exist
wdif(outdir)

#export point shapefile for all fields
fname<-file.path(outdir,'pts.shp')
writeVector(x=sall, filename=fname, overwrite=TRUE)

#export polygon shapefile for all fields
fname<-file.path(outdir,'poly.shp')
writeVector(x=b, filename=fname, overwrite=TRUE)

#export polygon shapefile for all fields
fname<-file.path(outdir,'poly_small.shp')
writeVector(x=bsmall, filename=fname, overwrite=TRUE)

#export polygon shapefile for aoi
fname<-file.path(outdir,'aoi.shp')
writeVector(x=aoi, filename=fname, overwrite=TRUE)

#export tiff of unfiltered values


#export filtered tiff


#export statistics
fname<-file.path(outdir,'stats.txt')
write.table(stats, fname, "\t", row.names = FALSE)