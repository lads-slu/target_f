
#export point shapefile for all fields
fname<-file.path(cleandir,'sall_cleaned.shp')
writeVector(x=sall, filename=fname, overwrite=TRUE)

#export polygon shapefile for all fields
fname<-file.path(cleandir,'block.shp')
writeVector(x=b, filename=fname, overwrite=TRUE)

#export polygon shapefile for all fields (buffered=shrunk)
fname<-file.path(cleandir,'block_small.shp')
writeVector(x=bsmall, filename=fname, overwrite=TRUE)

#export polygon shapefile for aoi (buffered=shrunk)
fname<-file.path(cleandir,'aoi.shp')
writeVector(x=aoi, filename=fname, overwrite=TRUE)

