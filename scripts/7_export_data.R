#create output directories, if they do not already exist
wdif(outdir)
shpdir<-file.path(outdir, "shp"); wdif(shpdir)
txtdir<-file.path(outdir, "txt"); wdif(txtdir)
tifdir<-file.path(outdir, "tif"); wdif(tifdir)

#export point shapefiles
fname<-file.path(shpdir,'pts_original.shp')
writeVector(x=s, filename=fname, overwrite=TRUE)

fname<-file.path(shpdir,'pts_filtered.shp')
writeVector(x=s[!s$o_any,], filename=fname, overwrite=TRUE)

#export point textfiles
wdif("txt")
a<-data.frame(crds(s), values(s))
fname<-file.path(txtdir,'pts_original.txt')
write.table(x=a, file=fname, sep="\t", row.names = FALSE)

fname<-file.path(txtdir,'pts_filtered.txt')
write.table(x=a[!a$o_any,], file=fname, sep="\t", row.names = FALSE)

#export polygon shapefiles
fname<-file.path(shpdir,'poly_original.shp')
writeVector(x=b, filename=fname, overwrite=TRUE)

fname<-file.path(shpdir,'poly_small.shp')
writeVector(x=bsmall, filename=fname, overwrite=TRUE)

#export tiff of unfiltered values
fname<-file.path(tifdir,'yield_filtered.tif')
writeRaster(x=r, fname, filetype='GTiff', overwrite=TRUE)

#export statistics
fname<-file.path( txtdir,'stats.txt')
write.table(stats, fname, sep="\t", row.names = FALSE)
