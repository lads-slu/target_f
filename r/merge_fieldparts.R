#temp
sel<-sall$fname=='wheat_2017_heddinge_10a.shp'|sall$fname=='wheat_2017_heddinge_10c.shp'
t<-sall[sel,]
fname<-file.path(shpdir, 'wheat_2017_heddinge_10ac.shp')
writeVector(x=t, filename=fname, overwrite=TRUE)

sel<-sall$fname=="wheat_2017_klagerup_8f.shp"|sall$fname=="wheat_2017_klagerup_8c.shp"
t<-sall[sel,]
fname<-file.path(shpdir, "wheat_2017_klagerup_8cf.shp")
writeVector(x=t, filename=fname, overwrite=TRUE)

sel<-sall$fname=="wheat_2017_klagerup_2e.shp"|sall$fname=="wheat_2017_klagerup_2d.shp"
t<-sall[sel,]
fname<-file.path(shpdir, "wheat_2017_klagerup_2de.shp")
writeVector(x=t, filename=fname, overwrite=TRUE)

sel<-sall$fname=="wheat_2017_klagerup_15a.shp"|sall$fname=="wheat_2017_klagerup_15b.shp"
t<-sall[sel,]
fname<-file.path(shpdir, "wheat_2017_klagerup_15ab.shp")
writeVector(x=t, filename=fname, overwrite=TRUE)