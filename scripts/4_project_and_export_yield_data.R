reproject and export yield data
if(!dir.exists(dname))dir.create(dname)
for(i in ff){
  s <- vect(file.path(shpdir,i))
  s<-project(s, "EPSG:3006")
  fname<-file.path(dname, i)
  writeVector(x=s, filename=fname, overwrite=TRUE)
  plot(b[s,],main =i)
  plot(s, "yield", col=rainbow(25), cex=0.2 ,  add=T)
 }