#set working directory
if(!'rstudioapi' %in% installed.packages())install.packages('rstudioapi')
wd<-dirname(dirname(rstudioapi::getSourceEditorContext()$path)) #now it automatically finds the parent folder to folder where this script is saved
setwd(wd)

#load packages
require('terra')
require('sf')

#set graphical parameters
par(mfrow=c(2,3))

#define object
shpdir<-"data\\ymaps17\\barley17\\2_epsg3006"

#list yield shape.files
ff<-list.files(shpdir)
sel1<-grepl(x=ff, pattern=".shp")
sel2<-grepl(x=ff, pattern=".shp.")
sel3<-grepl(x=ff, pattern="block")
blockname<-"block_sub4.shp" 
ff<-ff[sel1&!sel2&!sel3]

#import blockdata
b <-vect(file.path(shpdir,blockname))
summary(b)
#plot(b)

#buffer blockdata
bsmall<-buffer(x=b, width=-30)
#plot(bsmall, add=T)
#(sum(expanse(b))-sum(expanse(bsmall)))/sum(expanse(b))

#create output directory if it does not exists
dname<-file.path(shpdir,'2_epsg3006')

#reproject and export yield data
# if(!dir.exists(dname))dir.create(dname)
# for(i in ff){
#   s <- vect(file.path(shpdir,i))
#   s<-project(s, "EPSG:3006")
#   fname<-file.path(dname, i)
#   writeVector(x=s, filename=fname, overwrite=TRUE)
#   plot(b[s,],main =i)
#   plot(s, "WetMass", col=rainbow(25), cex=0.2 ,  add=T)
# }

#filter yield data
for(i in ff){
  s <- vect(file.path(shpdir,i))
  a<-as.data.frame(s)
  sel<-complete.cases(a); s<-s[sel,]

  #identify turn (variance in heading among nearest points along the track)
  a<-s$Heading
  p<-10
  id<-(p+1):(nrow(s)-p)
  for(j in -p:p){ifelse(j==-p, yes= aa<-a[id+j], no=aa<-cbind(aa,a[id+j]))}
  turn<-apply(aa, 1, var)
  pad<-rep(NA, p); turn<-c(pad, turn, pad)
  s$turn<-turn
  sel<-!is.na(s$turn); s<-s[sel,]
  
  #identify stop (variance in time among nearest points along the track)
  a<-lubridate::parse_date_time(s$Time[1], "%m/%d/%y %I:%M:%S %p")
  p<-10
  id<-(p+1):(nrow(s)-p)
  for(j in -p:p){ifelse(j==-p, yes= aa<-a[id+j], no=aa<-cbind(aa,a[id+j]))}
  stop<-apply(aa, 1, var)
  pad<-rep(NA, p); stop<-c(pad, stop, pad)
  s$stop<-stop
  sel<-!is.na(s$stop); s<-s[sel,]
  
  #compute no of points within a p meter radius
  a<-as.data.frame(crds(s))
  p<-5
  a<-FNN::knn.dist(data=a, k=100, algorithm='brute')
  a<-a<p
  s$density<-rowSums(a)
  
  #maxdist of last p last points along the track (to remove points after a stop)
  a<-s$DISTANCE
  p<-10
  id<-(p+1):(nrow(s)-p)
  for(j in -p:0){ifelse(j==-p, yes= aa<-a[id+j], no=aa<-cbind(aa,a[id+j]))}
  maxdist<-apply(aa, 1, max)
  pad<-rep(NA, p); maxdist<-c(pad, maxdist)
  s$maxdist<-maxdist
  sel<-!is.na(s$maxdist); s<-s[sel,]
  
  # 1 mark points to remove points based on geometry and harvesting performance
  n<-'turn'; a<-as.data.frame(s)[,n]; sel1<-a<=5; plot(b[s,],main =i, xlab=n); plot(s[!sel1,],add=T);s[,paste0('o_',n)]<-!sel1 
  n<-'maxdist'; a<-as.data.frame(s)[,n]; sel2<-a<=2; plot(b[s,],main =i, xlab=n); plot(s[!sel2,],add=T);s[,paste0('o_',n)]<-!sel2 
  n<-'density'; a<-as.data.frame(s)[,n]; sel3<-a<=10; plot(b[s,],main =i, xlab=n); if(sum(!sel3)>0) plot(s[!sel3,],add=T);s[,paste0('o_',n)]<-!sel3 
  n<-'SWATHWIDTH'; a<-as.data.frame(s)[,n]; sel4<-a>=8.9; #plot(b[s,],main =i, xlab=n); if(sum(!sel4)>0) plot(s[!sel4,],add=T)
  s[,paste0('o_',n)]<-!sel4 
  sel<-sel1&sel2&sel3&sel4
  s[,'omit']<-0; s[!sel,'omit']<-1
  s$omit_turn<-!sel1
  
  # 2 mark points to remove based on values
  n<-'Moisture'; a<-as.data.frame(s)[,n];qq<-quantile(a[sel], probs=c(0.025, 0.975), names=F)
  sel5<-a>=qq[1]&a<=qq[2]; plot(b[s,],main =i, xlab=n); plot(s[!sel5,],add=T); s[,paste0('o_',n)]<-!sel5 
  n<-'WetMass'; a<-as.data.frame(s)[,n];qq<-quantile(a[sel], probs=c(0.025, 0.975), names=F)
  sel6<-a>=qq[1]&a<=qq[2]; plot(b[s,],main =i, xlab=n); plot(s[!sel6,],add=T); s[,paste0('o_',n)]<-!sel6 
  sel<-sel&sel5&sel6
  s[!sel,'omit']<-1
  
  #3 mark points to remove within buffer
  s$id<-1:nrow(s)
  s[,'buffer']<-0; sel<-!s$id%in%s[bsmall,]$id; s[sel,'buffer']<-1
  
  #plot
  #plot(b[s,],main =i)
  #plot(s[!is.na(s$WetMass),], "WetMass", col=rainbow(25), cex=0.2 ,  add=T)
  #plot(bsmall[s,], add=T)
  
  #plot
  plot(b[s,],main =i)
  plot(s[s$omit==0,], "WetMass", col=rainbow(25), cex=0.2 ,  add=T)
  plot(bsmall[s,], add=T)
  
  #plot
  #plot(b[s,],main =i)
  #plot(s[s$omit==0&s$buffer==0,], "WetMass", col=rainbow(25), cex=0.2 ,  add=T)
  #plot(bsmall[s,], add=T)
  
  #merge all point datasets to one
  s$fname<-i
  if(i == ff[1]) {sall<-s} else {sall<-rbind(sall, s)}
  #plot(sall, " WetMass", col=rainbow(25), cex=0.2 , main =i)
}

#export data
dname<-file.path(dirname(shpdir),'3_epsg3006_cleaned')
if(!dir.exists(dname))dir.create(dname)
fname<-file.path(dname,'sall_cleaned.shp')
writeVector(x=sall, filename=fname, overwrite=TRUE)
fname<-file.path(dname,'block.shp')
writeVector(x=b, filename=fname, overwrite=TRUE)
fname<-file.path(dname,'block_small.shp')
writeVector(x=bsmall, filename=fname, overwrite=TRUE)

