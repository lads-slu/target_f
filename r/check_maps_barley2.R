#set working directory
if(!'rstudioapi' %in% installed.packages())install.packages('rstudioapi')
wd<-dirname(dirname(rstudioapi::getSourceEditorContext()$path)) #now it automatically finds the parent folder to folder where this script is saved
setwd(wd)

#load packages
require('terra')
require('sf')

#set graphical parameters
par(mfrow=c(4,3))

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
  s$id<-1:nrow(s)
  a<-as.data.frame(s)
  sel<-complete.cases(a); s<-s[sel,]

  #identify turn (variance in heading among nearest points along the track)
  a<-s$Heading
  p<-10
  id<-1+p:(nrow(s)-p) #all points except first p points and last p points
  for(j in -p:p){ifelse(j==-p, yes= aa<-a[id+j], no=aa<-cbind(aa,a[id+j]))}
  turn<-apply(aa, 1, var)
  pad<-rep(NA, p); turn<-c(pad, turn, pad)
  s$turn<-turn
  sel<-!is.na(s$turn); s<-s[sel,]
  
  #identify stop (variance in time among nearest points along the track)
  a<-lubridate::parse_date_time(s$Time, "%m/%d/%y %I:%M:%S %p")
  a<-a-min(a) #time since start
  p<-10
  id<-1+p:(nrow(s)-p) #all points except first p points and last p points
  for(j in -p:p){ifelse(j==-p, yes= aa<-a[id+j], no=aa<-cbind(aa,a[id+j]))}
  stop<-apply(aa, 1, var)
  pad<-rep(NA, p); stop<-c(pad, stop, pad)
  s$stop<-stop
  sel<-!is.na(s$stop); s<-s[sel,]
  
  #identify gap (variance in distance among nearest points along the track)
  a<-s$DISTANCE
  p<-10
  id<-1+p:(nrow(s)-p) #all points except first p points and last p points
  for(j in -p:p){ifelse(j==-p, yes= aa<-a[id+j], no=aa<-cbind(aa,a[id+j]))}
  gap<-apply(aa, 1, var)
  pad<-rep(NA, p); gap<-c(pad, gap, pad)
  s$gap<-gap
  sel<-!is.na(s$gap); s<-s[sel,]
  
  #compute point density (no of points within a p meter radius)
  p<-5
  a<-as.data.frame(crds(s))
  a<-FNN::knn.dist(data=a, k=100, algorithm='brute')
  a<-a<p
  s$density<-rowSums(a)
  
  #compute local yield heterogeneity (variance of p closest points)
  p<-30
  aa<-as.data.frame(crds(s))
  bb<-s$WetMass
  cc<-as.data.frame(FNN::knn.index(data=aa, k=30, algorithm='brute'))[1:nrow(aa),]
  fkn<-function(x){return(var(bb[x]))}
  dd<-apply(X=cc, MARGIN=1, FUN=fkn)
  s$yieldvar<-as.numeric(dd)
  
  #compute index for deviation rows
  xy<-as.data.frame(crds(s))
  id<-as.data.frame(s$id)
  aa<-as.data.frame(FNN::knn.index(data=xy, k=30, algorithm='brute'))[1:nrow(xy),] #indices for neareast points spatially
  bb<-as.data.frame(FNN::knn.index(data=id, k=10, algorithm='brute'))[1:nrow(id),] #indices for neareast points in row
  cc<-cbind(aa,bb)
  dd<-s$WetMass
  fkn<-function(x){
    sel<-as.numeric(x[31:40]) #points in row
    sel2<-as.numeric(x[1:30]) #points in area
    sel2<-sel2[!sel2%in%sel1] #points in area but not in row
    rowmean<-mean(dd[sel1], na.rm=T)
    areamean<-mean(dd[sel], na.rm=T)
    normdiff<-(rowmean-areamean)/(rowmean+areamean) #normalized difference between row and area.
    normdiff[is.infinite(normdiff)]<-max(normdiff)
    return(normdiff)
    }
  dd<-apply(X=cc, MARGIN=1, FUN=fkn)
  s$rowdiff<-as.numeric(dd)
  
  # 1 mark points to remove points based on geometry and harvesting performance
  n<-'turn'; a<-as.data.frame(s)[,n];qq<-quantile(a[], probs=c(0.05, 0.95), names=F, na.rm=T)
  sel1<-a<=5; plot(b[s,],main =i, xlab=n); plot(s[!sel1,],add=T);s[,paste0('o_',n)]<-!sel1 
  n<-'stop'; a<-as.data.frame(s)[,n]; qq<-quantile(a[], probs=c(0.05, 0.95), names=F, na.rm=T)
  sel2<-a<=40; plot(b[s,],main =i, xlab=n); plot(s[!sel2,],add=T);s[,paste0('o_',n)]<-!sel2 
  n<-'gap'; a<-as.data.frame(s)[,n];qq<-quantile(a[], probs=c(0.05, 0.95), names=F, na.rm=T)
  sel3<-a<=0.2; plot(b[s,],main =i, xlab=n); plot(s[!sel3,],add=T);s[,paste0('o_',n)]<-!sel3 
  n<-'density'; a<-as.data.frame(s)[,n]; qq<-quantile(a[], probs=c(0.025, 0.975), names=F, na.rm=T)
  sel4<-a<=10; plot(b[s,],main =i, xlab=n); if(sum(!sel4)>0) plot(s[!sel4,],add=T);s[,paste0('o_',n)]<-!sel4
  n<-'SWATHWIDTH'; a<-as.data.frame(s)[,n]; sel5<-a>=8.9; plot(b[s,],main =i, xlab=n); if(sum(!sel5)>0) plot(s[!sel5,],add=T); s[,paste0('o_',n)]<-!sel4 
  sel<-sel1&sel2&sel3&sel4&sel5
  s[,'omit']<-0; s[!sel,'omit']<-1
  
  # 2 mark points to remove based on values
  n<-'Moisture'; a<-as.data.frame(s)[,n];qq<-quantile(a[sel], probs=c(0.025, 0.975), names=F, na.rm=T)
  sel6<-a>=qq[1] &a<=qq[2]; plot(b[s,],main =i, xlab=n); plot(s[!sel6,],add=T); s[,paste0('o_',n)]<-!sel6 
  n<-'WetMass'; a<-as.data.frame(s)[,n];qq<-quantile(a[sel], probs=c(0.025, 0.975), names=F, na.rm=T)
  sel7<-a>=qq[1] &a<=qq[2]; plot(b[s,],main =i, xlab=n); plot(s[!sel7,],add=T); s[,paste0('o_',n)]<-!sel7 
  n<-'rowdiff';a<-as.data.frame(s)[,n];qq<-quantile(a[sel], probs=c(0.05, 0.95), names=F, na.rm=T)
  sel8<-a>=qq[1]; plot(b[s,],main =i, xlab=n); plot(s[!sel8,],add=T); s[,paste0('o_',n)]<-!sel8 
  n<-'yieldvar'; a<-as.data.frame(s)[,n];qq<-quantile(a[sel], probs=c(0.1, 0.95), names=F, na.rm=T)
  sel9<-a<=7000000; plot(b[s,],main =i, xlab=n); plot(s[!sel9,],add=T); s[,paste0('o_',n)]<-!sel9 
  
  sel<-sel&sel6&sel7&sel8&sel9
  s[!sel,'omit']<-1
  
  #3 mark points to remove within buffer
  s[,'buffer']<-0; sel<-!s$id%in%s[bsmall,]$id; s[sel,'buffer']<-1
  
  #plot
  plot(b[s,],main =i)
  plot(s[s$rowdiff<0.13,], "rowdiff", col=rainbow(25), cex=0.2 ,  add=T)
  plot(bsmall[s,], add=T)
  
  #plot
  plot(b[s,],main =i)
  plot(s[s$omit==0,], "WetMass", col=rainbow(25), cex=0.2 ,  add=T)
  plot(bsmall[s,], add=T)
  
  #plot
  plot(b[s,],main =i)
  plot(s[s$omit==0&s$buffer==0,], "WetMass", col=rainbow(25), cex=0.2 ,  add=T)
  plot(bsmall[s,], add=T)
  
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

