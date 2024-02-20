s <- vect(file.path(shpdir,i))
s$id<-1:nrow(s)
a<-as.data.frame(s)
#sel<-complete.cases(a); s<-s[sel,]

#rename columns
nold<-as.character(c(yieldname, moisturename, widthname))
nnew<-c('yield', 'moisture', 'width')
for (j in 1:length(nold)){
  noldj<-nold[j];nnewj<-nnew[j]
  names(s)[names(s)==nold[j]]<-nnew[j] #if column exists, change to new name
  if(!nnewj%in%names(s)){s[,nnewj]<-NA} #if column is missing, add it
}

#compute heading
xy0<-as.data.frame(crds(s)) #previous point 
xy1<-xy0[c(1, 1:(nrow(xy0)-1)),] #current point (add dummy for first instance)
xdiff<-xy0$x-xy1$x #adjacent leg of right triangle
ydiff<-xy0$y-xy1$y #adjacent leg of right triangle
s$heading<-(atan2(xdiff, ydiff)+pi)*180/pi 

#compute distance 
s$distance<-sqrt(xdiff^2+ydiff^2) 

#compute turn index (variance in heading among nearest points along the track)
a<-s$heading
p<-10
id<-1+p:(nrow(s)-p) #all points except first p points and last p points
for(j in -p:p){ifelse(j==-p, yes= aa<-a[id+j], no=aa<-cbind(aa,a[id+j]))}
turn<-apply(aa, 1, var)
pad<-rep(NA, p); turn<-c(pad, turn, pad)
s$turn<-turn
sel<-!is.na(s$turn); s<-s[sel,]

#identify gap (variance in distance among nearest points along the track)
a<-s$distance
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

# 1 mark points to remove points based on geometry and harvesting performance
n<-'turn'; a<-as.data.frame(s)[,n]
if(sum(is.na(a))<0.5*length(a)&var(a, na.rm=T)>0.005){
  qq<-quantile(a, probs=c(0.05, 0.95), names=F, na.rm=T)
  selk<-a<=qq[2];selk[is.na(selk)]<-FALSE
  plot(b[s,],main =i, xlab=n); plot(s[!selk,],add=T)
}else{
  selk<-rep(TRUE,length(a));selk[is.na(selk)]<-FALSE
  plot(x=1, y=1, main =paste0('no ', n, ' slot'))
}
s[,paste0('o_',n)]<-!selk; sel1<-selk

n<-'gap'; a<-as.data.frame(s)[,n]
if(sum(is.na(a))<0.5*length(a)&var(a, na.rm=T)>0.005){
  qq<-quantile(a, probs=c(0.05, 0.95), names=F, na.rm=T)
  selk<-a<=qq[2];selk[is.na(selk)]<-FALSE
  plot(b[s,],main =i, xlab=n); plot(s[!selk,],add=T)
}else{
  selk<-rep(TRUE,length(a));selk[is.na(selk)]<-FALSE
  plot(x=1, y=1, main =paste0('no ', n, ' slot'))
}
s[,paste0('o_',n)]<-!selk; sel2<-selk

n<-'density'; a<-as.data.frame(s)[,n]
if(sum(is.na(a))<0.5*length(a)&var(a, na.rm=T)>0.005){
  qq<-quantile(a, probs=c(0.05, 0.95), names=F, na.rm=T)
  selk<-a>=qq[1]&a<=qq[2];selk[is.na(selk)]<-FALSE
  plot(b[s,],main =i, xlab=n); plot(s[!selk,],add=T)
}else{
  selk<-rep(TRUE,length(a));selk[is.na(selk)]<-FALSE
  plot(x=1, y=1, main =paste0('no ', n, ' slot'))
}
s[,paste0('o_',n)]<-!selk; sel3<-selk

n<-'width'; a<-as.data.frame(s)[,n]
if(sum(is.na(a))<0.5*length(a)&var(a, na.rm=T)>0.005){
  high<-0.95*max(a, na.rm=T)
  selk<-a>=high;selk[is.na(selk)]<-FALSE
  plot(b[s,],main =i, xlab=n); plot(s[!selk,],add=T)
}else{
  selk<-rep(TRUE,length(a));selk[is.na(selk)]<-FALSE
  plot(x=1, y=1, main =paste0('no ', n, ' slot'))
}
s[,paste0('o_',n)]<-!selk; sel4<-selk

sel<-sel1&sel2&sel3&sel4
s[,'omit']<-0; s[!sel,'omit']<-1

# 2 mark points to remove based on values

n<-'moisture'; a<-as.data.frame(s)[,n]
if(sum(is.na(a))<0.5*length(a)&var(a, na.rm=T)>0.005){
  aa<-a[sel]; bb<-3*mad(aa, constant = 1, na.rm=T); cc<-median(a, na.rm=T)
  low<-cc-bb; high<-cc+bb #hampel filter 
  qq<-quantile(a, probs=c(0.025, 0.975), names=F, na.rm=T)
  selk<-a>=low&a<=high&a>=qq[1]&a<=qq[2];selk[is.na(selk)]<-FALSE
  plot(b[s,],main =i, xlab=n); plot(s[!selk,],add=T)
}else{
  selk<-rep(TRUE,length(a));selk[is.na(selk)]<-FALSE
  plot(x=1, y=1, main =paste0('no ', n, ' slot'))
}
s[,paste0('o_',n)]<-!selk; sel5<-selk

n<-'yield'; a<-as.data.frame(s)[,n]
if(sum(is.na(a))<0.5*length(a)&var(a, na.rm=T)>0.005){
  aa<-a[sel]; bb<-3*mad(aa, constant = 1, na.rm=T); cc<-median(a, na.rm=T)
  low<-cc-bb; high<-cc+bb #hampel filter 
  qq<-quantile(a, probs=c(0.025, 0.975), names=F, na.rm=T)
  selk<-a>=low&a<=high&a>=qq[1]&a<=qq[2];selk[is.na(selk)]<-FALSE
  plot(b[s,],main =i, xlab=n); plot(s[!selk,],add=T)
}else{
  selk<-rep(TRUE,length(a));selk[is.na(selk)]<-FALSE
  plot(x=1, y=1, main =paste0('no ', n, ' slot'))
}
s[,paste0('o_',n)]<-!selk; sel6<-selk

sel<-sel&sel5&sel6
s[!sel,'omit']<-1

#3 mark points to remove within buffer
s[,'buffer']<-0; sel<-!s$id%in%s[bsmall,]$id; s[sel,'buffer']<-1

#plot
plot(b[s,],main =i)
plot(s[s$omit==0,], "yield", col=rainbow(25), cex=0.2 ,  add=T)
plot(bsmall[s,], add=T)

#plot
plot(b[s,],main =i)
plot(s[s$omit==0&s$buffer==0,], "yield", col=rainbow(25), cex=0.2 ,  add=T)
plot(bsmall[s,], add=T)

hist(s$omit, main=paste0(round(100*mean(s$omit)), ' % omitted'))

#merge all point datasets to one
s$fname<-i
if(i == ff[1]) {sall<-s} else {sall<-rbind(sall, s)}
#plot(sall, "yield", col=rainbow(25), cex=0.2 , main =i)

