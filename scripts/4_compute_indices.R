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

#is point within buffer along field edge
s$buffer<-s$id%in%s[bsmall,]$id