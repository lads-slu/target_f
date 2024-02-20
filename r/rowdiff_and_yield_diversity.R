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