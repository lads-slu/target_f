# 1 mark points to remove points based on geometry and harvesting performance
n<-'turn'; a<-as.data.frame(s)[,n]
if(sum(is.na(a))<0.5*length(a)&var(a, na.rm=T)>0.005){
  qq<-quantile(a, probs=turnfilter, names=F, na.rm=T)
  selk<-a<=qq[2];selk[is.na(selk)]<-FALSE
  plot(b, xlab=n); plot(s[!selk,],add=T)
}else{
  selk<-rep(TRUE,length(a));selk[is.na(selk)]<-FALSE
  plot(x=1, y=1, main =paste0('no ', n, ' slot'))
}
s[,paste0('o_',n)]<-!selk; sel1<-selk

n<-'gap'; a<-as.data.frame(s)[,n]
if(sum(is.na(a))<0.5*length(a)&var(a, na.rm=T)>0.005){
  qq<-quantile(a, probs=gapfilter, names=F, na.rm=T)
  selk<-a<=qq[2];selk[is.na(selk)]<-FALSE
  plot(b, xlab=n); plot(s[!selk,],add=T)
}else{
  selk<-rep(TRUE,length(a));selk[is.na(selk)]<-FALSE
  plot(x=1, y=1, main =paste0('no ', n, ' slot'))
}
s[,paste0('o_',n)]<-!selk; sel2<-selk

n<-'density'; a<-as.data.frame(s)[,n]
if(sum(is.na(a))<0.5*length(a)&var(a, na.rm=T)>0.005){
  qq<-quantile(a, probs=densityfilter, names=F, na.rm=T)
  selk<-a>=qq[1]&a<=qq[2];selk[is.na(selk)]<-FALSE
  plot(b, xlab=n); plot(s[!selk,],add=T)
}else{
  selk<-rep(TRUE,length(a));selk[is.na(selk)]<-FALSE
  plot(x=1, y=1, main =paste0('no ', n, ' slot'))
}
s[,paste0('o_',n)]<-!selk; sel3<-selk

n<-'width'; a<-as.data.frame(s)[,n]
if(sum(is.na(a))<0.5*length(a)&var(a, na.rm=T)>0.005){
  high<-widthfilter*max(a, na.rm=T)
  selk<-a>=high;selk[is.na(selk)]<-FALSE
  plot(b, xlab=n); plot(s[!selk,],add=T)
}else{
  selk<-rep(TRUE,length(a));selk[is.na(selk)]<-FALSE
  plot(x=1, y=1, main =paste0('no ', n, ' slot'))
}
s[,paste0('o_',n)]<-!selk; sel4<-selk

# 2 mark points to remove based on values

n<-'moisture'; a<-as.data.frame(s)[,n]
if(sum(is.na(a))<0.5*length(a)&var(a, na.rm=T)>0.005){
  aa<-a[sel]; bb<-3*mad(aa, constant = 1, na.rm=T); cc<-median(a, na.rm=T)
  low<-cc-bb; high<-cc+bb #hampel filter 
  qq<-quantile(a, probs=moisturefilter, names=F, na.rm=T)
  selk<-a>=low&a<=high&a>=qq[1]&a<=qq[2];selk[is.na(selk)]<-FALSE
  plot(b, xlab=n); plot(s[!selk,],add=T)
}else{
  selk<-rep(TRUE,length(a));selk[is.na(selk)]<-FALSE
  plot(x=1, y=1, main =paste0('no ', n, ' slot'))
}
s[,paste0('o_',n)]<-!selk; sel5<-selk

n<-'yield'; a<-as.data.frame(s)[,n]
if(sum(is.na(a))<0.5*length(a)&var(a, na.rm=T)>0.005){
  aa<-a[sel]; bb<-3*mad(aa, constant = 1, na.rm=T); cc<-median(a, na.rm=T)
  low<-cc-bb; high<-cc+bb #hampel filter 
  qq<-quantile(a, probs=yieldfilter, names=F, na.rm=T)
  selk<-a>=low&a<=high&a>=qq[1]&a<=qq[2];selk[is.na(selk)]<-FALSE
  plot(b, xlab=n); plot(s[!selk,],add=T)
}else{
  selk<-rep(TRUE,length(a));selk[is.na(selk)]<-FALSE
  plot(x=1, y=1, main =paste0('no ', n, ' slot'))
}
s[,paste0('o_',n)]<-!selk; sel6<-selk

#3 mark points to remove within buffer (a bit cumbersome but just to b analogous to above)
n<-'buffer'
selk<-s$buffer
plot(b, xlab=n); plot(s[!selk,],add=T)
s[,paste0('o_',n)]<-!selk

#4_compile all points to omit
omitcols<-c("o_turn", "o_gap", "o_density", "o_width", "o_moisture", "o_yield", "o_buffer")
s$o_any<-apply(X=values(s)[,omitcols], MARGIN=1, FUN=any)

#plot
plot(b)
plot(s[s$o_any==0,], "yield", col=rainbow(25), cex=0.2 ,  add=T)
plot(bsmall[s,], add=T)


#compute summary statistics
omitcols2<-c(omitcols, "o_any")
stats<-round(colMeans(values(s)[,omitcols2]),2)
names(stats)<-gsub(names(stats), pattern="o_", replacement="")
stats<-data.frame(index=names(stats), omitted=stats)
row.names(stats)<-NULL


plot(x=as.factor(stats$index), y=stats$omitted)
