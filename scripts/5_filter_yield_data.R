# 1 mark points to remove points based on geometry and harvesting performance
n<-'turn'; a<-as.data.frame(s)[,n]
if(sum(is.na(a))<0.5*length(a)&var(a, na.rm=T)>0.005){
  qq<-quantile(a, probs=turnfilter, names=F, na.rm=T)
  selk<-a<=qq[2];selk[is.na(selk)]<-FALSE
  plot(b[s,], xlab=n); plot(s[!selk,],add=T)
}else{
  selk<-rep(TRUE,length(a));selk[is.na(selk)]<-FALSE
  plot(x=1, y=1, main =paste0('no ', n, ' slot'))
}
s[,paste0('o_',n)]<-!selk; sel1<-selk

n<-'gap'; a<-as.data.frame(s)[,n]
if(sum(is.na(a))<0.5*length(a)&var(a, na.rm=T)>0.005){
  qq<-quantile(a, probs=gapfilter, names=F, na.rm=T)
  selk<-a<=qq[2];selk[is.na(selk)]<-FALSE
  plot(b[s,], xlab=n); plot(s[!selk,],add=T)
}else{
  selk<-rep(TRUE,length(a));selk[is.na(selk)]<-FALSE
  plot(x=1, y=1, main =paste0('no ', n, ' slot'))
}
s[,paste0('o_',n)]<-!selk; sel2<-selk

n<-'density'; a<-as.data.frame(s)[,n]
if(sum(is.na(a))<0.5*length(a)&var(a, na.rm=T)>0.005){
  qq<-quantile(a, probs=densityfilter, names=F, na.rm=T)
  selk<-a>=qq[1]&a<=qq[2];selk[is.na(selk)]<-FALSE
  plot(b[s,], xlab=n); plot(s[!selk,],add=T)
}else{
  selk<-rep(TRUE,length(a));selk[is.na(selk)]<-FALSE
  plot(x=1, y=1, main =paste0('no ', n, ' slot'))
}
s[,paste0('o_',n)]<-!selk; sel3<-selk

n<-'width'; a<-as.data.frame(s)[,n]
if(sum(is.na(a))<0.5*length(a)&var(a, na.rm=T)>0.005){
  high<-widthfilter*max(a, na.rm=T)
  selk<-a>=high;selk[is.na(selk)]<-FALSE
  plot(b[s,], xlab=n); plot(s[!selk,],add=T)
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
  qq<-quantile(a, probs=moisturefilter, names=F, na.rm=T)
  selk<-a>=low&a<=high&a>=qq[1]&a<=qq[2];selk[is.na(selk)]<-FALSE
  plot(b[s,], xlab=n); plot(s[!selk,],add=T)
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
  plot(b[s,], xlab=n); plot(s[!selk,],add=T)
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
plot(b[s,])
plot(s[s$omit==0,], "yield", col=rainbow(25), cex=0.2 ,  add=T)
plot(bsmall[s,], add=T)

#plot
plot(b[s,])
plot(s[s$omit==0&s$buffer==0,], "yield", col=rainbow(25), cex=0.2 ,  add=T)
plot(bsmall[s,], add=T)
hist(s$omit, main=paste0(round(100*mean(s$omit)), ' % omitted'))
