#import polygon data
b <-vect(fieldfile)
b<-project(b, prj)

#create area of interest (aoi) polygon
aoi<-buffer(as.polygons((ext(b))),1000)
#buffer polygon
bsmall<-buffer(x=b, width=-edgefilter)

#import combine data
s <- vect(combinefile)
s<-project(s, prj)

#add id column
s$id<-1:nrow(s)

#convert to data.farme
a<-as.data.frame(s)

#rename columns
nold<-as.character(c(yieldname, moisturename, widthname))
nnew<-c('yield', 'moisture', 'width')
for (j in 1:length(nold)){
  noldj<-nold[j];nnewj<-nnew[j]
  names(s)[names(s)==nold[j]]<-nnew[j] #if column exists, change to new name
  if(!nnewj%in%names(s)){s[,nnewj]<-NA} #if column is missing, add it
}