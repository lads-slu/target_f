b<-sqrt(a^2+(pts$y1-pts$y0)^2) #hypotenuse of right triangle
h<-2*acos(a/b)
s$heading<-360*acos(a/b)/(2*pi)
s$heading<-2*acos(a/b)

x0<-rep(0,11)
y0<-rep(0,11)
x1<-cos(pi*(0:10/5))
y1<-sin(pi*(-5:5/5))
pts<-data.frame(x0, x1, y0, y1)
plot(x1, y1)