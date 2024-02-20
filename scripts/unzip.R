#set working directory
if(!'rstudioapi' %in% installed.packages())install.packages('rstudioapi')
wd<-dirname(dirname(rstudioapi::getSourceEditorContext()$path)) #now it automatically finds the parent folder to folder where this script is saved
setwd(wd)

#load packages
library(plyr)

#list zip.files
zz <- list.files(path = "data", pattern = "*.zip", full.names = TRUE, recursive = TRUE)

# unzip all your files
for(i in zz){unzip(zipfile=i, exdir = strsplit(i, '.zip')[[1]])}

#omit one folder
dd<-list.dirs(path = "data",  full.names = TRUE, recursive = TRUE)
dd<-dd[grep(pattern='doc', x=dd)] #keep only "doc" directories
for (i in dd){
  #file.copy(from = file.path(i, list.files(i)), to = file.path(dirname(i), list.files(i)))
  unlink(i)
  }

