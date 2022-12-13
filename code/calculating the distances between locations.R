
install.packages("geosphere")
library(geosphere)

#import latitudes and longitudes
#file is locations_no_temps.csv


loc<-read.csv(file.choose())


loc$Loc<-factor(loc$Loc)
str(loc)

#make longitude colunn 2, and latitude column 3
long<-loc$Longitude
lat<-loc$Latitude

loc<-data.frame(loc$Loc,long,lat)
str(loc)
names(loc)<-c("Loc","Longitude","Latitude")

names<-levels(loc$Loc)
length(names)
nrow(loc)
#need a matrix of 54x54 (54 locations)

M<-matrix(NA,nrow(loc),nrow(loc))
colnames(M)<-names
rownames(M)<-names

for(i in 1:nrow(loc)){
  for(j in 1:nrow(loc)){
    M[i,j]<-distm(loc[i,2:3],loc[j,2:3],fun=distHaversine)/1000
  }
}


#make matrix to store only the distances we care about
N<-matrix(NA,nrow(loc),nrow(loc))
colnames(N)<-names
rownames(N)<-names

#only have the distances less than 250, others are returned as 0
for(i in 1:nrow(loc)){
  for(j in 1:nrow(loc)){
    if(M[i,j]<=250){
      N[i,j]<-M[i,j]
    } else{
      N[i,j]<-0
    }
  }
}
#gives the number of locations that are within 250km of another location
sum(colSums(N)!=0)

#we want all the columns where the colSum is >0
Dist<-N[,colSums(N)>0]
Dist<-Dist[rowSums(Dist)>0,]
ncol(Dist) #matches the number of locations


