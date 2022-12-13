library(sp)
library(raster)

#import data "location_no_temps.csv"
location<-read.csv(file.choose())
str(location)

#download climate data
climdata<-getData("worldclim", var="bio",res=5)

#we want to use annual temperature, average temp in the warmest and coldest quarter
#this is 1, 10 and 11
climdata<-climdata[[c(1,10,11)]]
names(climdata)<-c("AnnualTemp","WarmTemp","ColdTemp")

#convert our coordinates to spatial points
lat<-location$Latitude
lon<-location$Longitude
coords<-data.frame("long"=lon,"lat"=lat)
#convert to spatial points
points <- SpatialPoints(coords, proj4string = climdata@crs)
#extract temperatures
values<-extract(climdata,points)

location<-cbind(location, values/10)






