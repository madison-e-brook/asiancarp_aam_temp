library(raster)

#set working directory where CPC data was stored


#download one file to get the spatial points
tmin.1979 <- brick("tmin.1979.nc", varname = "tmin")
tmin.1979<-rotate(tmin.1979)
plot(tmin.1979$X1979.01.01)

#download the file with our lat/longs
loc<-read.csv(file.choose())
str(loc)

lat<-loc$Latitude
lon<-loc$Longitude
#turn them to spatial points
coords<-data.frame(longitude=lon, latitude=lat)
locations<-SpatialPoints(coords, proj4string = tmin.1979@crs)


#we are interested in 41 years (1979-2019)
degree_day<-matrix(NA,length(loc$Loc),42)
degree_day[,1]<-loc$Loc

years<-seq(1979,2019,1)

#Extracting points for degree days base 0####
for(i in 1:41){
  #download tmax
  tmax.name<-paste0("tmax.",years[i],".nc")
  tmax<- brick(tmax.name, varname = "tmax")
  tmax<-rotate(tmax)
  #extract tmax
  max<-extract(tmax,locations,layer=1)
  #download tmin
  tmin.name<-paste0("tmin.",years[i],".nc")
  tmin<- brick(tmin.name, varname = "tmin")
  tmin<-rotate(tmin)
  #extract tmin
  min<-extract(tmin,locations,layer=1)
  
  #calculate the degree days
  degree<-(max+min)/2
  degree[degree<0]<-0
  calculated.degrees<-rowSums(degree)
  degree_day[,i+1]<-calculated.degrees
  removeTmpFiles(h=0.001)
}


degree_day<-as.data.frame(degree_day)
str(degree_day)
#convert to numeric
for(i in 1:41){
  degree_day[,i+1]<-as.numeric(degree_day[,i+1])
}

annual.average.dd<-rowMeans(degree_day[,2:42],na.rm = TRUE)
annual.average.dd.sd<-apply(degree_day[,2:42],1,sd,na.rm=T)


loc$AnnualDD<-annual.average.dd
#loc$AnnualDD.sd<-annual.average.dd.sd
str(loc)

#dd5#####

degree_day.5<-matrix(NA,length(loc$Loc),42)
degree_day.5[,1]<-loc$Loc

for(i in 1:41){
  #download tmax
  tmax.name<-paste0("tmax.",years[i],".nc")
  tmax<- brick(tmax.name, varname = "tmax")
  tmax<-rotate(tmax)
  #extract tmax
  max<-extract(tmax,locations,layer=1)
  #download tmin
  tmin.name<-paste0("tmin.",years[i],".nc")
  tmin<- brick(tmin.name, varname = "tmin")
  tmin<-rotate(tmin)
  #extract tmin
  min<-extract(tmin,locations,layer=1)
  
  #calculate the degree days
  degree<-((max+min)/2)-5
  degree[degree<0]<-0
  calculated.degrees<-rowSums(degree)
  degree_day.5[,i+1]<-calculated.degrees
  removeTmpFiles(h=0.001)
}


degree_day.5<-as.data.frame(degree_day.5)
str(degree_day.5)
for(i in 1:41){
  degree_day.5[,i+1]<-as.numeric(degree_day.5[,i+1])
}

annual.average.dd.5<-rowMeans(degree_day.5[,2:42],na.rm = TRUE)
annual.average.dd.sd.5<-apply(degree_day.5[,2:42],1,sd,na.rm=T)


loc$AnnualDD.5<-annual.average.dd.5
#loc$AnnualDD.sd<-annual.average.dd.sd
str(loc)


#DD10#####
degree_day.10<-matrix(NA,length(loc$Loc),42)
degree_day.10[,1]<-loc$Loc

for(i in 1:41){
  #download tmax
  tmax.name<-paste0("tmax.",years[i],".nc")
  tmax<- brick(tmax.name, varname = "tmax")
  tmax<-rotate(tmax)
  #extract tmax
  max<-extract(tmax,locations,layer=1)
  #download tmin
  tmin.name<-paste0("tmin.",years[i],".nc")
  tmin<- brick(tmin.name, varname = "tmin")
  tmin<-rotate(tmin)
  #extract tmin
  min<-extract(tmin,locations,layer=1)
  
  #calculate the degree days
  degree<-((max+min)/2)-10
  degree[degree<0]<-0
  calculated.degrees<-rowSums(degree)
  degree_day.10[,i+1]<-calculated.degrees
  removeTmpFiles(h=0.001)
}


degree_day.10<-as.data.frame(degree_day.10)
str(degree_day.10)
for(i in 1:41){
  degree_day.10[,i+1]<-as.numeric(degree_day.10[,i+1])
}

annual.average.dd.10<-rowMeans(degree_day.10[,2:42],na.rm = TRUE)
annual.average.dd.sd.10<-apply(degree_day.10[,2:42],1,sd,na.rm=T)


loc$AnnualDD.10<-annual.average.dd.10
#loc$AnnualDD.sd<-annual.average.dd.sd
str(loc)





#write.csv(loc,"locations_with_DD.csv",row.names = FALSE)

