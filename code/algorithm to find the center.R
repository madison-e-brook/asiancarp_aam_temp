

#import "raw location data.csv"
loc<-read.csv(file.choose())
str(loc)

#convert all lat/longs to radians
loc[,4:11]<-(loc[,4:11])*(pi/180)
head(loc)

#calculate X,Y and Z for each location

#make empty dataframe
locname<-loc$Loc
Xn<-NA
Xs<-NA
Xe<-NA
Xw<-NA

Xi<-data.frame(locname,Xn,Xe,Xs,Xw)
head(Xi)

for(i in 1:length(Xi$locname)){
  #north
  Xi[i,2]<-cos(loc[i,4])*cos(loc[i,5])
  #East
  Xi[i,3]<-cos(loc[i,6])*cos(loc[i,7])
  #south
  Xi[i,4]<-cos(loc[i,8])*cos(loc[i,9])
  #west
  Xi[i,5]<-cos(loc[i,10])*cos(loc[i,11])
}
head(Xi)

#Y
Yn<-NA
Ys<-NA
Ye<-NA
Yw<-NA

Yi<-data.frame(locname,Yn,Ye,Ys,Yw)
head(Yi)

for(i in 1:length(Yi$locname)){
  Yi[i,2]<-cos(loc[i,4])*sin(loc[i,5])
  Yi[i,3]<-cos(loc[i,6])*sin(loc[i,7])
  Yi[i,4]<-cos(loc[i,8])*sin(loc[i,9])
  Yi[i,5]<-cos(loc[i,10])*sin(loc[i,11])
}
head(Yi)

#Z
Zn<-NA
Zs<-NA
Ze<-NA
Zw<-NA

Zi<-data.frame(locname,Zn,Ze,Zs,Zw)
head(Zi)

for(i in 1:length(Zi$locname)){
  Zi[i,2]<-sin(loc[i,4])
  Zi[i,3]<-sin(loc[i,6])
  Zi[i,4]<-sin(loc[i,8])
  Zi[i,5]<-sin(loc[i,10])
}
head(Zi)

#take averages for each X,Y,Z
Xave<-rowMeans(Xi[,2:5])
Yave<-rowMeans(Yi[,2:5])
Zave<-rowMeans(Zi[,2:5])


#convert back to cartesian coordinates
hypo<-sqrt((Xave*Xave)+(Yave*Yave))
head(hypo)

Lat<-atan2(Zave,hypo)
Long<-atan2(Yave,Xave)

#Convert back to degrees
Latitude<-Lat*(180/pi)
Longitude<-Long*(180/pi)


#Save select columns as a csv for future analysis

locations_no_temps<-data.frame(loc$Loc,Latitude,Longitude)
locations_no_temps


#export the csv. make sure you have working directory set
write.csv(locations_no_temps,"locations_no_temps.csv",row.names = FALSE)



