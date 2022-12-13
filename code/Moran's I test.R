library(ape)
library(ncf)
library(raster)
library(dplyr)

#import data####
#final complete data
aamloc<-read.csv(file.choose())
str(aamloc)

my.aam<-aamloc[aamloc$Species!="Black",]

str(my.aam)

#import the raster layer of the world####
map<-getData('worldclim', var='bio', res=5)
map<-map[[1]]
map<-map$bio1/10
plot(map)


#moran's I on full dataset#####
#make spatial dataframe
coords<-data.frame("long"=DATA[,8],"lat"=DATA[,7])
df = data.frame(a = 1:nrow(DATA[8]))
spatial.data<-SpatialPointsDataFrame(coords,df,proj4string = map@crs)

#get distances from all points
dists<-spDists(spatial.data,longlat = TRUE)
diag(dists) <- 0 

#get residuals from whole dataset for each variable
lm.annual<-lm(log(my.aam$AAM)~my.aam$AnnualTemp)
lm.warm<-lm(log(my.aam$AAM)~my.aam$WarmTemp)
lm.cold<-lm(log(my.aam$AAM)~my.aam$ColdTemp)
lm.dd<-lm(log(my.aam$AAM)~my.aam$AnnualDD)
lm.dd5<-lm(log(my.aam$AAM)~my.aam$AnnualDD.5)
lm.dd10<-lm(log(my.aam$AAM)~my.aam$AnnualDD.10)

resid.annual<-lm.annual$residuals
resid.warm<-lm.warm$residuals
resid.cold<-lm.cold$residuals
resid.dd<-lm.dd$residuals
resid.dd.5<-lm.dd5$residuals
resid.dd.10<-lm.dd10$residuals

Moran.annual<-Moran.I(resid.annual,dists)
Moran.annual

Moran.warm<-Moran.I(resid.warm,dists)
Moran.warm

Moran.cold<-Moran.I(resid.cold,dists)
Moran.cold

Moran.dd<-Moran.I(resid.dd,dists)
Moran.dd

Moran.dd.5<-Moran.I(resid.dd.5,dists)
Moran.dd.5

Moran.dd.10<-Moran.I(resid.dd.10,dists)
Moran.dd.10

#all significantly autocorrelated except for degree days

#subsamples####


#annual####
moran.annual<-matrix(NA,10000,1)

for(i in 1:nrow(moran.annual)){
  sub<-my.aam %>% group_by(Code) %>% sample_n(size=1)
  reg.sub<-lm(log(sub$AAM)~sub$AnnualTemp)
  
  coords.sub<-data.frame("long"=sub[,8],"lat"=sub[,7])
  df.sub = data.frame(a = 1:nrow(sub[8]))
  spatial.data.sub<-SpatialPointsDataFrame(coords.sub,df.sub,proj4string = map@crs)
  
  dists.sub<-spDists(spatial.data.sub,longlat = TRUE)
  diag(dists.sub) <- 0 
  
  moran.annual[i,1]<- Moran.I(reg.sub$residuals,dists.sub)$p.value
}

hist(moran.annual[,1])
sum(moran.annual[,1]<0.05)
#percent not significant
100-(sum(moran.annual[,1]<0.05)/10000)*100

#warm####
moran.warm<-matrix(NA,10000,1)

for(i in 1:nrow(moran.warm)){
  sub<-my.aam %>% group_by(Code) %>% sample_n(size=1)
  reg.sub<-lm(log(sub$AAM)~sub$WarmTemp)
  
  coords.sub<-data.frame("long"=sub[,8],"lat"=sub[,7])
  df.sub = data.frame(a = 1:nrow(sub[8]))
  spatial.data.sub<-SpatialPointsDataFrame(coords.sub,df.sub,proj4string = map@crs)
  
  dists.sub<-spDists(spatial.data.sub,longlat = TRUE)
  diag(dists.sub) <- 0 
  
  moran.warm[i,1]<- Moran.I(reg.sub$residuals,dists.sub)$p.value
}

hist(moran.warm[,1])
sum(moran.warm[,1]<0.05)
#percent not significant
100-(sum(moran.warm[,1]<0.05)/10000)*100



#cold####
moran.cold<-matrix(NA,10000,1)

for(i in 1:nrow(moran.cold)){
  sub<-my.aam %>% group_by(Code) %>% sample_n(size=1)
  reg.sub<-lm(log(sub$AAM)~sub$ColdTemp)
  
  coords.sub<-data.frame("long"=sub[,8],"lat"=sub[,7])
  df.sub = data.frame(a = 1:nrow(sub[8]))
  spatial.data.sub<-SpatialPointsDataFrame(coords.sub,df.sub,proj4string = map@crs)
  
  dists.sub<-spDists(spatial.data.sub,longlat = TRUE)
  diag(dists.sub) <- 0 
  
  moran.cold[i,1]<- Moran.I(reg.sub$residuals,dists.sub)$p.value
}

hist(moran.cold[,1])
sum(moran.cold[,1]<0.05)
#percent not significant
100-(sum(moran.cold[,1]<0.05)/10000)*100



#dd####
moran.dd<-matrix(NA,10000,1)

for(i in 1:nrow(moran.dd)){
  sub<-my.aam %>% group_by(Code) %>% sample_n(size=1)
  reg.sub<-lm(log(sub$AAM)~sub$AnnualDD)
  
  coords.sub<-data.frame("long"=sub[,8],"lat"=sub[,7])
  df.sub = data.frame(a = 1:nrow(sub[8]))
  spatial.data.sub<-SpatialPointsDataFrame(coords.sub,df.sub,proj4string = map@crs)
  
  dists.sub<-spDists(spatial.data.sub,longlat = TRUE)
  diag(dists.sub) <- 0 
  
  moran.dd[i,1]<- Moran.I(reg.sub$residuals,dists.sub)$p.value
}

hist(moran.dd[,1])
sum(moran.dd[,1]<0.05)
#percent not significant
100-(sum(moran.dd[,1]<0.05)/10000)*100


#dd5####
moran.dd.5<-matrix(NA,10000,1)

for(i in 1:nrow(moran.dd.5)){
  sub<-my.aam %>% group_by(Code) %>% sample_n(size=1)
  reg.sub<-lm(log(sub$AAM)~sub$AnnualDD.5)
  
  coords.sub<-data.frame("long"=sub[,8],"lat"=sub[,7])
  df.sub = data.frame(a = 1:nrow(sub[8]))
  spatial.data.sub<-SpatialPointsDataFrame(coords.sub,df.sub,proj4string = map@crs)
  
  dists.sub<-spDists(spatial.data.sub,longlat = TRUE)
  diag(dists.sub) <- 0 
  
  moran.dd.5[i,1]<- Moran.I(reg.sub$residuals,dists.sub)$p.value
}

#dd10####
moran.dd.10<-matrix(NA,10000,1)

for(i in 1:nrow(moran.dd.10)){
  sub<-my.aam %>% group_by(Code) %>% sample_n(size=1)
  reg.sub<-lm(log(sub$AAM)~sub$AnnualDD.10)
  
  coords.sub<-data.frame("long"=sub[,8],"lat"=sub[,7])
  df.sub = data.frame(a = 1:nrow(sub[8]))
  spatial.data.sub<-SpatialPointsDataFrame(coords.sub,df.sub,proj4string = map@crs)
  
  dists.sub<-spDists(spatial.data.sub,longlat = TRUE)
  diag(dists.sub) <- 0 
  
  moran.dd.10[i,1]<- Moran.I(reg.sub$residuals,dists.sub)$p.value
}
#overall results####
100-(sum(moran.annual[,1]<0.05)/10000)*100
100-(sum(moran.warm[,1]<0.05)/10000)*100
100-(sum(moran.cold[,1]<0.05)/10000)*100
100-(sum(moran.dd[,1]<0.05)/10000)*100

100-(sum(moran.dd.5[,1]<0.05)/10000)*100
100-(sum(moran.dd.10[,1]<0.05)/10000)*100

