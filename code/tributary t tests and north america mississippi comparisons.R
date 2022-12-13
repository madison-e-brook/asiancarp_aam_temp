
library(raster)
library(sp)
library(maps)
library(maptools)
library(dplyr)

#final complete data
aamloc<-read.csv(file.choose())
str(aamloc)
#remove black carp
DATA<-aamloc[aamloc$Species!="Black",]
DATA$Species<-as.factor(DATA$Species)
DATA$Code<-as.factor(DATA$Code)
str(DATA)
#download k folds
#annual K folds.csv
annual<-read.csv(file.choose())
ann.slope<-mean(annual[,"slope"])
ann.int<-mean(annual[,"intercept"])


#DD K folds.csv
dd<-read.csv(file.choose())
dd.slope<-mean(dd[,"slope"])
dd.int<-mean(dd[,"intercept"])

#download maps
#NA annual average worldclim 1km.grd
NA.map.annual<-raster(file.choose())
plot(NA.map.annual)

#GDD0_mean.tif
NA.map.dd<-raster(file.choose())
plot(NA.map.dd)

#make a map of predicted age at maturity
annual.raw<-ann.int+(ann.slope*NA.map.annual)
aam.annual<-exp(annual.raw)
plot(aam.annual)

#degree day map
dd.raw<-dd.int+(dd.slope*NA.map.dd)
dd.annual<-exp(dd.raw)
plot(dd.annual)

#exclude ages too old
aam.annual[aam.annual>11]=NA
dd.annual[dd.annual>11]=NA


#map confidence intervals####
#n is the number of independent locations
sub<-DATA %>% group_by(Code) %>% sample_n(size=1)

x.annual<-DATA$AnnualTemp
y.annual<-log(DATA$AAM)
n<-nrow(sub)

pred.x.annual<-NA.map.annual
pred.y.annual<-annual.raw
pred.y.annual[pred.y.annual>log(11)]=NA
y.fitted.annual<-ann.int+(ann.slope*x.annual)

sse.annual <- sum((y.annual - y.fitted.annual)^2)
mse.annual <- sse.annual / (n - 2)
t.value<-qt(0.975, n - 2) 

mean.se.fit.annual <- (1 / n + (pred.x.annual - mean(x.annual))^2 / (sum((x.annual - mean(x.annual))^2)))
mean.conf.upper.annual <- pred.y.annual + t.value * sqrt(mse.annual * mean.se.fit.annual)
mean.conf.lower.annual <- pred.y.annual - t.value * sqrt(mse.annual * mean.se.fit.annual)

plot(mean.conf.upper.annual)
plot(mean.conf.lower.annual)

#degree days
x.dd<-DATA$AnnualDD
y.dd<-log(DATA$AAM)
n<-nrow(sub)

pred.x.dd<-NA.map.dd
pred.y.dd<-dd.raw
pred.y.dd[pred.y.dd>log(11)]=NA
y.fitted.dd<-dd.int+(dd.slope*x.dd)

sse.dd <- sum((y.dd - y.fitted.dd)^2)
mse.dd <- sse.dd / (n - 2)
t.value<-qt(0.975, n - 2) 

mean.se.fit.dd <- (1 / n + (pred.x.dd - mean(x.dd))^2 / (sum((x.dd - mean(x.dd))^2)))
mean.conf.upper.dd <- pred.y.dd + t.value * sqrt(mse.dd * mean.se.fit.dd)
mean.conf.lower.dd <- pred.y.dd - t.value * sqrt(mse.dd * mean.se.fit.dd)

plot(mean.conf.upper.dd)
plot(mean.conf.lower.dd)

#getting values for tributaries#####
#annual
locations<-c("Ontonagon","Nemadji",
             "Sheboygan", "Grand River",
             "Saginaw","Black River",
             "Etobicoke", "Humber",
             "Maumee","Vermillion")
lat<-c(46.8764,46.7035,
       43.7489,43.0579,
       43.6473, 43.0053,
       43.5848,43.6315,
       41.7091,41.4264) 
long<-c(-89.3272,-92.0257,
        -87.7024,-86.2512,
        -83.8495,-82.4191,
        -79.5412,-79.4706,
        -83.4405,-82.3642)

#extract values for locations
coords<-data.frame(longitude=long, latitude=lat)
loc.annual<-SpatialPoints(coords, proj4string = aam.annual@crs)
variables.annual<-extract(aam.annual,loc.annual)
variables.annual<-as.data.frame(variables.annual)

#extract confidence intervals
variables.annual.upper<-extract(mean.conf.upper.annual,loc.annual)
variables.annual.upper<-as.data.frame(exp(variables.annual.upper))

variables.annual.lower<-extract(mean.conf.lower.annual,loc.annual)
variables.annual.lower<-as.data.frame(exp(variables.annual.lower))

#annual values
loc_df.annual<-cbind(locations,coords,variables.annual,variables.annual.upper,variables.annual.lower)

loc_df.annual[,4:6]<-round(loc_df.annual[,4:6],1)
loc_df.annual

#degree days####
loc.dd<-SpatialPoints(coords, proj4string = dd.annual@crs)
variables.dd<-extract(dd.annual,loc.dd)
variables.dd<-as.data.frame(variables.dd)

variables.dd.upper<-extract(mean.conf.upper.dd,loc.dd)
variables.dd.upper<-as.data.frame(exp(variables.dd.upper))

variables.dd.lower<-extract(mean.conf.lower.dd,loc.dd)
variables.dd.lower<-as.data.frame(exp(variables.dd.lower))

loc_df.dd<-cbind(locations,coords,variables.dd,variables.dd.upper,variables.dd.lower)

loc_df.dd[,4:6]<-round(loc_df.dd[,4:6],1)
loc_df.dd

#t tests####

variables.ann<-cbind(variables.annual,rep(0,5))
variables.dd.t<-cbind(variables.dd,rep(1,5))

names(variables.ann)<-c("aam","temp")
names(variables.dd.t)<-c("aam","temp")
test<-rbind(variables.ann,variables.dd.t)


boxplot(test$aam~test$temp)

tt<-t.test(aam~temp, data=test, paired=T)
tt
#the degree day was 0.75 years larger than the annual

#t test of differnce in confidence intervals####
loc_df.annual[,5]
dif.ann<-loc_df.annual[,5]-loc_df.annual[,6]
dif.ann

dif.dd<-loc_df.dd[,5]-loc_df.dd[,6]
dif.dd

difference.ann<-cbind(dif.ann,rep(0,5))
difference.dd.t<-cbind(dif.dd,rep(1,5))

names(difference.ann)<-c("dif","temp")
names(difference.dd.t)<-c("dif","temp")
dif<-rbind(difference.ann,difference.dd.t)
dif<-as.data.frame(dif)
colnames(dif)<-c("dif","temp")

boxplot(dif$dif~dif$temp)

tt.dif<-t.test(dif~temp, data=dif,var.equal = T, paired=T)
tt.dif

#test for equal variance
library(car)
var.test(aam ~ temp, test, 
         alternative = "two.sided")
dif$temp<-as.factor(dif$temp)
leveneTest(dif ~ temp,data=dif)






#Mississippi and other North America Locations#####
locations<-c("Mississippi","Alabama","Arkansas","Cuba","Missouri_river")
lat<-c(37.9458,32.3508,35.4877,21.5079,39.3148) 
long<-c(-89.8858,-87.0124,-92.2701,-77.8094,-93.4119)

#extract values
coords<-data.frame(longitude=long, latitude=lat)
loc.annual<-SpatialPoints(coords, proj4string = aam.annual@crs)
variables.annual<-extract(aam.annual,loc.annual)
variables.annual<-as.data.frame(variables.annual)

variables.annual.upper<-extract(mean.conf.upper.annual,loc.annual)
variables.annual.upper<-as.data.frame(exp(variables.annual.upper))

variables.annual.lower<-extract(mean.conf.lower.annual,loc.annual)
variables.annual.lower<-as.data.frame(exp(variables.annual.lower))

#annual values
loc_df.annual<-cbind(locations,coords,variables.annual,variables.annual.upper,variables.annual.lower)
loc_df.annual

#degree day values
loc.dd<-SpatialPoints(coords, proj4string = dd.annual@crs)
variables.dd<-extract(dd.annual,loc.dd)
variables.dd<-as.data.frame(variables.dd)

variables.dd.upper<-extract(mean.conf.upper.dd,loc.dd)
variables.dd.upper<-as.data.frame(exp(variables.dd.upper))

variables.dd.lower<-extract(mean.conf.lower.dd,loc.dd)
variables.dd.lower<-as.data.frame(exp(variables.dd.lower))

loc_df.dd<-cbind(locations,coords,variables.dd,variables.dd.upper,variables.dd.lower)
loc_df.dd



