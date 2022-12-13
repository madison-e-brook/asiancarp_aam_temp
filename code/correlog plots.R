
library(ape)
library(ncf)
library(raster)
library(dplyr)


setwd("~/Documents/Asian carp age at maturity project/Final Data")

aamloc<-read.csv("final complete data.csv")
#remove all Black carp
DATA<-aamloc[aamloc$Species!="Black",]


#plots of regression residuals#####
png("annual residuals corr.png", width= 2404, height= 1600, units="px", res = 300)
par(mfrow=c(2,1),mar=c(4,4,2,2))
resid<-lm(log(AAM)~AnnualTemp,data=DATA)

test<-correlog(DATA[,9],DATA[,8],resid$residuals,increment=50,resamp=500,latlon=T)
plot(test, main="Annual Average Temperature Regression Residuals")
abline(h=0)
text(17400,min(test$correlation)+1,"A",cex=1.5)

plot(test, main="",xlim=c(0,2500))
text(2500,min(test$correlation)+1,"B",cex=1.5)
abline(h=0)
dev.off()


png("warm residuals corr.png", width= 2404, height= 1600, units="px", res = 300)
par(mfrow=c(2,1),mar=c(4,4,2,2))
resid<-lm(log(AAM)~WarmTemp,data=DATA)

test<-correlog(DATA[,9],DATA[,8],resid$residuals,increment=50,resamp=500,latlon=T)
plot(test, main="Warmest Quarter Temperature Regression Residuals")
abline(h=0)
text(17400,min(test$correlation)+1,"A",cex=1.5)

plot(test, main="",xlim=c(0,2500))
text(2500,min(test$correlation)+1,"B",cex=1.5)
abline(h=0)
dev.off()


png("cold residuals corr.png", width= 2404, height= 1600, units="px", res = 300)
par(mfrow=c(2,1),mar=c(4,4,2,2))
resid<-lm(log(AAM)~ColdTemp,data=DATA)

test<-correlog(DATA[,9],DATA[,8],resid$residuals,increment=50,resamp=500,latlon=T)
plot(test, main="Coldest Quarter Temperature Regression Residuals")
abline(h=0)
text(17400,min(test$correlation)+1,"A",cex=1.5)

plot(test, main="",xlim=c(0,2500))
text(2500,min(test$correlation)+1,"B",cex=1.5)
abline(h=0)
dev.off()

png("dd residuals corr.png", width= 2404, height= 1600, units="px", res = 300)
par(mfrow=c(2,1),mar=c(4,4,2,2))
resid<-lm(log(AAM)~AnnualDD,data=DATA)

test<-correlog(DATA[,9],DATA[,8],resid$residuals,increment=50,resamp=500,latlon=T)
plot(test, main="Annual Average Degree Days Regression Residuals")
abline(h=0)
text(17400,min(test$correlation)+1,"A",cex=1.5)

plot(test, main="",xlim=c(0,2500))
text(2500,min(test$correlation)+1,"B",cex=1.5)
abline(h=0)
dev.off()

#plots of temperature data####
setwd("~/Documents/aam ms/Final graphs/correlog plots")

png("annual corr.png", width= 2404, height= 1600, units="px", res = 300)
par(mfrow=c(2,1),mar=c(4,4,2,2))
test<-correlog(DATA[,9],DATA[,8],DATA$AnnualTemp,increment=50,resamp=500,latlon=T)
plot(test, main="Annual Average Temperature (°C)")
abline(h=0)
text(17400,min(test$correlation)+1,"A",cex=1.5)

plot(test, main="",xlim=c(0,2500))
text(2500,min(test$correlation)+1,"B",cex=1.5)
abline(h=0)
dev.off()

png("warm corr.png", width= 2404, height= 1600, units="px", res = 300)
par(mfrow=c(2,1),mar=c(4,4,2,2))
test<-correlog(DATA[,9],DATA[,8],DATA$WarmTemp,increment=50,resamp=500,latlon=T)
plot(test, main="Warmest Quarter Temperature (°C)")
abline(h=0)
text(17400,min(test$correlation)+1,"A",cex=1.5)


plot(test, main="",xlim=c(0,2500))
text(2500,min(test$correlation)+1,"B",cex=1.5)
abline(h=0)
dev.off()

png("cold corr.png", width= 2404, height= 1600, units="px", res = 300)
par(mfrow=c(2,1),mar=c(4,4,2,2))
test<-correlog(DATA[,9],DATA[,8],DATA$ColdTemp,increment=25,resamp=500,latlon=T)
plot(test, main="Coldest Quarter Temperature (°C)")
abline(h=0)
text(17400,min(test$correlation)+1,"A",cex=1.5)

plot(test, main="",xlim=c(0,1000))
text(1000,min(test$correlation)+1,"B",cex=1.5)
abline(h=0)
dev.off()

png("dd corr.png", width= 2404, height= 1600, units="px", res = 300)
par(mfrow=c(2,1),mar=c(4,4,2,2))
test<-correlog(DATA[,9],DATA[,8],DATA$AnnualDD,increment=25,resamp=500,latlon=T)
plot(test, main="Annual Average Degree Days")
abline(h=0)
text(17400,min(test$correlation)+1,"A",cex=1.5)

plot(test, main="",xlim=c(0,1000))
text(1000,min(test$correlation)+1,"B",cex=1.5)
abline(h=0)
dev.off()


