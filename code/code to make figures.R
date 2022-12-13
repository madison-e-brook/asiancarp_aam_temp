library(colorBlindness)
library(sp)
library(raster)
library(maps)
library(maptools)
library(viridis)
library(RColorBrewer)

#download data files
aamloc<-read.csv(file.choose())
str(aamloc)

DATA<-aamloc[aamloc$Species!="Black",]
DATA$Species<-as.factor(DATA$Species)
str(DATA)

#download K fold results
#annual K folds.csv
annual<-read.csv(file.choose())
ann.slope<-mean(annual[,"slope"])
ann.int<-mean(annual[,"intercept"])

#DD K folds.csv
dd<-read.csv(file.choose())
dd.slope<-mean(dd[,"slope"])
dd.int<-mean(dd[,"intercept"])

#cold K folds.csv
cold<-read.csv(file.choose())
cold.slope<-mean(cold[,"slope"])
cold.int<-mean(cold[,"intercept"])

#get subset size
sub<-DATA %>% group_by(Code) %>% sample_n(size=1)

#calculate confidence intervals around regression for all variables####
#annual
x.annual<-DATA$AnnualTemp
y.annual<-log(DATA$AAM)
n<-nrow(sub)

pred.x.annual<-seq(range(DATA$AnnualTemp)[1], range(DATA$AnnualTemp)[2],0.5)
pred.y.annual<-ann.int+(ann.slope*pred.x.annual)
y.fitted.annual<-ann.int+(ann.slope*x.annual)

sse.annual <- sum((y.annual - y.fitted.annual)^2)
mse.annual <- sse.annual / (n - 2)
t.value<-qt(0.975, n - 2) 

mean.se.fit.annual <- (1 / n + (pred.x.annual - mean(x.annual))^2 / (sum((x.annual - mean(x.annual))^2)))
mean.conf.upper.annual <- pred.y.annual + t.value * sqrt(mse.annual * mean.se.fit.annual)
mean.conf.lower.annual <- pred.y.annual - t.value * sqrt(mse.annual * mean.se.fit.annual)
plot(x.annual,y.annual)
points(pred.x.annual,mean.conf.upper.annual,type="l",col="blue")
points(pred.x.annual,mean.conf.lower.annual,type="l",col="blue")
abline(ann.int,ann.slope)

#cold
x.cold<-DATA$ColdTemp
y.cold<-log(DATA$AAM)
n<-nrow(sub)

pred.x.cold<-seq(range(DATA$ColdTemp)[1], range(DATA$ColdTemp)[2],0.5)
pred.y.cold<-cold.int+(cold.slope*pred.x.cold)
y.fitted.cold<-cold.int+(cold.slope*x.cold)

sse.cold <- sum((y.cold - y.fitted.cold)^2)
mse.cold <- sse.cold / (n - 2)
t.value<-qt(0.975, n - 2) 

mean.se.fit.cold <- (1 / n + (pred.x.cold - mean(x.cold))^2 / (sum((x.cold - mean(x.cold))^2)))
mean.conf.upper.cold <- pred.y.cold + t.value * sqrt(mse.cold * mean.se.fit.cold)
mean.conf.lower.cold <- pred.y.cold - t.value * sqrt(mse.cold * mean.se.fit.cold)

#dd
x.dd<-DATA$AnnualDD
y.dd<-log(DATA$AAM)
n<-nrow(sub)

pred.x.dd<-seq(range(DATA$AnnualDD)[1], range(DATA$AnnualDD)[2],10)
pred.y.dd<-dd.int+(dd.slope*pred.x.dd)
y.fitted.dd<-dd.int+(dd.slope*x.dd)

sse.dd <- sum((y.dd - y.fitted.dd)^2)
mse.dd <- sse.dd / (n - 2)
t.value<-qt(0.975, n - 2) 

mean.se.fit.dd <- (1 / n + (pred.x.dd - mean(x.dd))^2 / (sum((x.dd - mean(x.dd))^2)))
mean.conf.upper.dd <- pred.y.dd + t.value * sqrt(mse.dd * mean.se.fit.dd)
mean.conf.lower.dd <- pred.y.dd - t.value * sqrt(mse.dd * mean.se.fit.dd)
plot(x.dd,y.dd)
points(pred.x.dd,mean.conf.upper.dd,type="l",col="blue")
points(pred.x.dd,mean.conf.lower.dd,type="l",col="blue")
abline(dd.int,dd.slope)


#table returns whether 
#the datapoint was unique
#or was subsetted to avoid possible spatial autocorrelation
table(DATA$Code)[table(DATA$Code)>1]
uniques<-DATA[DATA$Code!="A" & DATA$Code!="AF"& DATA$Code!="AG"& DATA$Code!="AL"& DATA$Code!="AM" & DATA$Code!="B"& DATA$Code!="C" & DATA$Code!="D"& DATA$Code!="E"& DATA$Code!="F"& DATA$Code!="G"& DATA$Code!="H"& DATA$Code!="J" & DATA$Code!="M"& DATA$Code!="N"& DATA$Code!="O"& DATA$Code!="S"& DATA$Code!="Z",]
duplicates<-DATA[DATA$Code=="A" | DATA$Code=="AF"| DATA$Code=="AG"| DATA$Code=="AL"| DATA$Code=="AM" | DATA$Code=="B"| DATA$Code=="C"| DATA$Code=="D"|DATA$Code=="E"| DATA$Code=="F"| DATA$Code=="G"| DATA$Code=="H"| DATA$Code=="J"| DATA$Code=="M"| DATA$Code=="N"| DATA$Code=="O"| DATA$Code=="S"| DATA$Code=="Z",]

#plot#####
setwd("~/Documents/aam ms/Final graphs")


png("regression graphs.png", width= 2404, height= 1600, units="px", res = 300)
layout(matrix(c(1,2,3,4), 2, 2, byrow = TRUE))
par(mar=c(4,4,1,3))
plot(uniques$AnnualTemp,log(uniques$AAM),pch=c(23, 19, 17)[as.numeric(uniques$Species)],
     xlab="",bg="black",
     ylab="",las=1,xlim=c(range(DATA$AnnualTemp)[1],range(DATA$AnnualTemp)[2]),
     ylim=c(range(log(DATA$AAM))[1],range(log(DATA$AAM))[2]),col="black")
points(duplicates$AnnualTemp,log(duplicates$AAM),pch=c(5, 1, 2)[as.numeric(duplicates$Species)],
       col="black")
title(ylab=expression("ln Age at Maturity"),mgp=c(2.5,1,0))
title(xlab=expression("Annual Average Temperature (°C)"),mgp=c(2.5,1,0))

points(pred.x.annual,mean.conf.upper.annual,type="l",lty=2)
points(pred.x.annual,mean.conf.lower.annual,type="l",lty=2)
curve(ann.int+ann.slope*x,range(DATA$AnnualTemp)[1],range(DATA$AnnualTemp)[2],add=T)

plot(uniques$ColdTemp,log(uniques$AAM),pch=c(23, 19, 17)[as.numeric(uniques$Species)],
     xlab="",bg="black",
     ylab="",las=1,xlim=c(range(DATA$ColdTemp)[1],range(DATA$ColdTemp)[2]),
     ylim=c(range(log(DATA$AAM))[1],range(log(DATA$AAM))[2]),col="black")
points(duplicates$ColdTemp,log(duplicates$AAM),pch=c(5, 1, 2)[as.numeric(duplicates$Species)],
       col="black")
title(ylab=expression("ln Age at Maturity"),mgp=c(2.5,1,0))
title(xlab=expression("Coldest Quarter Temperature (°C)"),mgp=c(2.5,1,0))

points(pred.x.cold,mean.conf.upper.cold,type="l",lty=2)
points(pred.x.cold,mean.conf.lower.cold,type="l",lty=2)
curve(cold.int+cold.slope*x,range(DATA$ColdTemp)[1],range(DATA$ColdTemp)[2],add=T)

plot(uniques$AnnualDD,log(uniques$AAM),pch=c(23, 19, 17)[as.numeric(uniques$Species)],
     xlab="",bg="black",
     ylab="",las=1,xlim=c(range(DATA$AnnualDD)[1],range(DATA$AnnualDD)[2]),
     ylim=c(range(log(DATA$AAM))[1],range(log(DATA$AAM))[2]),col="black")
points(duplicates$AnnualDD,log(duplicates$AAM),pch=c(5, 1, 2)[as.numeric(duplicates$Species)],
       col="black")
title(ylab=expression("ln Age at Maturity"),mgp=c(2.5,1,0))
title(xlab=expression("Annual Average Degree Days"),mgp=c(2.5,1,0))

points(pred.x.dd,mean.conf.upper.dd,type="l",lty=2)
points(pred.x.dd,mean.conf.lower.dd,type="l",lty=2)
curve(dd.int+dd.slope*x,range(DATA$AnnualDD)[1],range(DATA$AnnualDD)[2],add=T)


par(fig = c(0, 1, 0, 1),oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0),new=T)
plot(0, 0, type = 'l', bty = 'n', xaxt = 'n', yaxt = 'n')
legend(0.4,-0.2,
       legend = c(levels(DATA$Species),"Unique","Subsampled"),
       col = "black",
       pch=c(5, 1, 2, 15, 0), xpd = TRUE, horiz = F, cex = 1.2, bty = 'n')


dev.off()


#maps####
#download maps for average annual temperature and degree days
library("raster")
NA.map.annual<-raster(file.choose())
plot(NA.map.annual)

NA.map.dd<-raster(file.choose())
plot(NA.map.dd)

#convert the maps to predcited age at maturity
annual.raw<-ann.int+(ann.slope*NA.map.annual)
aam.annual<-exp(annual.raw)
plot(aam.annual)


dd.raw<-dd.int+(dd.slope*NA.map.dd)
dd.annual<-exp(dd.raw)
plot(dd.annual)

#largest maturity we found in lit search was 11
#make all predicted ages above 11 empty
aam.annual[aam.annual>11]=NA
dd.annual[dd.annual>11]=NA

#set breaklengths
breaklength<-seq(2,11,1)
length(breaklength)

#download country outlines
data(wrld_simpl)

png("regression maps.png", width= 2655, height= 1086, units="px", res = 300)
par(mfrow=c(1,2),mar=c(1, 1, 1, 1), oma = c(1.5, 1.5, 0, 1.5))
plot(aam.annual, breaks=breaklength,col=brewer.pal(n = 9, name = "PuBu"),
     legend=F)
map("world",add=T)
map("lakes",add=T)
text(-160,80,"A",cex=2)

plot(dd.annual, breaks=breaklength,col=brewer.pal(n = 9, name = "PuBu"),
     legend=F)
map("world",add=T)
map("lakes",add=T)
text(-160,80,"B",cex=2)


par(fig = c(0, 1, 0, 1), oma = c(0, 15, 0,0), mar = c(0, 0, 0, 0), new = TRUE)
plot(dd.annual,legend.only=T,breaks=breaklength,col=brewer.pal(n = 9, name = "PuBu"),
     legend.args = list(text='      Age at\n      Maturity\n      (years)\n'))

dev.off()


#confidence intervals for maps####
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

#convert out of log space and take the difference between the upper and lower
CI.annual<-exp(mean.conf.upper.annual)-exp(mean.conf.lower.annual)
breakpoints<-seq(0,10,0.5)
plot(CI.annual)
map("world",add=T)
map("lakes",add=T)

test<-CI.annual
test[test>10]<-NA
plot(test,breaks=breakpoints, col=rainbow(length(breakpoints)))
map("world",add=T)
map("lakes",add=T)


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

CI.dd<-exp(mean.conf.upper.dd)-exp(mean.conf.lower.dd)
plot(CI.dd)
map("world",add=T)
map("lakes",add=T)
breakpoints.dd<-seq(0,6,0.5)


#plot#####

png("uncertainty map.png", width= 2655, height= 1086, units="px", res = 300)
par(mfrow=c(1,2),mar=c(1, 1, 1, 1), oma = c(1.5, 1.5, 0, 1.5))
plot(CI.annual,zlim=c(0,7),legend=F,col=rev(viridis(100)))
map("world",add=T)
map("lakes",add=T)
text(-160,80,"A",cex=2)

plot(CI.dd,zlim=c(0,7),legend=F,col=rev(viridis(100)))
map("world",add=T)
map("lakes",add=T)
text(-160,80,"B",cex=2)

par(fig = c(0, 1, 0, 1), oma = c(0, 15, 0,0), mar = c(0, 0, 0, 0), new = TRUE)
plot(CI.dd,zlim=c(0,7),legend.only=T,col=rev(viridis(100)),
     legend.args = list(text='        Uncertainty\n        (years)\n',cex=0.8))

dev.off()