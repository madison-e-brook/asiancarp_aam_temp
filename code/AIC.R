setwd("~/Documents/Asian carp age at maturity project/Final Data")
aamloc<-read.csv("final complete data.csv")
library(dplyr)
#remove all Black carp
DATA<-aamloc[aamloc$Species!="Black",]
str(DATA)

#AIC of total (not sampled) data#####

#annual
ann.lin.aic<-AIC(lm(DATA$AAM~DATA$AnnualTemp))
ann.non.aic<-AIC(nls(AAM~exp(int-slope*AnnualTemp), data=DATA,
                     start=list(int=2,slope=0.044)))

#warm
warm.lin.aic<-AIC(lm(DATA$AAM~DATA$WarmTemp))
warm.non.aic<-AIC(nls(AAM~exp(int-slope*WarmTemp), data=DATA,
                      start=list(int=2,slope=0.044)))

#cold
cold.lin.aic<-AIC(lm(DATA$AAM~DATA$ColdTemp))
cold.non.aic<-AIC(nls(AAM~exp(int-slope*ColdTemp), data=DATA,
                      start=list(int=2,slope=0.044)))

#dd
dd.lin.aic<-AIC(lm(DATA$AAM~DATA$AnnualDD))
dd.non.aic<-AIC(nls(AAM~exp(int-slope*AnnualDD), data=DATA,
                      start=list(int=2.9,slope=0.00016)))


#with subsampling at 250km####

#annual temp####
ann.lin.stor<-matrix(NA,10000,1)
for(i in 1:10000){
  sub<-DATA %>% group_by(Code) %>% sample_n(size=1)
  reg<-lm(sub$AAM~sub$AnnualTemp)
  ann.lin.stor[i,1]<-as.numeric(AIC(reg))
  
}

ann.exp.stor<-matrix(NA,10000,1)
for(i in 1:10000){
  sub<-DATA %>% group_by(Code) %>% sample_n(size=1)
  reg<-nls(AAM~exp(int-slope*AnnualTemp), data=sub,
           start=list(int=2,slope=0.044))
  ann.exp.stor[i,1]<-as.numeric(AIC(reg))
  
}

#number of times non linear was a better fit
sum(ann.exp.stor[,1]<ann.lin.stor[,1])

#percent
sum(ann.exp.stor[,1]<ann.lin.stor[,1])/100


#warm temp####
warm.lin.stor<-matrix(NA,10000,1)
for(i in 1:10000){
  sub<-DATA %>% group_by(Code) %>% sample_n(size=1)
  reg<-lm(sub$AAM~sub$WarmTemp)
  warm.lin.stor[i,1]<-as.numeric(AIC(reg))
  
}

warm.exp.stor<-matrix(NA,10000,1)
for(i in 1:10000){
  sub<-DATA %>% group_by(Code) %>% sample_n(size=1)
  reg<-nls(AAM~exp(int-slope*WarmTemp), data=sub,
           start=list(int=2,slope=0.044))
  warm.exp.stor[i,1]<-as.numeric(AIC(reg))
  
}

#number of times non linear was a better fit
sum(warm.exp.stor[,1]<warm.lin.stor[,1])

#percent
sum(warm.exp.stor[,1]<warm.lin.stor[,1])/100



#cold temp####
cold.lin.stor<-matrix(NA,10000,1)
for(i in 1:10000){
  sub<-DATA %>% group_by(Code) %>% sample_n(size=1)
  reg<-lm(sub$AAM~sub$ColdTemp)
  cold.lin.stor[i,1]<-as.numeric(AIC(reg))
  
}

cold.exp.stor<-matrix(NA,10000,1)
for(i in 1:10000){
  sub<-DATA %>% group_by(Code) %>% sample_n(size=1)
  reg<-nls(AAM~exp(int-slope*ColdTemp), data=sub,
           start=list(int=2,slope=0.044))
  cold.exp.stor[i,1]<-as.numeric(AIC(reg))
  
}

#number of times non linear was a better fit
sum(cold.exp.stor[,1]<cold.lin.stor[,1])

#percent
sum(cold.exp.stor[,1]<cold.lin.stor[,1])/100


#degree days####
dd.lin.stor<-matrix(NA,10000,1)
for(i in 1:10000){
  sub<-DATA %>% group_by(Code) %>% sample_n(size=1)
  reg<-lm(sub$AAM~sub$AnnualDD)
  dd.lin.stor[i,1]<-as.numeric(AIC(reg))
  
}

dd.exp.stor<-matrix(NA,10000,1)
for(i in 1:10000){
  sub<-DATA %>% group_by(Code) %>% sample_n(size=1)
  reg<-nls(AAM~exp(int-slope*AnnualDD), data=sub,
           start=list(int=2.9,slope=0.00016))
  dd.exp.stor[i,1]<-as.numeric(AIC(reg))
  
}

#number of times non linear was a better fit
sum(dd.exp.stor[,1]<dd.lin.stor[,1])

#percent
sum(dd.exp.stor[,1]<dd.lin.stor[,1])/100


#summary#####
mean(ann.lin.stor[,1])
mean(ann.exp.stor[,1])

mean(warm.lin.stor[,1])
mean(warm.exp.stor[,1])

mean(cold.lin.stor[,1])
mean(cold.exp.stor[,1])

mean(dd.lin.stor[,1])
mean(dd.exp.stor[,1])

sum(ann.exp.stor[,1]<ann.lin.stor[,1])/100
sum(warm.exp.stor[,1]<warm.lin.stor[,1])/100
sum(cold.exp.stor[,1]<cold.lin.stor[,1])/100
sum(dd.exp.stor[,1]<dd.lin.stor[,1])/100
