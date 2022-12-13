library(dplyr)
library(car)
#download data####
#final complete data.csv
DATA<-read.csv(file.choose())

#correct fators
DATA$Species<-as.factor(DATA$Species)
DATA$Loc<-as.factor(DATA$Loc)
DATA$Sp_Code<-as.factor(DATA$Sp_Code)
str(DATA)

#subsample the data
sub<-DATA %>% group_by(Sp_Code) %>% sample_n(size=1)
sum(sub$Species=="Silver") #17 silver
sum(sub$Species=="Bighead") #16 bighead
sum(sub$Species=="Grass") #32 grass
sum(sub$Species=="Black") #12 black
#so we will sample 17 from grass to make them approx equal

DATA<-DATA[DATA$Species!="Black",]

#annual ancova####
annual<-matrix(NA,10000,2) #to store the p values of the interaction
colnames(annual)<-c("interaction p value","shapiro")

#annual####
for(i in 1:nrow(annual)){
  sub<-DATA %>% group_by(Sp_Code) %>% sample_n(size=1)
  Other<-sub[sub$Species!="Grass",]
  Grass<-sub[sub$Species=="Grass",]
  grass_s<-Grass %>% group_by(Species) %>%sample_n(size=17)
  analysis<-bind_rows(Other,grass_s)
  anc<-lm(log(AAM)~AnnualTemp*Species,data=analysis)
  ANC<-Anova(anc,type=3)
  annual[i,1]<-ANC$`Pr(>F)`[4]
  annual[i,2]<-as.numeric(shapiro.test(anc$residuals)[2])
}

#warm####
warm<-matrix(NA,10000,2) #to store the p values of the interaction
colnames(warm)<-c("interaction p value","shapiro")

for(i in 1:nrow(warm)){
  sub<-DATA %>% group_by(Sp_Code) %>% sample_n(size=1)
  Other<-sub[sub$Species!="Grass",]
  Grass<-sub[sub$Species=="Grass",]
  grass_s<-Grass %>% group_by(Species) %>%sample_n(size=17)
  analysis<-bind_rows(Other,grass_s)
  anc<-lm(log(AAM)~WarmTemp*Species,data=analysis)
  ANC<-Anova(anc,type=3)
  warm[i,1]<-ANC$`Pr(>F)`[4]
  warm[i,2]<-as.numeric(shapiro.test(anc$residuals)[2])
}

#cold####
cold<-matrix(NA,10000,2) #to store the p values of the interaction
colnames(cold)<-c("interaction p value","shapiro")

for(i in 1:nrow(cold)){
  sub<-DATA %>% group_by(Sp_Code) %>% sample_n(size=1)
  Other<-sub[sub$Species!="Grass",]
  Grass<-sub[sub$Species=="Grass",]
  grass_s<-Grass %>% group_by(Species) %>%sample_n(size=17)
  analysis<-bind_rows(Other,grass_s)
  anc<-lm(log(AAM)~ColdTemp*Species,data=analysis)
  ANC<-Anova(anc,type=3)
  cold[i,1]<-ANC$`Pr(>F)`[4]
  cold[i,2]<-as.numeric(shapiro.test(anc$residuals)[2])
}

#DD####
DD<-matrix(NA,10000,2) #to store the p values of the interaction
colnames(DD)<-c("interaction p value","shapiro")

for(i in 1:nrow(DD)){
  sub<-DATA %>% group_by(Sp_Code) %>% sample_n(size=1)
  Other<-sub[sub$Species!="Grass",]
  Grass<-sub[sub$Species=="Grass",]
  grass_s<-Grass %>% group_by(Species) %>%sample_n(size=17)
  analysis<-bind_rows(Other,grass_s)
  anc<-lm(log(AAM)~AnnualDD*Species,data=analysis)
  ANC<-Anova(anc,type=3)
  DD[i,1]<-ANC$`Pr(>F)`[4]
  DD[i,2]<-as.numeric(shapiro.test(anc$residuals)[2])
}




#investigating data####
#annual
sum(annual[,1]<0.05) 
(sum(annual[,1]<0.05)/10000)*100
#percent not significant:
100-(sum(annual[,1]<0.05)/10000)*100

(sum(annual[,2]>0.05)/10000)*100

#warm
sum(warm[,1]<0.05)
(sum(warm[,1]<0.05)/10000)*100
#percent not significant:
100-(sum(warm[,1]<0.05)/10000)*100


100-(sum(warm[,2]<0.05)/10000)*100

#cold
sum(cold[,1]<0.05)
(sum(cold[,1]<0.05)/10000)*100
#percent not significant:
100-(sum(cold[,1]<0.05)/10000)*100

100-(sum(cold[,2]<0.05)/10000)*100

#degree days
sum(DD[,1]<0.05)
(sum(DD[,1]<0.05)/10000)*100
#percent not significant:
100-(sum(DD[,1]<0.05)/10000)*100

100-(sum(DD[,2]<0.05)/10000)*100

