library(dplyr)
library(rstatix)

#final complete data.csv
DATA<-read.csv(file.choose())

DATA$Species<-as.factor(DATA$Species)
DATA$Code<-as.factor(DATA$Code)

aamloc<- DATA %>% group_by(Code) %>% sample_n(size=1)

#estimate outliers than may impact results
#dd.0
lm.0<-lm(log(aamloc$AAM)~aamloc$AnnualDD)
estimated.0<-lm.0$coef[1]+lm.0$coef[2]*aamloc$AnnualDD

#dd.5
lm.5<-lm(log(aamloc$AAM)~aamloc$AnnualDD.5)
estimated.5<-lm.5$coef[1]+lm.5$coef[2]*aamloc$AnnualDD.5

#dd.10
lm.10<-lm(log(aamloc$AAM)~aamloc$AnnualDD.10)
estimated.10<-lm.10$coef[1]+lm.10$coef[2]*aamloc$AnnualDD.10


e.0<-cbind(1:length(estimated.0),estimated.0,rep(0,length(estimated.0)))
e.5<-cbind(1:length(estimated.5),estimated.5,rep(5,length(estimated.5)))
e.10<-cbind(1:length(estimated.10),estimated.10,rep(10,length(estimated.10)))

colnames(e.0)<-c("id","estimate_AAM","base")
colnames(e.5)<-c("id","estimate_AAM","base")
colnames(e.10)<-c("id","estimate_AAM","base")


aam.base<-rbind(e.0, e.5, e.10)
aam.base<-as.data.frame(aam.base)

boxplot(aam.base$estimate_AAM~aam.base$base)

aam.base %>%
  group_by(base) %>%
  identify_outliers(estimate_AAM)

order(aamloc$AnnualDD)
order(aamloc$AnnualDD.5)
order(aamloc$AnnualDD.10)

#the locations with the highest degree days are outliers
#locations 3 (Malacca), 35 (Cuttack), 15 (Tamilnadu), and 34 (Cuba) were therefore removed

#remove the locations
#and run 10 000 trials to determine if the degree day bases make a difference

#10 000 trials #####

trials<-matrix(NA,10000,8)
colnames(trials)<-c("DFn",
                    "DFd",
                    "F",
                    "p value",
                    "ges",
                    "normal 0",
                    "normal 5",
                    "normal 10")

for(i in 1:nrow(trials)){
  aamloc<- DATA %>% group_by(Code) %>% sample_n(size=1)
  
  #get the predicted ages at maturity for each degree day calculation
  lm.0<-lm(log(aamloc$AAM)~aamloc$AnnualDD)
  estimated.0<-lm.0$coef[1]+lm.0$coef[2]*aamloc$AnnualDD
  
  #dd.5
  lm.5<-lm(log(aamloc$AAM)~aamloc$AnnualDD.5)
  estimated.5<-lm.5$coef[1]+lm.5$coef[2]*aamloc$AnnualDD.5
  
  #dd.10
  lm.10<-lm(log(aamloc$AAM)~aamloc$AnnualDD.10)
  estimated.10<-lm.10$coef[1]+lm.10$coef[2]*aamloc$AnnualDD.10
  
  e.0<-cbind(1:length(estimated.0),estimated.0,rep(0,length(estimated.0)))
  e.5<-cbind(1:length(estimated.5),estimated.5,rep(5,length(estimated.5)))
  e.10<-cbind(1:length(estimated.10),estimated.10,rep(10,length(estimated.10)))
  
  colnames(e.0)<-c("id","estimate_AAM","base")
  colnames(e.5)<-c("id","estimate_AAM","base")
  colnames(e.10)<-c("id","estimate_AAM","base")
  
  #clean data types
  test<-rbind(e.0, e.5, e.10)
  test<-as.data.frame(test)
  test$id<-as.factor(test$id)
  test$base<-as.factor(test$base)
  
  #ensure outliers were not selected
  test<-test[test$id!=3 & test$id!=35 & test$id!=34 & test$id!=15,]
  
  #test for normality
  shap<-test %>%
    group_by(base) %>%
    shapiro_test(estimate_AAM)
  
  #run anova
  res.aov <- anova_test(data = test, dv = estimate_AAM, wid = id, within = base)
  
  summ<-get_anova_table(res.aov)
  
  #record results
  trials[i,"DFn"]<-summ[[2]]
  trials[i,"DFd"]<-summ[[3]]
  trials[i,"F"]<-summ[[4]]
  trials[i,"p value"]<-summ[[5]]
  trials[i,"ges"]<-summ[[7]]
  trials[i,"normal 0"]<-as.numeric(shap[1,4])
  trials[i,"normal 5"]<-as.numeric(shap[2,4])
  trials[i,"normal 10"]<-as.numeric(shap[3,4])
}


#write.csv(trials, "repeated measures.csv",row.names = F)
trials<-as.data.frame(trials)

#all p values were significant
sum(trials$`p value`>0.05)



