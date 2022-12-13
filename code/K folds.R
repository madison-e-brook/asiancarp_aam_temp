library(caret)
library(dplyr)
#import data: final complete data.csv
aamloc<-read.csv(file.choose())
str(aamloc)

DATA<-aamloc[aamloc$Species!="Black",]

str(DATA)


#set up the method for our folds
#method will be cross validation, and we want 5 folds
data_ctrl <- trainControl(method = "cv", number = 5)


#ANNUAL####
#to store the data
annual.storage<-matrix(NA,10000,8)
colnames(annual.storage)<-c("slope",
                            "slope std error",
                            "intercept",
                            "intercept std error",
                            "R2",
                            "R2sd",
                            "shapiro",
                            "p_value")


for(i in 1:nrow(annual.storage)){
  #subsample the data to minimize spatial autocorrelation
  sub<-DATA %>% group_by(Code) %>% sample_n(size=1)
  #run model
  model_caret <- train(log(AAM) ~ AnnualTemp, 
                       data = sub,                        
                       trControl = data_ctrl,
                       method = "lm")
  #store resultss
  annual.storage[i,"slope"]<-as.numeric(model_caret$finalModel$coefficients[2])
  annual.storage[i,"intercept"]<-as.numeric(model_caret$finalModel$coefficients[1])
  annual.storage[i,"R2"]<-summary(model_caret$finalModel)$adj.r.squared
  annual.storage[i,"R2sd"]<-as.numeric(model_caret$results$RsquaredSD)
  annual.storage[i,"shapiro"]<-as.numeric(shapiro.test(model_caret$finalModel$residuals)[2])
  annual.storage[i,"intercept std error"]<-summary(model_caret$finalModel)$coef[3]
  annual.storage[i,"slope std error"]<-summary(model_caret$finalModel)$coef[4]
  annual.storage[i,"p_value"]<-summary(model_caret$finalModel)$coef[8]
}

write.csv(annual.storage,file="annual K folds.csv",row.names = F)

#WARM####
warm.storage<-matrix(NA,10000,8)
colnames(warm.storage)<-c("slope",
                          "slope std error",
                          "intercept",
                          "intercept std error",
                          "R2",
                          "R2sd",
                          "shapiro",
                          "p_value")


for(i in 1:nrow(warm.storage)){
  sub<-DATA %>% group_by(Code) %>% sample_n(size=1)
  model_caret <- train(log(AAM) ~ WarmTemp, 
                       data = sub,                        
                       trControl = data_ctrl,
                       method = "lm")
  warm.storage[i,"slope"]<-as.numeric(model_caret$finalModel$coefficients[2])
  warm.storage[i,"intercept"]<-as.numeric(model_caret$finalModel$coefficients[1])
  warm.storage[i,"R2"]<-summary(model_caret$finalModel)$adj.r.squared
  warm.storage[i,"R2sd"]<-as.numeric(model_caret$results$RsquaredSD)
  warm.storage[i,"intercept std error"]<-summary(model_caret$finalModel)$coef[3]
  warm.storage[i,"slope std error"]<-summary(model_caret$finalModel)$coef[4]
   warm.storage[i,"shapiro"]<-as.numeric(shapiro.test(model_caret$finalModel$residuals)[2])
   warm.storage[i,"p_value"]<-summary(model_caret$finalModel)$coef[8]
}

write.csv(warm.storage,file="warm K folds.csv",row.names = F)

#COLD####
cold.storage<-matrix(NA,10000,8)
colnames(cold.storage)<-c("slope",
                          "slope std error",
                          "intercept",
                          "intercept std error",
                          "R2",
                          "R2sd",
                          "shapiro",
                          "p_value")


for(i in 1:nrow(cold.storage)){
  sub<-DATA %>% group_by(Code) %>% sample_n(size=1)
  model_caret <- train(log(AAM) ~ ColdTemp, 
                       data = sub,                        
                       trControl = data_ctrl,
                       method = "lm")
  cold.storage[i,"slope"]<-as.numeric(model_caret$finalModel$coefficients[2])
  cold.storage[i,"intercept"]<-as.numeric(model_caret$finalModel$coefficients[1])
  cold.storage[i,"R2"]<-summary(model_caret$finalModel)$adj.r.squared
  cold.storage[i,"R2sd"]<-as.numeric(model_caret$results$RsquaredSD)
  cold.storage[i,"intercept std error"]<-summary(model_caret$finalModel)$coef[3]
  cold.storage[i,"slope std error"]<-summary(model_caret$finalModel)$coef[4]
  cold.storage[i,"shapiro"]<-as.numeric(shapiro.test(model_caret$finalModel$residuals)[2])
  cold.storage[i,"p_value"]<-summary(model_caret$finalModel)$coef[8]
}

write.csv(cold.storage,file="cold K folds.csv",row.names = F)

#DD####
dd.storage<-matrix(NA,10000,8)
colnames(dd.storage)<-c("slope",
                        "slope std error",
                        "intercept",
                        "intercept std error",
                        "R2",
                        "R2sd",
                        "shapiro",
                        "p_value")


for(i in 1:nrow(dd.storage)){
  sub<-DATA %>% group_by(Code) %>% sample_n(size=1)
  model_caret <- train(log(AAM) ~ AnnualDD, 
                       data = sub,                        
                       trControl = data_ctrl,
                       method = "lm")
  dd.storage[i,"slope"]<-as.numeric(model_caret$finalModel$coefficients[2])
  dd.storage[i,"intercept"]<-as.numeric(model_caret$finalModel$coefficients[1])
  dd.storage[i,"R2"]<-summary(model_caret$finalModel)$adj.r.squared
  dd.storage[i,"R2sd"]<-as.numeric(model_caret$results$RsquaredSD)
  dd.storage[i,"intercept std error"]<-summary(model_caret$finalModel)$coef[3]
  dd.storage[i,"slope std error"]<-summary(model_caret$finalModel)$coef[4]
  dd.storage[i,"shapiro"]<-as.numeric(shapiro.test(model_caret$finalModel$residuals)[2])
  dd.storage[i,"p_value"]<-summary(model_caret$finalModel)$coef[8]
}


write.csv(dd.storage,file="DD K folds.csv",row.names = F)




#investigating data####
hist(annual.storage[,"slope"])
hist(annual.storage[,"slope std error"])
hist(annual.storage[,"intercept"])
hist(annual.storage[,"R2"])
hist(annual.storage[,"R2sd"])
hist(annual.storage[,"shapiro"])
sum(warm.storage[,"shapiro"]<0.05)
sum(annual.storage[,"p_value"]>0.05)

hist(warm.storage[,"slope"])
hist(warm.storage[,"intercept"])
hist(warm.storage[,"R2"])
hist(warm.storage[,"R2sd"])
hist(warm.storage[,"shapiro"])
sum(warm.storage[,"shapiro"]<0.05)
sum(warm.storage[,"p_value"]>0.05)


hist(cold.storage[,"slope"])
hist(cold.storage[,"intercept"])
hist(cold.storage[,"R2"])
hist(cold.storage[,"R2sd"])
hist(cold.storage[,"shapiro"])
sum(cold.storage[,"shapiro"]<0.05)
sum(cold.storage[,"p_value"]>0.05)

hist(dd.storage[,"slope"])
hist(dd.storage[,"intercept"])
hist(dd.storage[,"R2"])
hist(dd.storage[,"R2sd"])
hist(dd.storage[,"shapiro"])
sum(dd.storage[,"shapiro"]<0.05)
sum(dd.storage[,"p_value"]>0.05)




mean(annual.storage[,"slope"])
mean(annual.storage[,"intercept"])
mean(annual.storage[,"R2"])
mean(annual.storage[,"R2sd"])
hist(annual.storage[,"shapiro"])


mean(warm.storage[,"slope"])
mean(warm.storage[,"intercept"])
mean(warm.storage[,"R2"])
mean(warm.storage[,"R2sd"])
hist(warm.storage[,"shapiro"])
(sum(warm.storage[,"shapiro"]<0.05)/nrow(warm.storage))*100



mean(cold.storage[,"slope"])
mean(cold.storage[,"intercept"])
mean(cold.storage[,"R2"])
mean(cold.storage[,"R2sd"])
hist(cold.storage[,"shapiro"])
sum(cold.storage[,"shapiro"]<0.05)


mean(dd.storage[,"slope"])
mean(dd.storage[,"intercept"])
mean(dd.storage[,"R2"])
mean(dd.storage[,"R2sd"])
hist(dd.storage[,"shapiro"])
sum(dd.storage[,"shapiro"]<0.05)/100



#residuals % that were normal
(sum(annual.storage[,"shapiro"]>0.05)/nrow(annual.storage))*100
(sum(warm.storage[,"shapiro"]>0.05)/nrow(warm.storage))*100
(sum(cold.storage[,"shapiro"]>0.05)/nrow(cold.storage))*100
(sum(dd.storage[,"shapiro"]>0.05)/nrow(dd.storage))*100




