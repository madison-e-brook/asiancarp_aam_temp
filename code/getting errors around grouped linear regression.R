
#final complete data
aamloc<-read.csv(file.choose())
str(aamloc)

DATA<-aamloc[aamloc$Species!="Black",]
str(DATA)

#get slopes and intercepts
#download K fold results
#annual K folds.csv
annual<-read.csv(file.choose())

#DD K folds.csv
dd<-read.csv(file.choose())

ann.slope<-mean(annual[,"slope"])
ann.int<-mean(annual[,"intercept"])


dd.slope<-mean(dd[,"slope"])
dd.int<-mean(dd[,"intercept"])

#annual#####
#observed - predicted
error.all<-NULL
for(i in 1:nrow(DATA)){
  expected<-ann.int+(ann.slope*DATA$AnnualTemp[i])
  error<-log(DATA$AAM[i])-expected
  error.all<-c(error.all,error)
}
plot(error.all)
mean(error.all)
median(error.all)
hist(error.all,30)

plot(DATA$AnnualTemp,error.all)

ann<-cbind(DATA,error.all)

#just North america
ann[ann$Longitude<0,]

#degree days####

error.dd<-NULL
for(i in 1:nrow(DATA)){
  expected<-dd.int+(dd.slope*DATA$AnnualDD[i])
  error<-log(DATA$AAM[i])-expected
  error.dd<-c(error.dd,error)
}
plot(error.dd)
mean(error.dd)
median(error.dd)
hist(error.dd)

plot(DATA$AnnualDD,error.dd)

dd<-cbind(DATA,error.dd)


dd[dd$Longitude<0,]



#warm####
#import the k folds
warm<-read.csv(file.choose())

w.slope<-mean(warm[,"slope"])
w.int<-mean(warm[,"intercept"])

error.w<-NULL
for(i in 1:nrow(DATA)){
  expected<-w.int+(w.slope*DATA$WarmTemp[i])
  error<-log(DATA$AAM[i])-expected
  error.w<-c(error.w,error)
}
plot(error.w)
mean(error.w)
median(error.w)
hist(error.w)



#cold####
#import the k fold results
cold<-read.csv(file.choose())

c.slope<-mean(cold[,"slope"])
c.int<-mean(cold[,"intercept"])

error.c<-NULL
for(i in 1:nrow(DATA)){
  expected<-c.int+(c.slope*DATA$ColdTemp[i])
  error<-log(DATA$AAM[i])-expected
  error.c<-c(error.c,error)
}

mean(error.c)
median(error.c)
hist(error.c)


#combine all error results
cc<-cbind(DATA,error.all,error.dd,error.w,error.c)

#which locations had the largest errors
which.max(cc$error.c)
max(cc$error.c)
which.min(cc$error.c)
min(cc$error.c)
cc[45,]
cc[43,]


which.max(cc$error.w)
max(cc$error.w)
which.min(cc$error.w)
min(cc$error.w)
cc[3,]
cc[47,]

which.max(cc$error.all)
max(cc$error.all)
which.min(cc$error.all)
min(cc$error.all)
cc[3,]
cc[43,]

which.max(cc$error.dd)
max(cc$error.dd)
which.min(cc$error.dd)
min(cc$error.dd)
cc[4,]
cc[43,]

#looking at north america errors####
North<-cc[cc$Longitude<0,]
North
North[-4,]
mean(North[-4,'error.all'])
North[4,'error.all']
mean(North[-4,'error.dd'])
North[4,'error.dd']


plot(North[,'error.all'])
plot(North[,'error.dd'])

