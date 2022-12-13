library(dplyr)

#import data: final complete data.csv
aamloc<-read.csv(file.choose())
str(aamloc)

#separate by species#####
Black<-aamloc[aamloc$Species=="Black",]

min(Black$AAM)
max(Black$AAM)

Grass<-aamloc[aamloc$Species=="Grass",]

min(Grass$AAM)
max(Grass$AAM)

Bighead<-aamloc[aamloc$Species=="Bighead",]

min(Bighead$AAM)
max(Bighead$AAM)


Silver<-aamloc[aamloc$Species=="Silver",]

min(Silver$AAM)
max(Silver$AAM)


#testing errors of individual species
DATA<-aamloc[aamloc$Species!="Black",]
str(DATA)

#retrieve K fold results
annual<-read.csv(file.choose())

#DD K folds.csv
dd<-read.csv(file.choose())

cold<-read.csv(file.choose())

c.slope<-mean(cold[,"slope"])
c.int<-mean(cold[,"intercept"])


ann.slope<-mean(annual[,"slope"])
ann.int<-mean(annual[,"intercept"])


dd.slope<-mean(dd[,"slope"])
dd.int<-mean(dd[,"intercept"])



#silver#####

#error for silver with annual temp
silver.a<-NULL
for(i in 1:nrow(Silver)){
  expected<-ann.int+(ann.slope*Silver$AnnualTemp[i])
  error<-log(Silver$AAM[i])-expected
  silver.a<-c(silver.a,error)
}
silver.a
mean(silver.a)
hist(silver.a)
#error for silver with degree days
silver.dd<-NULL
for(i in 1:nrow(Silver)){
  expected<-dd.int+(dd.slope*Silver$AnnualDD[i])
  error<-log(Silver$AAM[i])-expected
  silver.dd<-c(silver.dd,error)
}
silver.dd
mean(silver.dd)
hist(silver.dd)
#error for silver with cold temp
silver.cold<-NULL
for(i in 1:nrow(Silver)){
  expected<-c.int+(c.slope*Silver$ColdTemp[i])
  error<-log(Silver$AAM[i])-expected
  silver.cold<-c(silver.cold,error)
}
silver.cold
mean(silver.cold)
hist(silver.cold)

#Grass####
#error for grass with annual temp
grass.a<-NULL
for(i in 1:nrow(Grass)){
  expected<-ann.int+(ann.slope*Grass$AnnualTemp[i])
  error<-log(Grass$AAM[i])-expected
  grass.a<-c(grass.a,error)
}
grass.a
mean(grass.a)
hist(grass.a)
#error for grass with degree days
grass.dd<-NULL
for(i in 1:nrow(Grass)){
  expected<-dd.int+(dd.slope*Grass$AnnualDD[i])
  error<-log(Grass$AAM[i])-expected
  grass.dd<-c(grass.dd,error)
}
grass.dd
mean(grass.dd)
hist(grass.dd)
#error for grass with cold temp
grass.cold<-NULL
for(i in 1:nrow(Grass)){
  expected<-c.int+(c.slope*Grass$ColdTemp[i])
  error<-log(Grass$AAM[i])-expected
  grass.cold<-c(grass.cold,error)
}
grass.cold
mean(grass.cold)
hist(grass.cold)


#Bighead####
#error for bighead with annual temp
bighead.a<-NULL
for(i in 1:nrow(Bighead)){
  expected<-ann.int+(ann.slope*Bighead$AnnualTemp[i])
  error<-log(Bighead$AAM[i])-expected
  bighead.a<-c(bighead.a,error)
}
bighead.a
mean(bighead.a)
hist(bighead.a)
#error for bighead with degree days
bighead.dd<-NULL
for(i in 1:nrow(Bighead)){
  expected<-dd.int+(dd.slope*Bighead$AnnualDD[i])
  error<-log(Bighead$AAM[i])-expected
  bighead.dd<-c(bighead.dd,error)
}
bighead.dd
mean(bighead.dd)
hist(bighead.dd)
#error for bighead with cold temp
bighead.cold<-NULL
for(i in 1:nrow(Bighead)){
  expected<-c.int+(c.slope*Bighead$ColdTemp[i])
  error<-log(Bighead$AAM[i])-expected
  bighead.cold<-c(bighead.cold,error)
}
bighead.cold
mean(bighead.cold)
hist(bighead.cold)


