
#import data: final complete data.csv
aamloc<-read.csv(file.choose())

variables<-data.frame(
  aamloc$AnnualTemp,
  aamloc$WarmTemp,
  aamloc$ColdTemp,
  aamloc$AnnualDD
)

str(variables)

tab<-round(cor(variables),2)
tab[upper.tri(tab)]<-""
tab



