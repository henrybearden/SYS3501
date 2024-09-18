#Henry Bearden zuf3ys SYS 3501 Lab 1

setwd("~/Desktop/SYS3501/Data/TrainData/")

Accidents2020<-read.csv("RailAccidents20.csv")
nrow(Accidents2020)
ncol(Accidents2020)

which.max(Accidents2020$CASINJ)
Accidents2020$INCDTNO[which.max(Accidents2020$CASINJ)]
which.max(Accidents2020$ACCDMG)
Accidents2020[which.max(Accidents2020$ACCDMG),]
max(Accidents2020$EQPDMG)
max(Accidents2020$TRNSPD)
max(Accidents2020$CASKLD)
max(Accidents2020$CASINJ)
max=(Accidents2020$ACCDMG)
median(Accidents2020$CASINJ)
sum(Accidents2020$ACCDMG[Accidents2020$TOTKLD >= 1 | Accidents2020$TOTINJ >= 1])
