#Henry Bearden zuf3ys EXAM 1
setwd("~/Desktop/SYS3501/Data")
library("ggplot2")
library("dplyr")
library("tidyverse")
library(VIM)
data("airquality")
airquality
hprice<-read.csv("housing-prices.csv")
hprice

#2
which.min(hprice$Price)
#3
hprice$Size[(which.max(hprice$Price))]
#4
ggplot(hprice, aes(x=Size)) + geom_boxplot() + labs(title="Sizebp", x="Size")+
  theme_minimal()

max(hprice$Size)
#5-7
sizestats<-boxplot.stats(hprice$Size)
upwhisk<-sizestats$stats[5]
sizeout<-data.frame(Size=hprice$Size[hprice$Size>upwhisk])
sizeout
max(sizeout)
mean(sizeout[,1])

soful<-hprice[hprice$Size>upwhisk,]
soful
mean(soful[,5])
#8
ggplot(hprice, aes(x=Baths)) + geom_boxplot() + labs(title="Bathbp", x="Bath")+
  theme_minimal()
bathstat<- boxplot.stats(hprice$Baths)
bathstat$stats[5]
#9-11
pairs.panels(hprice[, c("Size", "Rooms", "Baths","Price")])
#12
hprice %>% filter(Age == "Old") %>% summarise(total_sum = sum(Price, na.rm = TRUE))
#13
hprice$Baths<-as.factor(hprice$Baths)
hprice$Rooms<-as.factor(hprice$Rooms)
table(hprice$Baths, hprice$Rooms)
heatmap(table(hprice$Baths, hprice$Rooms), Rowv = NA, Colv = NA)

#14
summary(airquality)
#15
mtbm<-airquality%>%group_by(Month) %>% summarise(mtbm=mean(Temp, na.rm=TRUE))
mtbm
#16-18
hist(airquality$Solar.R)

#19-21
pairs.panels(airquality[, c("Solar.R", "Ozone", "Wind","Temp")])

#22-25
ggplot(airquality, aes(x=as.factor(Month), y=Wind)) + geom_boxplot() + labs(title="windmonth", x="month")+
  theme_minimal()

airquality%>%group_by(Month) %>% summarise(IQR=IQR(Wind, na.rm=TRUE))


