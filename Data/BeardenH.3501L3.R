#Henry Bearden zuf3ys LAB 3
setwd("~/Desktop/SYS3501/Data")
library("ggplot2")
library("dplyr")
library("tidyverse")
homes<-read.csv("housing.csv")

#20
prices <- na.omit(homes$price)
ggplot(homes, aes(x=prices)) + geom_boxplot() + labs(title="pricebp", x="price")+
 theme_minimal()

#21
boxplot.stats(prices)$stats[5]

#22-24
pairs.panels(homes[, c("price", "sqft", "baths","bedrooms")])

#25
bdrms <- na.omit(homes$bedrooms)
ggplot(homes, aes(x=bdrms)) + geom_boxplot()+labs(title="bedroomsbp", x="bedrooms")+
  theme_minimal()

boxplot.stats(bdrms)$stats[1]

#26
homes %>% filter(price == max(price))

which(homes$price == max(homes$price))

#27
homes %>% filter(City == "Oxnard") %>% summarise(total_sum = sum(price, na.rm = TRUE))
#28
cityprice<- homes %>% group_by(City) %>% summarise(total_sum_price = sum(price, na.rm = TRUE))

cityprice %>% filter(total_sum_price == max(total_sum_price))
#29
homes %>% summarise(num_levels = n_distinct(City))
