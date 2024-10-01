# Henry Bearden zuf3ys

library(ggplot2)
library(dplyr)
library(psych)
library(lattice)
library(gplots)

traindir <- "~/Desktop/SYS3501/Data/TrainData/"
sourcedir <-"~/Desktop/SYS3501/Code"

setwd(traindir)

setwd(sourcedir)
source("FileInput.R")

acts <- file.inputl(traindir)

totacts <- combine.data(acts)

dim(totacts)

totacts$Cause <- rep(NA, nrow(totacts))

totacts$Cause <- factor(totacts$Cause)

head(totacts)

#Q10
summary(totacts$"HIGHSPD")

sum(totacts$HIGHSPD > 30, na.rm = TRUE)

sum(totacts$HIGHSPD > 100, na.rm = TRUE)

#Q11

ggplot(totacts, aes(x = as.factor(IYR), y = EQPDMG)) +
  geom_boxplot() +
  labs(title = "EQPDMG by Year",
       x = "Year (IYR)",
       y = "Equipment Damage (EQPDMG)") +
  theme_minimal()

#Q12
ggplot(totacts, aes(x = as.factor(IYR), y = TOTKLD)) +
  geom_boxplot() +
  labs(title = "TOTKLD by Year",
       x = "Year (IYR)",
       y = " (TOTLKD)") +
  theme_minimal()

#Q13
ggplot(totacts, aes(x = as.factor(IYR), y = TRKDMG)) +
  geom_boxplot() +
  labs(title = "TRKDMG by Year",
       x = "Year (IYR)",
       y = "(TRKDMG)") +
  theme_minimal()

#Q14
ggplot(totacts, aes(x = as.factor(IYR), y = TOTINJ)) +
  geom_boxplot() +
  labs(title = "TOTINJ by Year",
       x = "Year (IYR)",
       y = " (TOTINJ)") +
  theme_minimal()

#Q15
totacts$INCDTNO[which.max(totacts$TOTINJ)]

#Q16
ggplot(totacts, aes(x = as.factor(IYR), y = CARSDMG)) +
  geom_boxplot() +
  labs(title = "CARSDMG by Year",
       x = "Year (IYR)",
       y = "(CARSDMG)") +
  theme_minimal()

#Q17
ggplot(totacts, aes(x = TEMP)) +
  geom_histogram(breaks = hist(totacts$TEMP, plot = FALSE, breaks = "Sturges")$breaks,
                 fill = NA, color = "steelblue") +
  labs(title = "Histogram for TEMP",
       x = "(TEMP)",
       y = "Count") +
  theme_minimal()

#Q18-20
matrixdata <- totacts[, c("TRKDMG", "EQPDMG", "ACCDMG", "TOTINJ", "TOTKLD")]

pairs.panels(matrixdata,
             method = "pearson",
             hist.col = NULL,  
             density = FALSE, 
             ellipses = FALSE)

#Q21
totacts$IYR[which.max(totacts$ACCDMG)]
#Q22
totacts$INCDTNO[which.max(totacts$ACCDMG)]==totacts$INCDTNO[which.max(totacts$TOTKLD)]
totacts$TYPE[which.max(totacts$ACCDMG)]
totacts$INCDTNO2[which.max(totacts$ACCDMG)]
#Q23
max(totacts$ACCDMG)
#Q24
totacts$IYR[which.max(totacts$TOTKLD)]
#Q25
max(totacts$TOTKLD)
#Q26
sum(totacts$ACCDMG > 1500000, na.rm = TRUE)
#Q27
sum(totacts$TOTKLD >= 1, na.rm = TRUE)
#Q28
ggplot(totacts, aes(x = as.factor(TYPE))) +
  geom_bar() +
  labs(title = "Frequency by Accident Type",
       x = "(TYPE)",
       y = "Frequency") +
  theme_minimal()
#Q29
totacts$Cause <- rep(NA, nrow(totacts))

totacts$Cause[which(substr(totacts$CAUSE, 1, 1) == "M")] <- "M"
totacts$Cause[which(substr(totacts$CAUSE, 1, 1) == "T")] <- "T"
totacts$Cause[which(substr(totacts$CAUSE, 1, 1) == "S")] <- "S"
totacts$Cause[which(substr(totacts$CAUSE, 1, 1) == "H")] <- "H"
totacts$Cause[which(substr(totacts$CAUSE, 1, 1) == "E")] <- "E"

totacts$Cause <- factor(totacts$Cause)

ggplot(totacts, aes(x = totacts$Cause)) +
  geom_bar() +
  labs(title = "Frequency of Each Accident Cause",
       x = "Cause",
       y = "Frequency") +
  theme_minimal()
#Q30
ggplot(totacts, aes(y = log(ACCDMG))) +
  geom_boxplot() +
  labs(title = "Box Plot of Log-Transformed ACCDMG",
       y = "Log(Accident Damage (ACCDMG))") +
  theme_minimal()

IQR <- quantile(totacts$ACCDMG, 0.75, na.rm = TRUE) - quantile(totacts$ACCDMG, 0.25, na.rm = TRUE)
quantile(totacts$ACCDMG, 0.75, na.rm = TRUE) + 1.5 * IQR

#Q31
sum(totacts$ACCDMG > 165804, na.rm = TRUE)

#Q32
sum(totacts$ACCDMG > 165804, na.rm = TRUE)/nrow(totacts)

#Q33
sum(totacts$ACCDMG[totacts$ACCDMG > 165804], na.rm = TRUE)/(sum(totacts$ACCDMG, na.rm = TRUE))

#Q34
sum(totacts$ACCDMG > 15000000, na.rm = TRUE)

dates <- paste(totacts$YEAR[totacts$ACCDMG > 15000000], 
                           totacts$MONTH[totacts$ACCDMG > 15000000], 
                           totacts$DAY[totacts$ACCDMG > 15000000], 
                           totacts$TIMEHR[totacts$ACCDMG > 15000000],
                           sep = "-")
print(dates)

types <- totacts$TYPE[totacts$ACCDMG > 15000000]
print(types)

max(totacts$ACCDMG[totacts$ACCDMG > 15000000], na.rm = TRUE)

#Q35
sum(duplicated((totacts[totacts$ACCDMG > 165804,])[, c("INCDTNO", "YEAR", "MONTH", "DAY", "TIMEHR", "TIMEMIN")]))
