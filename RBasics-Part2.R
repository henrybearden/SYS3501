#********************************************************************************
#             
#					R Basics - Part 2
#
#********************************************************************************

#***************************
# 0.1 installing and loading the library for the visualization
#***************************
sessionInfo() #information about the version of your R and packages that are loaded in this session

#***************************
# 0.2 installing and loading the library for this session
#***************************
# First install packages (only needs to be done once), and next load the packages
#install.packages('ggplot2')
library(ggplot2)
#install.packages('dplyr')
library(dplyr)

#**********************************************************
# 1. Reading in data
#**********************************************************

#***************************
# 1.1 Set working directory
#***************************

# For example, an Mac user
traindir <- "~/Desktop/SYS3501/Data/TrainData/" 
#~/Google Drive/My Drive/UVA/Courses/ProgSIE/Data/TrainData/
sourcedir <- "~/Desktop/SYS3501/Source/"

# or a Windows user
traindir <- "J:/SYS4021/InClassR/Data/Train/"
sourcedir <-"J:/SYS4021/InClassR/Source/"

# set the working directory to traindir
setwd("~/Desktop/SYSProg/Data/TrainData/")
#check the current working directory
#it should be same as your traindir directory
getwd()

# list the files
dir()


#***************************
# 1.2 loading the data
#***************************

# Read in the accident files one at at time
# for the first questions in the in-class assignment we will 
# only use data from 2022

accident_data_22 <- read.csv("RailAccidents22.csv")

#***************************
# 1.3 information about the selected year accident data
#***************************

head(accident_data_22) # the first 6 rows (observations) of data
dim(accident_data_22) #number of rows (observation) and number of columns (variables)

summary(accident_data_22) #summary of each column (variable)
str(accident_data_22) #type of each column (variable) of data

# To get a summary of a subset of the variables (e.g., "ACCDMG", "TOTKLD", "CARS" ) 
str(accident_data_22[,c("ACCDMG", "TOTKLD", "CARS", "STATION")])

colnames(accident_data_22) # number of columns (variable)
class(accident_data_22$TOTKLD) #type of one specific variable 
var(accident_data_22$TOTKLD) #variance of one specific variable (total killed) - quantitative
mean(accident_data_22$TOTKLD) #mean of one specific variable (total killed) - quantitative
table(accident_data_22$TOTKLD) #frequency of each value in the selected variable

class(accident_data_22$STATION) #type of one specific variable 
factor(accident_data_22$STATION) #different values of categorical variable (station) - qualitative
table(accident_data_22$STATION) #frequency of each value in the selected variable

# You can round your answer using the round() function
print(mean(accident_data_22$TOTKLD))
print(round(mean(accident_data_22$TOTKLD)))

#What is the total accident damage from all accidents in 2022?
sum(accident_data_22$ACCDMG)

#What is the total accident damage from 2022 for the 5 most expensive accidents
x <- accident_data_22$ACCDMG

xsort <- sort(x, decreasing = TRUE)

sumacc5 <- sum(xsort[1:5])
sumacc5

#Now let's create a new data frame with accidents that cost more than $1e6
bigacc <- accident_data_22[(accident_data_22$ACCDMG) > 1e6, ]


#print the narrative from the most expensive accident
summary(accident_data_22)


