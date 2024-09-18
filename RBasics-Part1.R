#********************************************************************************
#             
#					R Basics - Part 1
#
#********************************************************************************

# SYS 3501-001: Intro to R
# Some examples from: from from R Programming Tutorial - 
# Learn the Basics of Statistical Computing:  
# https://www.youtube.com/watch?v=_V8eKsto3Ug
# https://www.w3schools.com/r/default.asp
# https://r4ds.had.co.nz/index.html


# Installs pacman ("package manager") if needed
install.packages("pacman")

library(pacman)

# PACKAGES ############################################
install.packages("psych")
library(psych)

?p_help

# Get info on package
p_help(psych, web = F)  # Opens help in R Viewer


# READING IN DATA FROM A FILE #################################################

# GET / SET WORKING DIRECTORY ###########################################

getwd()

#Navigate to directory where your data is stored

#Mac
setwd("~/Google Drive/My Drive/UVA/Courses/ProgSIE/Data/")

#Windows
setwd("C:/Me/Data")

# READ IN A SINGLE DATA FILE ###########################################
iriscsv <- read.csv("iris.txt")


# LOAD DATA ################################################

library(datasets)  # Load built-in datasets
#https://stat.ethz.ch/R-manual/R-devel/library/datasets/html/00Index.html

# SUMMARIZE DATA ###########################################

head(iris)         # Show the first six lines of iris data
summary(iris)      # Summary statistics for iris data
plot(iris)         # Scatterplot matrix for iris data


# DESCRIBE() ###############################################

# For quantitative variables only.

describe(iris)               # Entire data frame
describe(iris$Sepal.Length)  # One quantitative variable


# CLEAN UP #################################################

# Clear environment
rm(list = ls()) 

# Clear packages
p_unload(all)  # Remove all add-ons
detach("package:datasets", unload = TRUE)   # For base

# Clear console
cat("\014")  # ctrl+L

# DATA TYPES ###############################################

# Numeric

n1 <- 15  # Double precision by default
n1
typeof(n1)

n2 <- 1.5
n2
typeof(n2)

# Character

c1 <- "c"
c1
typeof(c1)

c2 <- "a string of text"
c2
typeof(c2)

# Logical

l1 <- TRUE
l1
typeof(l1)

l2 <- F
l2
typeof(l2)

# DATA STRUCTURES ##########################################

## Vector ##################################################
# 1+ numbers in a 1d array, all same data type

v1 <- c(1, 2, 3, 4, 5)
v1
is.vector(v1)

v2 <- c("a", "b", "c")
v2
is.vector(v2)

v3 <- c(TRUE, TRUE, FALSE, FALSE, TRUE)
v3
is.vector(v3)

## Matrix ##################################################
#2 dimensions, same length, same class,no column names
m1 <- matrix(c(T, T, F, F, T, F), nrow = 2)
m1

m2 <- matrix(c("a", "b", 
               "c", "d"), 
             nrow = 2,
             byrow = T)
m2

## Array ###################################################
#like a matrix but 3 dimensions

# Give data, then dimensions (rows, columns, tables)
a1 <- array(c(1:24), c(4, 3, 2))
a1

## Data frame ##############################################
#vectors of multiple types, all same length, closest to a spreadsheet
# Can combine vectors of the same length

vNumeric   <- c(1, 2, 3)
vCharacter <- c("a", "b", "c")
vLogical   <- c(T, F, T)

dfa <- cbind(vNumeric, vCharacter, vLogical)
dfa  # Matrix of one data type

df <- as.data.frame(cbind(vNumeric, vCharacter, vLogical))
df  # Makes a data frame with three different data types

## List ####################################################
# Lists are the R objects which contain elements of different types 
# and are most flexible, put anything in a list with any length or 
# any structure; hard to work w/ lists

o1 <- c(1, 2, 3)
o2 <- c("a", "b", "c", "d")
o3 <- c(T, F, T, T, F)

list1 <- list(o1, o2, o3)
list1

list2 <- list(o1, o2, o3, list1)  # Lists within lists!
list2

list1[o1[1]]

# COERCING TYPES ###########################################

## Automatic coercion ######################################

# Goes to "least restrictive" data type

(coerce1 <- c(1, "b", TRUE))

typeof(coerce1)

## Coerce numeric to integer ###############################

(coerce2 <- 5)
typeof(coerce2)

(coerce3 <- as.integer(5))
typeof(coerce3)

## Coerce character to numeric #############################

(coerce4 <- c("1", "2", "3"))
typeof(coerce4)

(coerce5 <- as.numeric(c("1", "2", "3")))
typeof(coerce5)

## Coerce matrix to data frame #############################

(coerce6 <- matrix(1:9, nrow= 3))
is.matrix(coerce6)

(coerce7 <- as.data.frame(matrix(1:9, nrow= 3)))
is.data.frame(coerce7)

# CLEAN UP #################################################

# Clear environment
rm(list = ls()) 

# Clear console
cat("\014")  # ctrl+L


# COLON OPERATOR ###########################################

# Assigns number 0 through 10 to x1
x1 <- 0:10
x1

# Descending order
x2 <- 10:0
x2

# SEQ ######################################################

?seq  # R help on seq

# Ascending values (duplicates 1:10)
(x3 <- seq(10))

# Specify change in values
(x4 <- seq(30, 0, by = -3))

# ENTER MULTIPLE VALUES WITH C #############################

# c = concatenate (or combine or collect)
?c  # R help on c

x5 <- c(5, 4, 1, 6, 7, 2, 2, 3, 2, 8)
x5

# SCAN #####################################################

?scan  # R help on scan

x6 <- scan()  # After running this command, go to console
# Hit return after each number
# Hit return twice to stop
x6

# REP ######################################################

?rep  # R help on rep
x7 <- rep(TRUE, 5)
x7

# Repeats set
x8 <- rep(c(TRUE, FALSE), 5)
x8

# Repeats items in set
x9 <- rep(c(TRUE, FALSE), each = 5)
x9

# CLEAN UP #################################################

# Clear environment
rm(list = ls()) 

# Clear console
cat("\014")  # ctrl+L


# Factors ##########################################


# CREATE DATA ##############################################

(x1 <- 1:3)
(y  <- 1:9)

# Combine variables
(df1 <- cbind.data.frame(x1, y))
typeof(df1$x1)
str(df1)

# AS.FACTOR ################################################

(x2  <- as.factor(c(1:3)))
(df2 <- cbind.data.frame(x2, y))
typeof(df2$x2)
str(df2)

# DEFINE EXISTING VARIABLE AS FACTOR #######################

x3  <- c(1:3)
df3 <- cbind.data.frame(x3, y)

(df3$x3 <- factor(df3$x3,
                  levels = c(1, 2, 3)))
typeof(df3$x3)
str(df3)

# LABELS FOR FACTOR ########################################

x4  <- c(1:3)
df4 <- cbind.data.frame(x4, y)
df4$x4 <- factor(df4$x4,
                 levels = c(1, 2, 3),
                 labels = c("macOS", "Windows", "Linux"))
df4
typeof(df4$x4)
str(df4)

# ORDERED FACTORS AND LABELS ###############################

x5  <- c(1:3)
df5 <- cbind.data.frame(x5, y)
(df5$x5 <- ordered(df5$x5,
                   levels = c(3, 1, 2),
                   labels = c("No", "Maybe", "Yes")))
df5
typeof(df5$x5)
str(df5)

# CLEAN UP #################################################

# Clear environment
rm(list = ls()) 

# Clear console
cat("\014")  # ctrl+L


# CONTROL STRUCTURES #################################################

#If Else Loops

x1 <- 200
x2 <- 5000

if (x2 > x1) {
  print("x2 is greater than x1")
}


if (x2 > x1) {
  print("x2 is greater than x1")
} else if (x1 == x2) {
  print ("x1 and x2 are equal")
}

if (x2 > x1) {
  print("x2 is greater than x1")
} else if (x1 == x2) {
  print("a and b are equal")
} else {
  print("x1 is greater than x2")
}

#While Loop

i <- 1
while (i < 10) {
  print(i)
  i <- i + 1
}

i <- 1
while (i < 10) {
  print(i)
  i <- i + 1
  if (i == 9) {
    break
  }
}


#For Loop

for (x in 1:10) {
  print(x)
}

letters <- list("A", "B", "C")

for (x in letters) {
  print(x)
}


#FUNCTIONS

#Create a function

hello_world <- function() { # create a function with the name hello_world
  print("Hello World!")
}

hello_world()

#Create a function with arguments

add_nums <- function(x1, x2) {
  return(x1+x2)
}

dir()
iriscsv<-read.csv("iris.txt")
dir()

x <- c(2, 2, 4, 5, 7, 10, 11, 9, 10)

print(x[5])