# R Review Session

#-------------------------------ASSIGNMENT--------------------------
xa <- 4
xb=5

#-----------------------------WORKING DIRECTORY------------------
getwd()
setwd("~/Desktop/SYS3501/Data/")

#------------------------READING DATA-----------------------------

weather_data<-read.csv("weather.csv")
#print first 6 lines
head(weather_data)

#---------------------------DATATYPES-----------------------------

#double
c<- 10
typeof(c)

#Logical
log_var<- TRUE
typeof(log_var)

#Vector
b_vector<- c(2,3,4,5,6)
b_vector

#--------------------------BINDING VECTORS-----------------------
vara <- c(0,1,2,3,4,5)
varb <- c(4,5,6,7,8,9)

result<- cbind(vara, varb)
result

#factoring

factor(c(1,2,3))

factor(c("cat","dog","cat","dog"))

#---------------------------INDEXING-----------------------------
varb[2]
varb[3:7]
varb[2:4]
varb[varb>1]

#--------------------------CONDITIONALS---------------------------

va<-3
vb<-5
va>vb

if(va>vb)
  print('va is greater than vb')

#--------------------------FOR LOOPS-------------------------

for (x in 1:10){
  print("Monday is bad")
}

#-------------------------FUNCTIONS------------------------------
add_num <- function(a,b){
  return(a+b)
}
  

       