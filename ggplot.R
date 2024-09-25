#https://jumpingrivers.github.io/nhs-ggplot2/slides.html#1
#Follows the grammar of graphics.  This is only 1 of many ways 
#to make graphics in R

library("ggplot2")
data("mpg")
#Creates an empty canvas
ggplot()

#This function has two arguments:
#data: this must be a data frame (or tibble)
#an aesthetic mapping: this tells {ggplot2} how to map data to 
#the graphical elements

summary(mpg)

#The function aes() maps our data to the graph
g = ggplot(data = mpg,
           mapping = aes(x = cty, y = hwy))

g

#To add data, we need a geom
h = ggplot(data = mpg,
           mapping = aes(x = cty, y = hwy)) 

h + geom_point()

#bar plot
ggplot(mpg, aes(x = manufacturer)) + 
  geom_bar()

#histogram
ggplot(mpg, aes(x = hwy)) + geom_histogram(binwidth = 5)

#box plots
boxplot = ggplot(mpg, aes(x = as.factor(cyl), y = hwy)) + 
  geom_boxplot()

boxplot

boxplot + labs(x = "# of Cylinders", 
               y = "Hwy MPG", 
               title = "MPG Heading 1", 
               subtitle = "MPG Heading 2", 
               caption = "These box plots represent stuff")

#Aesthetics
#There are many different types of aesthetic attributes 
#you can modify

#Colour/fill
#Shape
#Size
#Linetype 
#Alpha

#If you are using the columns of the data to determine style, 
#use inside of aes
ggplot(mpg, aes(x = cty, y = hwy)) + 
  geom_point(aes(colour = manufacturer)) 

#If you are hard-coding style, no aes needed
ggplot(mpg, aes(x = cty, y = hwy)) + 
  geom_point(colour = "blue")

#Change the shape based on data
ggplot(mpg, aes(x = cty, y = hwy)) + 
  geom_point(aes(shape = as.factor(cyl)), size = 3) 

#Change the shape for all data
ggplot(mpg, aes(x = cty, y = hwy)) + 
  geom_point(colour = "blue", shape = 9) 
  

 