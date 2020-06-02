#Step 1 
library(sqldf)

#Step 2
library(jsonlite)

#Step 3

# Reveals the first few records of airquality data
head(airquality)

#step 4

##Getting the average ozone datafrom the ozone column
sqldf("select avg(Ozone) from airquality") 

#Attaching the average ozone to a new variable
AvgOzone<-sqldf("select avg(Ozone) from airquality") 
AvgOzone

#step 5

#get all the data of  ozone that is higher than the average and attach it to a new var.
newAQ<-sqldf("select * from airquality where Ozone>(select avg(Ozone) from airquality) ")
newAQ 

#step 6

#Display the structure of newAQ
str(newAQ)

#Dimension (Rows, Columns)
dim(newAQ)

#Will show the first few rows
head(newAQ)

#Steo 7 this is the futhest I could get with tapply
NewAvg <-tapply(airquality$Ozone, airquality$Month, mean, na.rm= T)
mean(NewAvg)

#another way to do step 7 which I found simpler compared to tapply
library("dplyr")
newMean <- mean(airquality$Ozone, na.rm = T)
newMean
NewAQ2 = filter(airquality, airquality$Ozone > newMean)
NewAQ2
