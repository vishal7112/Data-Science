library("ggplot2")
library(readxl)
library("gdata")

link <- "http://college.cengage.com/mathematics/brase/understandable_statistics/7e/students/datasets/mlr/excel/mlr01.xls"

#download the data into an excell file
download.file(link,destfile = "./file.xls", mode = 'wb')

#view the data
datafile <- read_xls("file.xls")
str(datafile)

#columns renaming
columns <- c("Baby_Antelope", "Adult_Antelope_Population","Annual_Precipitation","Winter_Weather")
colnames(datafile) <- columns


#bivariate  plots
Graph1 <- ggplot(datafile, aes(x=Adult_Antelope_Population, y=Baby_Antelope)) + geom_point()  + stat_smooth(method = "lm", col ="red") 
Graph1
Graph2 <- ggplot(datafile, aes(x=Annual_Precipitation, y=Baby_Antelope)) + geom_point() + stat_smooth(method = "lm", col = "red")
Graph2
Graph3 <- ggplot(datafile, aes(x=Winter_Weather, y=Baby_Antelope)) +  geom_point() + stat_smooth(method = "lm", col = "red")
Graph3

#Regression model 1
#using winter condition to predict number of fawns
M1_data <- datafile[,c(1,4)]
M1 <- lm(Baby_Antelope ~ ., data = M1_data) 
summary(M1)

#model 2
#using winter condition and adult population to predict number of fawns
M2_data  <- datafile[,-3]

M2 <- lm(Baby_Antelope ~ ., data = M2_data)
summary(M2)


#model 3
#using all three variables
M3 <- lm(Baby_Antelope ~ ., data = datafile)
summary(M3)

#Which model works best? 
#Ans: Model 3 because it had the strongest P value. 
  
#Which of the predictors are statistically significant in each model? 
#Ans:In model 1 Adult population is the significant predictor. model 2 and 3 all the predictors are significant with about the same P values. 

#If you wanted to create the most parsimonious model (i.e., the one that did the best job with the fewest predictors), what would it contain?
#Something similar to model 2 because it was a good middle ground between P level and use of predictors.