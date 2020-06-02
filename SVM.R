library("kernlab")
library("ggplot2")
library(e1071)
library(gridExtra)

airquality

#removs na's
airquality <- airquality
airquality[is.na(airquality)] <- 0
airquality

#create train and test dataset
sample	<- sample(1:nrow(airquality))
head(sample)
nr<- nrow(airquality)

split	<- floor(2*nr/3)
split

trainAirquality <-airquality[sample[1:split],]
testAirquality <-airquality[sample[(split+1):nr],]
trainAirquality
testAirquality

#: Build a Model using KSVM and visualize the results

#Building the model
predictOzone <- function(a, airquality){
  predictedOzone <- predict(a, airquality)
  results1 <- table(predictedOzone, airquality$Ozone)
  print(results1)
  percentCorrect1 <- (results1[1,1]+results1[2,2])/(results1[1,1]+results1[1,2]+results1[2,1]+results1[2,2])*100
  round(percentCorrect1)  
  return(percentCorrect1)
}

KSVM <- ksvm(Ozone ~ ., data = airquality)
predictOzone(KSVM, airquality)


root_square <- function(error)
{
  sqrt(mean(error^2))
}

KSVM.first <- predict(KSVM, airquality)
KSVM.error <- (airquality$Ozone - KSVM.first)
root_square(KSVM.error)

#new Air quality dataset
NewAirquality <- data.frame(airquality$Wind,airquality$Temp,KSVM.error)

#stating columns
colnames(NewAirquality) <- c("Wind","Temp","Error")

#plotting KSVM.error
plotAq <- ggplot(data = NewAirquality,aes(x=airquality$Temp,y=airquality$Wind)) + geom_point(aes(size=KSVM.error), color = "red") + ggtitle("KSVM Model")
plotAq

#compute models and plot using e1071 package

SVM <- svm(Ozone ~ ., data = airquality)
predictOzone(SVM, airquality)

SVM.first <- predict(SVM, airquality)
SVM.error <- (airquality$Ozone - SVM.first)
root_square(SVM.error)

#new data fram for SVm.error

NewAirquality1 <- data.frame(airquality$Wind,airquality$Temp,SVM.error)
colnames(NewAirquality1) <- c("Wind","Temp","Error")
plotAq1 <- ggplot(data = NewAirquality1,aes(x=airquality$Temp,y=airquality$Wind)) + geom_point(aes(size=SVM.error), color = "red") + ggtitle("SVM Model")
plotAq1

#lm Model
LM <- lm(Ozone ~., data=airquality)

LM.first <- predict(LM, airquality)
LM.error <- (airquality$Ozone - LM.first)
root_square(LM.error)

NewAirqualit2 <- data.frame(airquality$Wind,airquality$Temp,LM.error)
colnames(NewAirqualit2) <- c("Wind","Temp","Error")
plotAq2 <- ggplot(data = NewAirqualit2,aes(x=airquality$Temp,y=airquality$Wind)) + geom_point(aes(size=LM.error), color = "red") + ggtitle("LM Model")
plotAq2

#using the grid.arrange function to show all plots in one window
grid.arrange(plotAq,plotAq1, plotAq2, ncol = 1)

#Create a goodOzone variable 

goodOzone <-c()
for (i in 1:153) {
  if (airquality$Ozone[i] < mean(airquality$Ozone)){
    goodOzone<-append(goodOzone,0)
  }
  else goodOzone<-append(goodOzone,1)
}

#adding the column in the dataframe
airquality<-data.frame(airquality,goodOzone)

#See if we can do a better job predicting good and bad days


predictGoodozone <- function(m, airquality){
  predictedGoodozone <- predict(m, airquality)
  results1 <- table(predictedGoodozone, airquality$goodOzone)
  print(results1)
  percentCorrect1 <- (results1[1,1]+results1[2,2])/(results1[1,1]+results1[1,2]+results1[2,1]+results1[2,2])*100
  round(percentCorrect1)  
  return(percentCorrect1)
}

#KSVM model for goodOzone
KSVM1 <- ksvm(goodOzone ~ ., data = airquality)
predictGoodozone(KSVM1, airquality)


root_square1 <- function(error)
{
  sqrt(mean(error^2))
}

KSVM1.first <- predict(KSVM1, airquality)
KSVM1.error <- (airquality$goodOzone - KSVM1.first)
root_square1(KSVM1.error)


NewAirquality1 <- data.frame(airquality$Wind,airquality$Temp,KSVM1.error)
colnames(NewAirquality1) <- c("Wind","Temp","Error")
plotAq11 <- ggplot(data = NewAirquality1,aes(x=airquality$Temp,y=airquality$Wind)) + geom_point(aes(size=KSVM1.error), color = "red") + ggtitle("KSVM Model")
plotAq11

#SVM model for goodOzone
SVM1 <- svm(goodOzone ~ ., data = airquality)
predictGoodozone(SVM1, airquality)

SVM1.first <- predict(SVM1, airquality)
SVM1.error <- (airquality$goodOzone - SVM1.first)
root_square1(SVM1.error)

NewAirquality12 <- data.frame(airquality$Wind,airquality$Temp,SVM1.error)
colnames(NewAirquality12) <- c("Wind","Temp","Error")
plotAq12 <- ggplot(data = NewAirquality12,aes(x=airquality$Temp,y=airquality$Wind)) + geom_point(aes(size=SVM1.error), color = "red") + ggtitle("SVM Model")
plotAq12


grid.arrange(plotAq11,plotAq12, ncol = 1)

#Step 6: best Models for this data? 

#the KSVM is the better fit compared to other models.