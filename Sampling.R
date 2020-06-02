#HW 4
library(moments)

#distribution of a vector
printVectorInfo <-function(vectorInputs){
  mean_value <- mean(vectorInputs)
  median_value <- median(vectorInputs)
  min_value <- min(vectorInputs)
  max_value <- max(vectorInputs)
  std_value <- sd(vectorInputs)
  qt_value <- quantile(vectorInputs, probs = c(0.05, 0.95))
  skw_value <- skewness(vectorInputs)
  cat('Mean : ',mean_value,'\n')
  cat('Median : ',median_value,'\n')
  cat('Min : ',min_value,'  ')
  cat('Max : ',max_value, '\n')
  cat('Std : ',std_value,'\n')
  cat('quatile : ',qt_value,'\n')
  cat('Skewness : ',skw_value,'\n\n')
}

#test the fucntion
x <- c  (1,2,3,4,5,6,7,8,9,10,50)
printVectorInfo(x)

#step 2

new_jar <- c('red','blue')
jar <- rep(new_jar,50)
length(which(jar == 'red'))

new_sample <- 10
samples <- sample(jar,new_sample, replace = TRUE)
red_prop <- length(which(samples == 'red'))/new_sample 
red_prop
samples_prop <- replicate(20, length(which((sample(jar,new_sample, replace = TRUE)) == 'red'))/new_sample)

hist(samples_prop)
printVectorInfo(samples_prop)
#100 times
new_sample <- 100
samples_prop2 <- replicate(20, length(which((sample(jar,new_sample, replace = TRUE)) == 'red'))/new_sample)
hist(samples_prop2)
printVectorInfo(samples_prop2)
#1000 times
new_sample <- 1000
samples_prop3 <- replicate(20, length(which((sample(jar,new_sample, replace = TRUE)) == 'red'))/new_sample)
hist(samples_prop3)
printVectorInfo(samples_prop3)

#air Quality Dataset
temp_data <-airquality
summary(temp_data)

#remove na
remove_na <- function(df, n=0){
  df[rowSums(is.na(df)) <= n,]
}

temp_data <- remove_na(temp_data)
summary(temp_data)

printVectorInfo(temp_data$Ozone)
printVectorInfo(temp_data$Wind)
printVectorInfo(temp_data$Temp)

#sapply function
sapply(temp_data, printVectorInfo)
