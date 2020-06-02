library(ggplot2)
library(reshape2)

#Step 1 and Step 2
airquality
new_data <-airquality
summary(new_data)

# remove na and store the data to a new var
remove_na <- function(df, n=0){
  df[rowSums(is.na(df)) <= n,]
}

new_data <- remove_na(new_data)
summary(new_data)



#step 3,  Histogram for each var
ggplot(new_data, aes(x = Ozone)) + geom_histogram(binwidth = 10, fill="blue", color="black")
ggplot(new_data, aes(x = Solar.R)) + geom_histogram(binwidth = 10, fill="red", color="black")
ggplot(new_data, aes(x = Wind)) + geom_histogram(binwidth = 1, fill="orange", color="black")
ggplot(new_data, aes(x = Temp)) + geom_histogram(binwidth = 1, fill="green", color="black")

#ozone boxplot
ggplot(new_data, aes(y = Solar.R, x = 1)) + geom_boxplot(fill="orange")

#Boxplot for different wind values
wind<-factor(new_data$Wind)
ggplot(new_data, aes(y = Wind, x = 1)) + geom_boxplot(fill="blue")

#Step 3

#merge month and date into date variable
date <- as.Date(with(new_data, paste(1973, new_data$Month, new_data$Day,sep="-")), "%Y-%m-%d")

##Line Plot for each variable
ggplot(new_data, aes(x= date, y = Ozone)) + geom_line(color = "blue") + geom_point()
ggplot(new_data, aes(x= date, y = Solar.R)) + geom_line(color = "red") + geom_point()
ggplot(new_data, aes(x= date, y = Wind)) + geom_line(color = "orange") + geom_point() 
ggplot(new_data, aes(x= date, y = Temp)) + geom_line(color = "green") + geom_point()

#combines all the line plots and shows on one plot
ggplot(new_data, aes(x=date)) + 
  geom_line(aes(y = Ozone, color= "Ozone") ) +
  geom_line(aes(y = Solar.R,  color = "Solar.R") ) +
  geom_line(aes(y = Wind, color = "Wind")) +
  geom_line(aes(y = Temp, color = "Temp" ))

#step 4

#combines all vars  and creates a heatmap
ggplot(new_data, aes(x=Day, group=Day)) +
  geom_tile(aes(y = Ozone, color= "Ozone") ) +
  geom_tile(aes(y = Solar.R,  color = "Solar.R") ) +
  geom_tile(aes(y = Wind, color = "Wind")) +
  geom_tile(aes(y = Temp, color = "Temp" ) )


#Step 5

#Makes a  scatter chart, where  the x-axis representing the wind, the y-axis representing the temperature,
#the size of each dot representing the ozone and the color representing solar.R.
ggplot(new_data, aes(x=Wind,y=Temp)) +
  geom_point(aes(size=Ozone,color=Solar.R)) 

