library("gdata")
library("zipcode")
library("maps")
library("ggplot2")
library(readxl)

#step 1
#read the dataa

HW7_Data <- read_excel("C:/Users/xboxv/Downloads/HW7-Data.xlsx")
View(HW7_Data)

#renaming the colmnsu
colnames(HW7_Data) <- c("zip","median","mean","population")
View(HW7_Data)

#importing the zipcode package
data("zipcode")

#remvoing Alaska and Hawai from the data
latestZip <- subset(zipcode,zipcode$state != "AK")
newZip <- subset(latestZip,latestZip$state != "HI")

#step 2

#combining the two dataframes
mergeData <- merge(x=HW7_Data,y=newZip,by="zip")

#in mergeData, this will sort the state abbreviations
stateAbr <- sort(unique(mergeData$state))

#get the average and the population
avgMedian <- tapply(as.numeric(mergeData$median),mergeData$state,mean)
newPop <- tapply(as.numeric(mergeData$population),mergeData$state,sum)

#a simple data frame with just the average median income and the population for each state.
simpleData <- data.frame(avgMedian,newPop,stateAbr)

#adding the states name to the SimpleData dataframe
simpleData$states <- state.name[match(simpleData$stateAbr,state.abb)]
     
#get the map data ofr US                 
US_Map <- map_data("state")

#creating a simple map
map.simple	<- ggplot()		
map.simple	<- map.simple +	geom_map(data=US_Map,aes(x=US_Map$long,y=US_Map$lat,map_id=region),map=US_Map,fill="white",	color="black")	
map.simple
#Map representing the color with the average median income of that state
simpleData$states <- tolower(simpleData$states)
Income_map	<- map.simple + geom_map(data=simpleData,map=US_Map,aes(fill=avgMedian,map_id=states),color="black",na.rm=TRUE)
Income_map

#second map with color representing the population of the state.
Popu_map <- map.simple + geom_map(data=simpleData,map=US_Map,aes(fill=newPop,map_id=states),color="black",na.rm=TRUE)
Popu_map


#Step 3

#Show the income per zip code
Inc_zip <- map.simple + geom_point(data=mergeData,aes(x=mergeData$latitude,y=mergeData$longitude,colour=median),na.rm=TRUE)
Inc_zip <- Inc_zip + ggtitle("Income per zip code")
Inc_zip

#step 4
#Using the Stat.density2D fucntion to represent the zip code density 
map.density <- map.simple + stat_density2d(aes(x=mergeData$longitude, y=mergeData$latitude), data=mergeData, geom="polygon") +
  scale_fill_gradient(low="black",high="green")+
  scale_alpha(range=c(0.00,0.25))+
  ggtitle("Density for all Zip codes in USA")

map.density

#step 5

library(ggmap)

# Get api key using google cloud platform, did a lot of rersearch on it to getthis zoominng step right.
register_google(key = "AIzaSyCniRebA4EVvTRnWlA0iw4KnDdFeuPFUN0")

# Get the center of New York by using get_googlemap
NyMap <- get_googlemap("New York", zoom = 5) %>% ggmap()

#step 3 again using hte new map
Inc_zip <- NyMap + geom_point(data=mergeData,aes(x=mergeData$latitude,y=mergeData$longitude,colour=median),na.rm=TRUE)
Inc_zip <- Inc_zip + ggtitle("Income per zip code")
Inc_zip

#step 4 again
map.density <- NyMap + stat_density2d(aes(x=mergeData$longitude, y=mergeData$latitude), data=mergeData, geom="polygon") +
  scale_fill_gradient(low="black",high="green")+
  scale_alpha(range=c(0.00,0.25))+
  ggtitle("Density for all Zip codes in USA")

map.density


