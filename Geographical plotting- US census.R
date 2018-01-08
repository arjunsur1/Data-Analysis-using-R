install.packages('gdata')
install.packages('ggmap')
install.packages('xlsx')
install.packages('zipcode')

library(xlsx)
library(ggplot2)
library(RCurl)
library(ggmap)
library(gdata)

census<-read.csv(file.choose(),stringsAsFactors = FALSE)
census

#cleaning data
UScensus<- census[-1,]
colnames(UScensus)<- c('ZIP', 'median', 'mean', 'population')
UScensus 

UScensus$median<- as.character(gsub(",","", census$median))
UScensus$mean<- as.character(gsub(",","", census$mean))
UScensus$population<- as.character(gsub(",","", census$population))
UScensus

#zipcodes
library(zipcode)
data(zipcode)
View(zipcode)
str(zipcode)

UScensus$Zip<- clean.zipcodes(UScensus$Zip)
View(UScensus)
str(UScensus)

UScensus$median<- as.numeric(gsub(",","", UScensus$median))
UScensus$mean<- as.numeric(gsub(",","", UScensus$mean))
UScensus$population<- as.numeric(gsub(",","", UScensus$population))
UScensus

#merging US census with zipcodes
UScensus<- merge(UScensus, zipcode, by.x = 'ZIP', by.y = 'zip')
UScensus

#removing Hawaii and Alaska
census48<- UScensus[which(UScensus$state !="HI"),]
census48<- census48[which(census48$state!= "AK"),]
census48

#finding mean of median incomes
median<- tapply(census48$median,census48$state, mean)
median
avgStatedf<- data.frame(median)
avgStatedf

#total population of each state
avgStatedf$pop<- tapply(census48$population, census48$state, sum)
avgStatedf

avgStatedf$stateAbb <- rownames(avgStatedf)
avgStatedf

#replacing rownames with incremental nos starting from 1
rownames(avgStatedf) <- c(1:length(avgStatedf$median))
#Adding state names and state abbreviations to new columns


#lower case of state names
avgStatedf$stateName<- tolower(state.name[match(avgStatedf$stateAbb,state.abb)])
avgStatedf

#removing record of Washington DC
avgStatedf<- avgStatedf[which(avgStatedf$stateName !="DC"),]

#US staes based on avg income
usMapData<- map_data("state")
mapAvgIncome <- ggplot(avgStatedf, aes(map_id = stateName, fill= median))
mapAvgIncome <- mapAvgIncome + geom_map(map = usMapData, color="black")
mapAvgIncome

mapAvgIncome <- mapAvgIncome+ expand_limits(x = usMapData$long, y = usMapData$lat)
mapAvgIncome<- mapAvgIncome +coord_map() + ggtitle("US states based on Average Median Income of State")
mapAvgIncome

mapPop <- ggplot(avgStatedf, aes(map_id = stateName, fill= pop))
mapPop <- mapPop + geom_map(map = usMapData, color="white")

#forming states using lat and long based on mean state income
mapPop <- mapPop+ expand_limits(x = usMapData$long, y = usMapData$lat)
mapPopulation<- mapPop +coord_map() + ggtitle("US states based on Total Population of State")
mapPop  <- mapPop +scale_fill_continuous(low ="grey", high="black")
mapPop

#census 
censusMedian <- census48
censusMedian
censusMedian$stateName<- tolower(state.name[match(censusMedian$state,state.abb)])
mapZipCode <- ggplot(censusMedian, aes(map_id = stateName, fill="red"))
mapZipCode <- mapZipCode + geom_map(map = usMapData) +theme(panel.background = element_rect(fill = 'black', colour = 'white'))

#income per zip code
mapZipCode <- mapZipCode+ expand_limits(x = usMapData$long, y = usMapData$lat)
mapZipCode<- mapZipCode +coord_map() + ggtitle("Median Income by Zipcodes")

mapZipCode <- mapZipCode + geom_point(data =censusMedian, aes(x=longitude, y=latitude, color= median), size = 1)
mapZipCode 

#Show Zip Code Density

censusMedian$ZIP <- as.numeric(as.character(censusMedian$ZIP))
censusMedian$ZIP <- as.character(as.numeric(censusMedian$ZIP))
mapUS <- ggplot(censusMedian, aes(map_id = stateName, fill="red"))
mapUS <- mapUS + geom_map(map = usMapData, color="white")

#forming states using lat and long based on mean state income

mapUS <- mapUS+ expand_limits(x = usMapData$long, y = usMapData$lat)
mapUS<- mapUS +coord_map() + ggtitle("Zipcode density visualization")
mapUS <- mapUS + geom_point(data =censusMedian, aes(x=longitude, y=latitude),color="blue", size=1)
mapUS <- mapUS + stat_density2d( data = censusMedian, geom = "density2d",
                                 position = "identity", na.rm = FALSE, contour = TRUE, aes(x=longitude, y=latitude))
mapUS

#Zooming in east coast region around NYC


#Repeating Step 3 for NY

#NY records table
nyState <- censusMedian[which(censusMedian$state =="NY"),]
nyState

#selecting max, min latitude and longitude
minLat<- min(nyState$latitude)
maxLat <- max(nyState$latitude)
minLong <- min(nyState$longitude)
maxLong <- max(nyState$longitude)


mapNYZipCode <- ggplot(nyState, aes(map_id = stateName))
mapNYZipCode <- mapNYZipCode + geom_map(map = usMapData,fill="grey") +theme(panel.background = element_rect(fill = 'blue', colour = 'white'))


mapNYZipCode <- mapNYZipCode+ expand_limits(x = usMapData$long, y = usMapData$lat)
#Zooming in
mapNYZipCode<- mapNYZipCode + xlim(minLong,maxLong) + ylim(minLat, maxLat) +coord_map() + ggtitle("Median Income by Zipcodes in NY")

mapNYZipCode <- mapNYZipCode + geom_point(data =nyState, aes(x=longitude, y=latitude, color=median), size = 2)
mapNYZipCode

#Repeating step 4 for NY

mapNY <- ggplot(nyState, aes(map_id = stateName))
mapNY <- mapNY + geom_map(map = usMapData, color="black", fill="white")

mapNY <- mapNY+ expand_limits(x = usMapData$long, y = usMapData$lat)
#Mapping
mapNY<- mapNY + xlim(minLong,maxLong) + ylim(minLat, maxLat) + coord_map() + ggtitle("Density of zipcodes in NY")
mapNY <- mapNY + geom_point(data =nyState, aes(x=longitude, y=latitude),color="orange", size=2)
mapNY <- mapNY + stat_density2d( data = nyState, geom = "density2d",
                                 position = "identity", contour = TRUE, aes(x=longitude, y=latitude))
mapNY

