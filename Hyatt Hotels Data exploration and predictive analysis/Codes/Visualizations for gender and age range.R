library("zipcode")
library("ggplot2")
install.packages("ggmap")
library("ggmap")
install.packages("gridExtra")
library(gridExtra)

# Rating Values
mainDataFrame$Guest_Room_Score [is.na(mainDataFrame$Guest_Room_Score)] = ceiling(mean(mainDataFrame$Guest_Room_Score, na.rm = TRUE))
mainDataFrame$Tranquility_Score [is.na(mainDataFrame$Tranquility_Score)] = ceiling(mean(mainDataFrame$Tranquility_Score, na.rm = TRUE))
mainDataFrame$Hotel_Condition_Score [is.na(mainDataFrame$Hotel_Condition_Score)] = ceiling(mean(mainDataFrame$Hotel_Condition_Score, na.rm = TRUE))
mainDataFrame$Cust_Service_Score [is.na(mainDataFrame$Cust_Service_Score)] = ceiling(mean(mainDataFrame$Cust_Service_Score, na.rm = TRUE))
mainDataFrame$Staff_Care [is.na(mainDataFrame$Staff_Care)] = ceiling(mean(mainDataFrame$Staff_Care, na.rm = TRUE))
mainDataFrame$Internet_Score [is.na(mainDataFrame$Internet_Score)] = ceiling(mean(mainDataFrame$Internet_Score, na.rm = TRUE))
mainDataFrame$Check_In [is.na(mainDataFrame$Check_In)] = ceiling(mean(mainDataFrame$Check_In, na.rm = TRUE))

#check the na values
sum(is.na(mainDataFrame$Overall_Score))
mainDataFrame = mainDataFrame[! is.na(mainDataFrame$Overall_Score),]

#copy dataset
hotelData <- mainDataFrame
hotelData.Cali <- hotelData

str(hotelData.Cali)
summary(hotelData.Cali)
#visulization for gender
#nps value for female and male
#Male NPS value
MNPS = (sum(hotelData.Cali$NPS_Type=="Promoter"& hotelData.Cali$Gender=="Male")-sum(hotelData.Cali$NPS_Type=="Detractor"&hotelData.Cali$Gender=="Male"))/sum(hotelData.Cali$Gender=="Male")
#Female NPS value
FNPS = (sum(hotelData.Cali$NPS_Type=="Promoter"& hotelData.Cali$Gender=="Female")-sum(hotelData.Cali$NPS_Type=="Detractor"&hotelData.Cali$Gender=="Female"))/sum(hotelData.Cali$Gender=="Female")
#NPS for gender visualization
vizNPS = data.frame(gender=c('Male', 'Female'), NPS= c(MNPS, FNPS))
plot.NPS.Gender = ggplot(vizNPS, aes(x=gender, y=NPS, fill=gender, group=factor(1)))
plot.NPS.Gender = plot.NPS.Gender+geom_bar(stat = 'identity', width = 0.5)

#count of female and male
FCount <- sum(hotelData.Cali$Gender=="Female")
MCount <- sum(hotelData.Cali$Gender=="Male")
df.CountGender <- data.frame(c("Female", "Male"), c(FCount, MCount))
colnames(df.CountGender) <- c("Gender", "Count")
plot.df.CountGender = ggplot(df.CountGender, aes(x=Gender, y=Count, fill=Gender, group=factor(1)))
plot.df.CountGender = plot.df.CountGender + geom_bar(stat = 'identity', width = 0.5)

#visulization for age range
sum(hotelData.Cali$Age_Range=="76+")
CountAge = table(hotelData.Cali$Age_Range)
df.CountAge = data.frame(CountAge)
colnames(df.CountAge) = c("Age","Count")
df.CountAge = df.CountAge[-1,]

plot.df.CountAge = ggplot(df.CountAge, aes(x=Age, y=Count, fill=Age, group=factor(1)))
plot.df.CountAge = plot.df.CountAge + geom_bar(stat = 'identity', width = 0.5)

#function for calculating NPS for different age range
AgeNps <- function(x){
  y = (sum(hotelData.Cali$NPS_Type=="Promoter"& hotelData.Cali$Age_Range==x)-sum(hotelData.Cali$NPS_Type=="Detractor"&hotelData.Cali$Age_Range==x))/sum(hotelData.Cali$Age_Range==x)
  return(y)
  }
Age.Nps.Vec <- c(AgeNps("18-25"),AgeNps("26-35"),AgeNps("36-45"),AgeNps("46-55"),AgeNps("56-65"),AgeNps("66-75"),AgeNps("76+"))
Age.Range <- as.vector(df.CountAge$Age)
df.AgeNPS <- data.frame(Age.Range, Age.Nps.Vec)
colnames(df.AgeNPS) <- c("Age", "AgeNPS")
#visualization for NPS of Age Range
plot.df.AgeNPS = ggplot(df.AgeNPS, aes(x=Age, y=AgeNPS, fill=Age, group=factor(1)))
plot.df.AgeNPS = plot.df.AgeNPS + geom_bar(stat = 'identity', width = 0.5)
grid.arrange(plot.df.CountAge, plot.df.AgeNPS, nrow=2)
