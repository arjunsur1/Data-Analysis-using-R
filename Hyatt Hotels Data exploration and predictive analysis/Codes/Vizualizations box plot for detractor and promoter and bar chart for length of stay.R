install.packages("zipcode")
install.packages("Rcpp")
install.packages("ggplot2")
install.packages("ggmap")
install.packages("maps")
library("zipcode")
library("Rcpp")
library("ggmap")
library(maps)
library("ggplot2")

#Visualizing the purpose of visit
 vizDataFrame<-mainDataFrame
purp_vis <- cbind.data.frame(vizDataFrame$POVH, vizDataFrame$State)
summary(purp_vis)
colnames(purp_vis) <- c( "POVH", "State")
purp_vis <- subset(purp_vis, State == "California")
purp_vis$POVH[purp_vis$POVH == ""] <- NA
purp_vis$State[purp_vis$State == ""] <- NA
purp_vis <- na.omit(purp_vis)
any(is.na(purp_vis))
summary(purp_vis)
tapply(purp_vis$State, purp_vis$POVH, length)
slices <- c(15840, 17005, 4650, 354)
lbls <- c("Business", "Leisure", "Business and Leisure","Unspecified")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(slices,labels = lbls, col=rainbow(length(lbls)),
    main="Purpose of Visit")




#visualization for length of stay based on purpose of visit
install.packages("stringr")
library(stringr)
lengthOfStay <- cbind.data.frame(vizDataFrame$POVH, vizDataFrame$Length_Of_Stay, vizDataFrame$State)
colnames(lengthOfStay) <- c("POVH", "Length_Of_Stay", "State")
lengthOfStay <- subset(lengthOfStay, State == "California")
lengthOfStay <- subset(lengthOfStay, Length_Of_Stay>0 & Length_Of_Stay<15)
lengthOfStay$Length_Of_Stay[lengthOfStay$Length_Of_Stay == ""] <- NA
lengthOfStay$POVH[lengthOfStay$POVH == ""] <- NA
lengthOfStay$State[lengthOfStay$State == ""] <- NA
lengthOfStay <- na.omit(lengthOfStay)
lengthOfStay$POVH <- str_replace(lengthOfStay$POVH,"Combination of both business and leisure","Combination")
f <-ggplot(data=subset(lengthOfStay,!(POVH %in% "Prefer not to answer")),aes(x=as.numeric(Length_Of_Stay)))
f <-f+ geom_bar(stat="count",position="stack") 
f <-f +labs(title = "Demographic Distribution",x="Length of Stay")
f <- f + facet_grid(~ POVH) + xlim(0,10) + scale_fill_discrete(name="Number of People")
f


install.packages("ggplot2")
library("ggplot2")
install.packages("gridExtra")
library("gridExtra")
install.packages("grid")
library("grid")
install.packages("arules")

#Analysis on promoters and Detractors
Data1 <-as.numeric(vizDataFrame$L2R)
Detractors <- subset(vizDataFrame,Data1 < 7)
View(Promoters) <- subset(vizDataFrame,Data1>8)

Data2 <- cbind.data.frame(Detractors$`F&B_Freq`, Detractors$Internet_Score,Detractors$State,Detractors$Guest_Room_Score, Detractors$Tranquility_Score, Detractors$Hotel_Condition_Score, Detractors$Cust_Service_Score, Detractors$Staff_Care, Detractors$Check_In, Detractors$NPS_Type)
colnames(Data2) <- c("Fnb","InternetScore","State" , "Guest_Room_Score", "Tranquility_Score", "Hotel_Condition_Score", "Cust_Service_Score","Staff_Care", "Check_In", "NPS_Type" )
Data3 <- subset(Data2, State == "California")
Data3$State[Data3$State == ""] <- NA
Data3$Guest_Room_Score[Data3$Guest_Room_Score == ""] <- NA
Data3$Tranquility_Score[Data3$Tranquility_Score == ""] <- NA
Data4 <- na.omit(Data3)
View(Data4)


Data5 <- cbind.data.frame(Promoters$`F&B_Freq`,Promoters$`F&B_Exp`,Promoters$Internet_Score,Promoters$State,Promoters$Guest_Room_Score, Promoters$Tranquility_Score, Promoters$Hotel_Condition_Score, Promoters$Cust_Service_Score, Promoters$Staff_Care, Promoters$Internet_Score, Promoters$Check_In, Promoters$NPS_Type)
colnames(Data5) <- c("F&B_Freq","F&B_Exp","Net_Rev","State" , "Guest_Room", "Tranquility", "Condition_Hotel", "Customer_SVC","Staff_Cared", "Internet_Sat", "Check in", "NPS_Type" )
Data6 <- subset(Data5, State == "California")
Data3$State[Data3$State == ""] <- NA
Data3$Guest_Room_Score[Data3$Guest_Room_Score == ""] <- NA
Data3$Tranquility_Score[Data3$Tranquility_Score == ""] <- NA
Data7 <- na.omit(Data6)
Guest_Room_box <- Data4$Guest_Room_Score
library(grid)
library(gridExtra)
library(ggplot2)
library(s)
#visualization for NPS value of detractors
Detra<-boxplot(Data4$InternetScore,Data4$Guest_Room_Score,Data4$Tranquility_Score,
           Data4$Hotel_Condition_Score,Data4$Cust_Service_Score,
           Data4$Staff_Care,Data4$Check_In,
           las=2, names = c("Internet","Guest","Tranquility","Hotel_Cond","Cust_SVC","Staff_Care","Check In"), col=c("violet","purple","blue","green","yellow","blue","red"), at = c(1,2,3,4,5,6,7))



#visualization for NPS value of promoters
Promo<-boxplot(Data7$`F&B_Exp`,Data7$Guest_Room,Data7$Tranquility,Data7$Condition_Hotel, Data7$Customer_SVC,Data7$Staff_Cared,Data7$Internet_Sat,Data7$`Check in`,las=2, names = c("FnB_EXP","GuestRoom","Tranquility","Hotel_Cond","Cust_Svc","Staff_Care","Int_Sat","Check In"),col=c("purple","blue","green","yellow","orange","red","Sienna","Pink"),at = c(1,2,3,4,5,6,7,8))





