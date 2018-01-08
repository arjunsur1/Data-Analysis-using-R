load("C:/Users/bhaum/Desktop/Project Files/Data Sets/MainDataFrame.rdata")
# Rating Values

mainDataFrame$Guest_Room_Score [is.na(mainDataFrame$Guest_Room_Score)] = ceiling(mean(mainDataFrame$Guest_Room_Score, na.rm = TRUE))
mainDataFrame$Tranquility_Score [is.na(mainDataFrame$Tranquility_Score)] = ceiling(mean(mainDataFrame$Tranquility_Score, na.rm = TRUE))
mainDataFrame$Hotel_Condition_Score [is.na(mainDataFrame$Hotel_Condition_Score)] = ceiling(mean(mainDataFrame$Hotel_Condition_Score, na.rm = TRUE))
mainDataFrame$Cust_Service_Score [is.na(mainDataFrame$Cust_Service_Score)] = ceiling(mean(mainDataFrame$Cust_Service_Score, na.rm = TRUE))
mainDataFrame$Staff_Care [is.na(mainDataFrame$Staff_Care)] = ceiling(mean(mainDataFrame$Staff_Care, na.rm = TRUE))
mainDataFrame$Internet_Score [is.na(mainDataFrame$Internet_Score)] = ceiling(mean(mainDataFrame$Internet_Score, na.rm = TRUE))
mainDataFrame$Check_In [is.na(mainDataFrame$Check_In)] = ceiling(mean(mainDataFrame$Check_In, na.rm = TRUE))

mainDataFrame = mainDataFrame[! is.na(mainDataFrame$Overall_Score),]


