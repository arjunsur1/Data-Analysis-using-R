 load("C:/Users/bhaum/Desktop/Project Files/Data Sets/MainDataFrame.rdata")

mainDataFrame
frameA = mainDataFrame
boxplot(detractor$Guest_Room_Score,detractor$Tranquility_Score, detractor$Hotel_Condition_Score, detractor$Internet_Score, las= 2)

lm = lm(formula = L2R~Guest_Room_Score+Cust_Service_Score+Tranquility_Score
        + Hotel_Condition_Score+ Cust_Service_Score+Staff_Care+Brand+Gender
        + POVC , data = trainSet)
summary(lm)
lmPredict = predict(lm, testSet)
sum(testSet$L2R==round(lmPredict))/length(lmPredict)



library("kernlab")
library("e1071")
library("ggplot2")
library("gridExtra")

svmOutput <- ksvm( L2R~Guest_Room_Score+Cust_Service_Score+Tranquility_Score
                   + Hotel_Condition_Score+ Cust_Service_Score+Staff_Care+Brand+Gender
                   + POVC  , 
                   data=trainSet,
                   kernel="rbfdot",
                   kpar="automatic",
                   C=5,
                   cross=3,
                   prob.model=TRUE)
svmPredict = predict(svmOutput, testSet, type="votes")
#predicted2 <-predict.lm(svmOutput, predict.lm(model,data.frame(Sepal.Width=test$Sepal.Width))
svmPredict

cat("RMS Error",sqrt(mean((testSet$L2R-svmPredict)^2)))


frameA = mainDataFrame
nps_type = NULL
for(i in 1:43987){
  
  if(frameA$NPS_Type[i]=="Promoter") {
    #print("A")
    nps_type[i] = 1 
  } else if(frameA$NPS_Type[i]=="Detractor"){
    #print("B")
    nps_type[i] = 3
  } else{
    #print("C")
    nps_type[i]= 2
  }
  #print ("D")
}


frameA$nps_type_dig = nps_type

random = sample(1:dim(frameA)[1])

#Split data
cutpoint = floor(2* dim(frameA)[1]/3)

#Create Training an Testing DF
trainSet = frameA[random[1:cutpoint],]

testSet = frameA[random[(cutpoint+1):dim(frameA)[1]],]


svmOutput <- ksvm( nps_type_dig~Guest_Room_Score+Cust_Service_Score+ Tranquility_Score, 
                   data=trainSet,
                   kernel="rbfdot",
                   kpar="automatic",
                   C=5,
                   cross=3,
                   prob.model=TRUE)
svmPredict = predict(svmOutput, testSet, type="votes")


cat("RMS Error",sqrt(mean((testSet$nps_type_dig[1:14244]-svmPredict)^2)))
sum(testSet$nps_type_dig[1:14244]==round(svmPredict))/length(svmPredict)

error = abs(round(svmPredict) - testSet)

g = ggplot(compTable,aes(x=Temp,y=Wind))
g = g + geom_point(aes(size = error,color = Actual, shape=Predicted))
g

