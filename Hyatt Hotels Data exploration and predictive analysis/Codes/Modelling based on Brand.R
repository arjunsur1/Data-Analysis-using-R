
###############################################
######## MODEL 1: BRAND HYATT REGENCY #########
###############################################

frameLM1 = mainDataFrame

#Taking only relevant columns
frame.LM1 = frameLM1[,c(2,4:10,20,23,24,26,27)]

#Removing NAs from Length Of Stay
frame.LM1 = frame.LM1[!is.na(frame.LM1$Length_Of_Stay) ,]

#Filtering on Brand Hyatt Regency
frame.LM1 = frame.LM1[frame.LM1$Brand == "Hyatt Regency",]

#### Creating Training and Testing Data Frames
random.LM1 = sample(1:dim(frame.LM1)[1])
cutpoint.LM1 = floor(2* dim(frame.LM1)[1]/3)

trainSet.LM1 = frame.LM1[random.LM1[1:cutpoint.LM1],]

testSet.LM1 = frame.LM1[random.LM1[(cutpoint.LM1+1):dim(frame.LM1)[1]],]

######### BRAND = Regency + Room + service +Age Range ########## 

#Checking for NAs
colnames(frame.LM1)[colSums(is.na(frame.LM1))>0]

########### Linear Model ########### 
##Linear Model done with L2R value as this variable cannot a Factor
lm.lm1 = lm(formula = L2R~Guest_Room_Score+Cust_Service_Score+Internet_Score+Staff_Care+Tranquility_Score, 
         data = trainSet.LM1)
summary(lm.lm1)
lmPredict.lm1 = predict(lm.lm1, testSet.LM1)

#Accuracy
#sum(testSet.LM1$L2R==round(lmPredict.lm1))/length(lmPredict.lm1)



########### Naive Bayes Model ###########
nbTrain.LM1 = naiveBayes(as.factor(NPS_Type)~Guest_Room_Score+Cust_Service_Score+Internet_Score+Staff_Care+Age_Range,
                         data = trainSet.LM1, kernel = "rbfdot")
nb_Predict.LM1 = predict(nbTrain.LM1 , testSet.LM1)

#Accuracy
sum(testSet.LM1$NPS_Type==nb_Predict.LM1)/length(nb_Predict.LM1)


########### SVM Model ###########
svm_O.LM1 = svm(NPS_Type~Guest_Room_Score+Cust_Service_Score+Internet_Score+Staff_Care+Age_Range, 
                trainSet.LM1)
svm_Pred.LM1 = predict(svm_O.LM1, testSet.LM1)

#Accuracy
sum(testSet.LM1$NPS_Type==svm_Pred.LM1)/length(svm_Pred.LM1)


