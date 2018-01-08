###############################################
######## MODEL 3: Gender Male Analysis ########
###############################################

frameLM2 = mainDataFrame

frameLM2.A = frameLM2[,c(2,4:10,20,23,24,26,27)]
frameLM2.B = frameLM2.A[frameLM2.A$Gender == "Male",]

colnames(frameLM2.B)[colSums(is.na(frameLM2.B))>0]

random.LM2 = sample(1:dim(frameLM2.B)[1])

#Split data
cutpoint.LM2 = floor(2* dim(frameLM2.B)[1]/3)

#Create Training an Testing DF
trainSet.LM2 = frameLM2.B[random.LM2[1:cutpoint.LM2],]

testSet.LM2 = frameLM2.B[random.LM2[(cutpoint.LM2+1):dim(frameLM2.B)[1]],]


######### NPS VS. Guest + service + Service + Hotel Condition + Staff Care ##########

########### Linear Model ########### 
lm.lm2 = lm(formula = L2R~Guest_Room_Score+Cust_Service_Score+Hotel_Condition_Score+Staff_Care, 
            data = trainSet.LM2)
summary(lm.lm2)
lmPredict2 = predict(lm.lm2, testSet.LM2)

sum(testSet.LM2$L2R==round(lmPredict2))/length(lmPredict2)

########### Naive Bayes Model ###########
nbTrain.LM2 = naiveBayes(as.factor(NPS_Type)~Guest_Room_Score+Cust_Service_Score+Hotel_Condition_Score+Staff_Care,
                         data = trainSet.LM2,kernel = "rbfdot")
nb_Predict.LM2 = predict(nbTrain.LM2 , testSet.LM2)

sum(testSet.LM2$NPS_Type==nb_Predict.LM2)/length(nb_Predict.LM2)

########### SVM Model ###########
svm_O.LM2 = svm(NPS_Type~Guest_Room_Score+Guest_Room_Score+Cust_Service_Score+Hotel_Condition_Score+Staff_Care, 
                trainSet.LM2)
svm_Pred_LM2 = predict(svm_O.LM2, testSet.LM2)
#length(svm_Pred)
sum(testSet.LM2$NPS_Type==svm_Pred_LM2)/length(svm_Pred_LM2)
