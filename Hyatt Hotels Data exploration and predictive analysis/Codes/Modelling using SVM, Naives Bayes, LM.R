library("kernlab")
library("e1071")

frameLM = mainDataFrame
frameLM = frameLM[,c(2,4:10,20,23,26,27)]
frameLM = frameLM[,c(1:8,10)]
colnames(frameLM)[colSums(is.na(frameLM))>0]

#### Creating Training and Testing Data Frames
random = sample(1:dim(frameLM)[1])
cutpoint = floor(2* dim(frameLM)[1]/3)

trainSet = frameLM[random[1:cutpoint],]

testSet = frameLM[random[(cutpoint+1):dim(frameLM)[1]],]

########### Linear Model ###########
lm = lm(formula = L2R~., data = trainSet)
summary(lm)
lmPredict = predict(lm, testSet)

#Accuracy
sum(testSet$L2R==round(lmPredict))/length(lmPredict)


########### SVM Model ###########
svm_O = svm(NPS_Type~., trainSet)
svm_Pred = predict(svm_O, testSet)
#Accuracy
sum(testSet$NPS_Type==svm_Pred)/length(svm_Pred)

########### Naive Bayes Model ###########
nbTrain = naiveBayes(as.factor(NPS_Type)~.,data = trainSet,kernel = "rbfdot")
#nbTrain
nb_Predict = predict(nbTrain , testSet)
#Accuracy
sum(testSet$NPS_Type==nb_Predict)/length(nb_Predict)
