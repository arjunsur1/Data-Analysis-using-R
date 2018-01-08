#loading the dataset
data10 <-airquality
#cleaning the dataset
#replace NA with mean in Ozone column

data10$Ozone[is.na(data10$Ozone)] <- mean(data10$Ozone, na.rm=TRUE)


#replace NA with mean in Solar column

data10$Solar.R[is.na(data10$Solar.R)] <- mean(data10$Solar.R, na.rm=TRUE)

View(data10)

#Dividing the data into separate rows
random.indexes <- sample(1:nrow(data10))
cutPoint2_3 <- floor(nrow(data10)/3*2)

#Creating test and train datasets for future computation
data10.train <- data10[random.indexes[1:cutPoint2_3],]
data10.test <- data10[random.indexes[(cutPoint2_3+1):nrow(data10)],]
data10.train
data10.test

#KSVM model using kernlab package to train Dataset

#Building a Model using packages
install.packages("e1071")
library(e1071)
install.packages("kernlab")
library(kernlab)

model.ksvm <- ksvm(Ozone ~ . , data=data10)

#predictions
PredictY<- predict(model.ksvm, data10.test)
PredictY

#RMSE
error <- data10.test$Ozone - PredictY
sqrt(mean(error^2))

#Deployment and the usage of a scatter plot for visualization
install.packages("ggplot2")
library("ggplot2")
g1 <- ggplot(data10.test, aes(x=Temp, y=Wind)) + geom_point(aes(size = error, color=error)) 
g1


#Using SVM to train the data set
model.svm <- svm(Ozone ~ Solar.R+Wind , data=data10.train)
PredictY2<- predict(model.svm, data10.test)

#RMSE
error1 <- data10.test$Ozone - PredictY2
sqrt(mean(error1^2))

#Using a scatter plot for visualization
g2 <- ggplot(data10.test, aes(x=Temp, y=Wind)) + geom_point(aes(size = error1, color=error1)) 
g2

#Use of Linear Modelling 
model.lm <- lm(formula = Ozone~Wind+Temp, data = data10.train)
PredictY3 <- predict(model.lm, data10.test, type="response")
error2 <- data10.test$Ozone - PredictY3
#RMSE
sqrt(mean(error2^2))
#Scatter Plot
g3 = ggplot(data = data10.test, aes(x=Temp,y=Wind)) + geom_point(aes(size=error2, color = error2)) 
g3

#Illustration of All plots in one window
install.packages("gridExtra")
library(gridExtra)
grid.arrange(g1,g2,g3, ncol=3)

#creating a good ozone variable 
#Calculating the mean and depicting it
Meandf<- mean(data10$Ozone)
data10$goodOzone <- ifelse(data10$Ozone<Meandf,0,1)
dim(data10)
table(data10$goodOzone)

#training the dataset to predict good or bad ozone levels

newdf.train <- data10[random.indexes[1:cutPoint2_3],]
newdf.test <- data10[random.indexes[(cutPoint2_3+1):nrow(data10)],]

#Model 1- KSVM
model.ksvm2 <- ksvm(as.factor(goodOzone) ~ ., newdf.train)
predictGO <- predict(model.ksvm2, newdf.test)
summary(predictGO)
str(predictGO)

#RMSE
errorGO <- as.numeric(newdf.test$goodOzone)-as.numeric(predictGO)
View(errorGO)
sqrt(mean(errorGO^2))

#Scatter Plot
View(newdf.test)
g4 <- ggplot(data = newdf.test, aes(x=Temp,y=Wind)) + geom_point(aes(size=errorGO,shape = predictGO, color = newdf.test$goodOzone))
g4

#Calculating KSVM
results <- table(predictGO, newdf.test$goodOzone)
print(results)
Correct <- (results[1,1]+results[2,2])/(results[1,1]+results[1,2]+results[2,1]+results[2,2])*100
Correct

#Model 2- SVM
model.svm2 <- svm(as.factor(goodOzone) ~ ., newdf.train, type = "C-classification")
predictGO1 <- predict(model.svm2, newdf.test)
summary(predictGO1)
errorGO1 <- as.numeric(newdf.test$goodOzone) - as.numeric(predictGO1)
sqrt(mean(errorGO1^2))

#Scatter plot
g5 = ggplot(data = newdf.test, aes(x=Temp,y=Wind)) + geom_point(aes(size=errorGO1,shape = predictGO1, color = newdf.test$goodOzone))
g5


#Calculating how accurately svm has predicted
results2 <- table(predictGO1, newdf.test$goodOzone)
print(results2)
Correct2 <- (results2[1,1]+results2[2,2])/(results2[1,1]+results2[1,2]+results2[2,1]+results2[2,2])*100
Correct2


# Model 3- Naive Bayes
model.nb <- naiveBayes(as.factor(goodOzone) ~ ., newdf.test)
NBpredict <- predict(model.nb, newdf.test)
str(NBpredict)
summary(NBpredict)
errorNB <- as.numeric(newdf.test$goodOzone) - as.numeric(NBpredict)
sqrt(mean(errorNB^2))

#Calculating how accurately nb has predicted
results3 <- table(NBpredict, newdf.test$goodOzone)
print(results3)
Correct3 <- (results3[1,1]+results3[2,2])/(results3[1,1]+results3[1,2]+results3[2,1]+results3[2,2])*100
Correct3

# Scatter Plot:
g6 = ggplot(data = newdf.test, aes(x=Temp,y=Wind)) + geom_point(aes(size=errorNB,shape = NBpredict, col = newdf.test$goodOzone))
g6
grid.arrange(g4,g5,g6, ncol=3)


#Step 6
#Naive baves is the best model that is used followed by KSVM and SVM