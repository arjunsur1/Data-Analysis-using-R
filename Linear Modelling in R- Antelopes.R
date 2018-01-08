library(ggplot2)
install.packages("caret")

#reading the data set
antelope<-read.csv(file.choose(),stringsAsFactors = FALSE)
antelope

colnames(antelope)<-c('numfawn', 'popadultant', 'anprecip', 'wintergrade')
head(antelope)
str(antelope)
summary(antelope)

#bivariate plots population of adult vs number of fawns
plotpop<- ggplot(antelope, aes(x=popadultant,y=numfawn))+geom_point(color="red")
plotpop

#bivariate plots precipitation vs number of fawns
plotprec<- ggplot(antelope, aes(x=anprecip,y=numfawn))+geom_point(color="blue")
plotprec

#bivariate plots precipitation vs number of fawns
plotwinter<- ggplot(antelope, aes(x=wintergrade,y=numfawn))+geom_point(color="black")
plotwinter

#predicting number of fawns from severity of winter
#modelling with one variable

#prediction based on winter (one variable)

model1 <- lm(formula=numfawn ~ wintergrade, data=antelope)
summary(model1)
plot(model1)
abline(model1)

test1 = data.frame(wintergrade=2)
predict(model1,test1, type="response")

#prediction based on population of adults (one variable)
model2 <- lm(formula=numfawn ~ popadultant , data=antelope)
summary(model2)

test2 = data.frame(popadultant=6)
predict(model2,test2, type="response")

#prediction based on precipitation (one variable)
model3 <- lm(formula=numfawn ~ anprecip , data=antelope)
summary(model3)
plot(model3)

test3 = data.frame(anprecip=6)
predict(model2,test2, type="response")

#modelling with two variables

#prediction based on precipitation and wintergrade of adults (two variables)
model4 <- lm(formula=numfawn ~ wintergrade+anprecip, data=antelope)
summary(model4)
plot(model4)

test4 = data.frame(wintergrade=4,anprecip=6)
predict(model4,test4, type="response")

#prediction based on population of adults and wintergrade of adults (two variables)
model5 <- lm(formula=numfawn ~ wintergrade+popadultant, data=antelope)
summary(model5)
plot(model5)

test5 = data.frame(wintergrade=2,popadultant=6)
predict(model5,test5, type="response")

#prediction based on precipitation and wintergrade of adults (two variables)
model6 <- lm(formula=numfawn ~ anprecip+popadultant, data=antelope)
summary(model6)
plot(model6)

test6 = data.frame(anprecip=4,popadultant=6)
predict(model6,test6, type="response")

#modelling with 3 variables

#prediction based on precipitation, wintergrade population of adult (three variables)
model7 <- lm(formula=numfawn ~ wintergrade+anprecip+popadultant, data=antelope)
summary(model7)
plot(model7)

test7 = data.frame(wintergrade=2,anprecip=4,popadultant=6)
predict(model7,test7, type="response")

