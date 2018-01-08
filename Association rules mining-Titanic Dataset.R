install.packages("arules")
install.packages("arulesViz")
library(arules)
library(arulesViz)

load("titanic.raw.rdata")
View(titanic.raw)
summary(titanic.raw)

ship <- titanic.raw 

#Percentage of people survived
survivalOfPeople <- ((length(grep("Yes",ship$Survived)))/ length(ship$Survived)) * 100
survivalOfPeople

#percentage of children survived
children <- ((length(grep("Child",ship$Age)))/ length(ship$Age)) * 100
children

#percentage of females survived
female<- ((length(grep("Female",ship$Sex)))/ length(ship$Sex)) * 100
female

#percentage of people in first class
classOfPeople<-  ((length(grep("1st",ship$Class)))/ length(ship$Class) ) * 100
classOfPeople

#percentage of children survived
childrenSurvived <- nrow(ship[ship$Survived=="Yes" & ship$Age=="Child",])/nrow(ship[ship$Age=="Child",])
childrenSurvived

#percentage of female survived
femaleSurvived <- nrow(ship[ship$Survived=="Yes" & ship$Sex=="Female",])/nrow(ship[ship$Sex=="Female",])
femaleSurvived

#percentage of first class people survived
firstClassSurvival <- nrow(ship[ship$Survived=="Yes" & ship$Class=="1st",])/nrow(ship[ship$Class=="1st",])
firstClassSurvival

#percentage of 3rd class people survived
thirdClassSurvival<- nrow(ship[ship$Survived=="Yes" & ship$Class=="3rd",])/nrow(ship[ship$Class=="3rd",])
thirdClassSurvival

#function that returns a new dataframe of people that satisfy the specified criteria of sex, age, class and survived as parameters
peopleSurvived <- function(sex,age,class,survived){
  
  df<- ship[(ship$Sex==sex)&(ship$Age==age)&(ship$Class==class)&(ship$Survived==survived),]
  df
}

peopleSurvived('Male','Adult','2nd','No')

#percentage (who lives, who dies) for specified (parameters) of age, class and sex.
percPeopleSurivied <- function(sex,age,class,survived){
  df.new <- peopleSurvived(sex,age,class,survived)
  peopleAlive <- nrow(df.new[df.new$Survived==survived,])/nrow(ship)
  peopleAlive
}

percPeopleSurivied('Male','Adult','2nd','No')


#function to compare age & 3rd class male survival rates

#child    
peopleSurvived('Male','Child','3rd','Yes')
percPeopleSurivied('Male','Child','3rd','Yes')

#Adult
peopleSurvived('Male','Adult','3rd','Yes')
percPeopleSurivied('Male','Adult','3rd','Yes')

#function to compare age & 1st class female survival rates

#Child
peopleSurvived('Female','Child','1st','Yes')
percPeopleSurivied('Female','Child','1st','Yes')

#Adult
peopleSurvived('Female','Adult','1st','Yes')
percPeopleSurivied('Female','Adult','1st','Yes')

#calculate some rules (clusters) for the titanic dataset
#support factor 0.01

ruleSet <- apriori(ship,parameter=list(support=0.01,confidence=0.5),appearance=list(default="lhs",rhs=("Survived=Yes")))
#Summary of the plot
summary(ruleSet)
#visualize the plot
plot(ruleSet)

goodRules <- ruleSet[quality(ruleSet)$lift > 2 ]
goodRules <- sort(goodRules,by='lift',decreasing=T)
goodRules

inspect(ruleSet)

#support factor 0.005
ruleSet2 <- apriori(ship,parameter=list(support=0.005,confidence=0.5),appearance=list(default="lhs",rhs=("Survived=Yes")))
#Summary of the plot
summary(ruleSet2)
#visualize the plot
plot(ruleSet2)

goodRules2 <- ruleSet2[quality(ruleSet2)$lift > 2 ]
goodRules2 <- sort(goodRules2,by='lift',decreasing=T)
goodRules2

inspect(ruleSet2)

#After inspecing the goodRules, I found 2 interesting rules with relevant lift and confidence outputs
# The most interesting rules here are:
#{Class=2nd,Sex=Female,Age=Adult} => {Survived=Yes} 0.03634711 0.8602151  2.662916  80  
#{Class=1st,Sex=Female,Age=Adult} => {Survived=Yes} 0.06360745 0.9722222  3.009650 140 

#3)	Pick the 3 most interesting & useful rules.
#3 good and most interesting Rules I shortlisted:
#{Class=2nd,Sex=Female,Age=Child}  => {Survived=Yes} 0.005906406 1.0000000  3.095640  13  
#{Class=2nd,Sex=Female,Age=Adult}  => {Survived=Yes} 0.036347115 0.8602151  2.662916  80  
#{Class=1st,Sex=Female,Age=Adult}  => {Survived=Yes} 0.063607451 0.9722222  3.009650 140  

#4)	How does this compare to the descriptive analysis we did on the same dataset? 

#From descriptive analysis, I got to know that out of overall population
#there are 4.9% children and 23.35393% females 

#Of the above percentage of child and females found, 
#52.29358% children survived and 73.19149% females survived and they were major survivors

#Arules and apriori suggested 3 good rules which is in compliance with our descriptive analysis
#Apriori suggested that major survivors are : 2nd class females(both adults and child) and 1st class Female Adults.

#Therefore, both analysis strongly complement each other



