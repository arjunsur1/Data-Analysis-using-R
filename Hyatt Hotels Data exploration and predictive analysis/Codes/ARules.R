
###############################################
############# Associative Rules ##############
###############################################

library("arules")
library("arulesViz")

frame.Rules = mainDataFrame
frame.Rules.A = frame.Rules[,c(1:10,12,20,23,24,25,26,27,28)]

frame.Rules.B = frame.Rules.A [frame.Rules.A$Brand == "Hyatt Regency",]

frame.Rules.C = frame.Rules.B[,c(4:7,10,12,13,14,15)]
frame.Rules.C = frame.Rules.C[,-8]
for (i in 1:8){
  frame.Rules.C[,i] = discretize(frame.Rules.C[,i])
}

############Promoter Rules############
ruleSet.1 = apriori(frame.Rules.C,parameter=list(support=0.3,confidence=0.5),
                    appearance=list(rhs='NPS_Type=Promoter',default='lhs'))
summary(ruleSet.1) 

rules.good.filtered.Pro = ruleSet.1[quality(ruleSet.1)$lift < 1.25 & quality(ruleSet.1)$lift > 1.23]
inspect(rules.good.filtered.Pro)

rules.good.filtered.Pro1 = ruleSet.1[quality(ruleSet.1)$lift < 1.25 & quality(ruleSet.1)$lift > 1.1]
plot(rules.good.filtered.Pro1, measure=c("support","confidence"), shading="lift")


############Passive Rules############
ruleSet.2 = apriori(frame.Rules.C,parameter=list(support=0.005,confidence=0.5),
                    appearance=list(rhs='NPS_Type=Passive',default='lhs'))
summary(ruleSet.2) 

rules.good.filtered.Pas = ruleSet.2[quality(ruleSet.2)$lift < 2.5 & quality(ruleSet.2)$lift > 2.45]
inspect(rules.good.filtered.Pas)
ruleSet.2a = apriori(frame.Rules.C,parameter=list(support=0.005,confidence=0.4),
                     appearance=list(rhs='NPS_Type=Passive',default='lhs'))
summary(ruleSet.2a) 
rules.good.filtered.Pas1 = ruleSet.2a[quality(ruleSet.2a)$lift < 2.5 & quality(ruleSet.2a)$lift > 2.1]
plot(rules.good.filtered.Pas1, measure=c("support","confidence"), shading="lift")

############Detractors Rules############
ruleSet.3 = apriori(frame.Rules.C,parameter=list(support=0.01,confidence=0.5),
                    appearance=list(rhs='NPS_Type=Detractor',default='lhs'))
summary(ruleSet.3) 

rules.good.filtered.Det = ruleSet.3[quality(ruleSet.3)$lift < 6.32 & quality(ruleSet.3)$lift > 6.3]
inspect(rules.good.filtered.Det)

rules.good.filtered.Det1 = ruleSet.3[quality(ruleSet.3)$lift < 6.32 & quality(ruleSet.3)$lift > 6.0]
plot(rules.good.filtered.Det1, measure=c("support","confidence"), shading="lift")


