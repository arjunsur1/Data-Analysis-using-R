misc = read.csv("C:/Users/bhaum/Desktop/Project Files/Data Sets/Misc data Set.csv", header = TRUE, fill =TRUE, sep = ",")

misc$Guest_Room_H [is.na(misc$Guest_Room_H)] = ceiling(mean(misc$Guest_Room_H, na.rm = TRUE))
misc$Tranquility_H [is.na(misc$Tranquility_H)] = ceiling(mean(misc$Tranquility_H, na.rm = TRUE))
misc$Condition_Hotel_H [is.na(misc$Condition_Hotel_H)] = ceiling(mean(misc$Condition_Hotel_H, na.rm = TRUE))
misc$Customer_SVC_H [is.na(misc$Customer_SVC_H)] = ceiling(mean(misc$Customer_SVC_H, na.rm = TRUE))
misc$Staff_Cared_H [is.na(misc$Staff_Cared_H)] = ceiling(mean(misc$Staff_Cared_H, na.rm = TRUE))
misc$Internet_Sat_H [is.na(misc$Internet_Sat_H)] = ceiling(mean(misc$Internet_Sat_H, na.rm = TRUE))
misc$Check_In_H [is.na(misc$Check_In_H)] = ceiling(mean(misc$Check_In_H, na.rm = TRUE))
misc$Checkin_Length_H [is.na(misc$Checkin_Length_H)] = ceiling(mean(misc$Checkin_Length_H, na.rm = TRUE))
misc$e_hy_gss_room_floor_I [is.na(misc$e_hy_gss_room_floor_I)] = 0

misc.A = misc[c(1:21,37:64)]

misc.A = misc.A[! is.na(misc.A$Overall_Sat_H),]


colnames(misc.A)[colSums(is.na(misc.A))>0]

nullToNA <- function(x) {
  x[sapply(x, is.null)] <- NA
  return(x)
}

misc.B = data.frame(lapply(misc.A,nullToNA))

colnames(misc.B)[colSums(is.na(misc.B))>0]
tapply(misc.A$Likelihood_Recommend_H,misc.A$e_hy_gss_room_floor_I, mean)
tapply(misc.A$Likelihood_Recommend_H,misc.A$ROOM_TYPE_CODE_R,mean)


tapply(misc.A$Likelihood_Recommend_H,misc.A$MEMBER_STATUS_R,mean)

########## Check In Length
A = tapply(misc.A$Checkin_Length_H,misc.A$Checkin_Length_H,mean)
B = tapply(misc.A$Likelihood_Recommend_H,misc.A$Checkin_Length_H,mean)
C = tapply(misc.A$Likelihood_Recommend_H,misc.A$Checkin_Length_H,length)
df1 = data.frame(A,B,C)
df1 = df1[1:50,]
ggplot() + geom_line(data= df1,aes(x=A,y=B)) + geom_line(data= df1,aes(x=A,y=C))


misc.Copy = misc.B[-c(1:20,21,48,49)]

str(misc.Copy)
#for (i in 1:7){
#  misc.Copy[,i] = as.integer(misc.Copy[,i])
#}

for (i in 1:36){
  misc.Copy[,i] = discretize(misc.Copy[,i])
}

ruleSet.A = apriori(misc.Copy,parameter=list(support=0.5,confidence=0.5, maxlen = 4),
                    appearance=list(rhs='NPS_Type=Promoter',default='lhs'))
summary(ruleSet.A)
rules.good.filtered.ProA = ruleSet.A[quality(ruleSet.A)$lift < 1.1 & quality(ruleSet.A)$lift > 1.020]
inspect(rules.good.filtered.ProA)
plot(rules.good.filtered.ProA, measure=c("support","confidence"), shading="lift")



ruleSet.B = apriori(misc.Copy,parameter=list(support=0.05,confidence=0.10, maxlen = 4),
                    appearance=list(rhs='NPS_Type=Detractor',default='lhs'))
summary(ruleSet.B)
rules.good.filtered.ProB = ruleSet.B[quality(ruleSet.B)$lift < 1.37 & quality(ruleSet.B)$lift > 1.36]
inspect(rules.good.filtered.ProB)
plot(rules.good.filtered.ProB, measure=c("support","confidence"), shading="lift")

ruleSet.C = apriori(misc.Copy,parameter=list(support=0.05,confidence=0.20, maxlen = 4),
                    appearance=list(rhs='NPS_Type=Passive',default='lhs'))
summary(ruleSet.C)
rules.good.filtered.ProB = ruleSet.C[quality(ruleSet.C)$lift < 1.31 & quality(ruleSet.C)$lift > 1.198]
inspect(rules.good.filtered.ProB)
plot(rules.good.filtered.ProB, measure=c("support","confidence"), shading="lift")
