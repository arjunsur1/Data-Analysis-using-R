# Extracting data
install.packages('reshape2')

library(reshape2)
air<- airquality
air

colnames(air)[colSums(is.na(air))>0]

# Cleaning Data
air$Ozone[is.na(air$Ozone)]<- mean(air$Ozone,na.rm =TRUE)
air$Solar.R[is.na(air$Solar.R)]<- mean(air$Solar.R,na.rm =TRUE)
air$Wind[is.na(air$Wind)]<- mean(air$Wind,na.rm =TRUE)
air$Temp[is.na(air$Temp)]<- mean(air$Temp,na.rm =TRUE)
air$Month[is.na(air$Month)]<- mean(air$Month,na.rm =TRUE)
air$Month[is.na(air$Day)]<- mean(air$Day,na.rm =TRUE)
air

#histogram Ozone
ggplot(air,aes(x=Ozone)) + geom_histogram(binwidth=10,color='black',fill='white')

#histogram Solar.R
ggplot(air,aes(x=Solar.R)) + geom_histogram(bins=10,color='orange',fill='blue')

#histogram Wind 
ggplot(air,aes(x=Wind)) + geom_histogram(binwidth=1,color='black',fill='green')

#histogram Temp
ggplot(air,aes(x=Temp)) + geom_histogram(binwidth=2,color='white',fill='black')

#histogram Month and Day
ggplot(air,aes(x=Month)) + geom_histogram(binwidth=0.5,color='blue',fill='yellow')
ggplot(air,aes(x=Day)) + geom_histogram(binwidth=0.5,color='black',fill='violet')

#box plots
ggplot(air,aes(x=as.factor(0), y=Ozone)) + geom_boxplot()
ggplot(air,aes(x=factor(0), y=round(air$Wind))) + geom_boxplot()

#dates 
air$Date<- paste(air$Month, air$Day, '1973', sep='-')
air

#line plots
#Ozone
ggplot(air,aes(x= Date,y=Ozone, group=1)) +geom_line(color="blue", size=1)
#Solar.R
ggplot(air,aes(x= Date,y=Solar.R, group=1)) +geom_line(color="green", size=1)
#Wind
ggplot(air,aes(x= Date,y=Wind, group=1)) +geom_line(color="red", size=1)
#Temp
ggplot(air,aes(x= Date,y=Temp, group=1)) +geom_line(color="black", size=1)

#line plot combined
g<- ggplot(air,aes(x= Date,y=Ozone, group=1 )) +geom_line(color="blue", size=1 )  
g<-g+ geom_line(aes(x= Date,y=Solar.R, group=1), color="green", size=1)
g<-g+ geom_line(aes(x= Date,y=Wind, group=1), color="red", size=1)
g<-g+ geom_line(aes(x= Date,y=Temp, group=1), color="black", size=1)
g

#Line plot using melt function
g_melt<- ggplot(aql,aes(x= Date,y= climate_value, group= climate_variable, color=climate_variable )) +geom_line()
g_melt

#heat map using melt
aql <- melt(air, id.vars = c('Date', 'Day', 'Month'),
variable.name = "climate_variable", 
value.name = "climate_value")
aql

ggplot(aql, aes(x=Date,y=climate_variable, fill= climate_value)) +
geom_tile() + scale_fill_gradient(low="white", high="red")

#Scatter plot
ggplot(air, aes(x=Wind,y= Temp))+ geom_point(aes(size= Ozone, color= Solar.R)) + geom_text(aes(label=Day, size=3))

#After exploring the data set, I could visualize the ranges of Climate attributes, the amount of variation of data in each variable.     
#With the increase in solar R, temperature also increases.
#Heat map visualizes the data set in the best possible way. 






