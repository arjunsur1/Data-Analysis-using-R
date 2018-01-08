install.packages("sqldf")
install.packages("RJSONIO")
install.packages("stringr")
install.packages("RCurl")
install.packages("rjson")
install.packages('data.table')

library(sqldf)
library(RJSONIO)
library(stringr)
library(Rcurl)
library(rjson)
library(data.table)

# reading JSON dataset
apiResult<- getURL('https://data.maryland.gov/api/views/pdvh-tf2u/rows.json?accessType=DOWNLOAD')
results <- fromJSON(apiResult)
str(results)
results

results2<- results[2]
results2
length(results2)
rowLength<- length(results2[[1]])

# removing NA values

for (i in 1:rowLength){
  data_file<- results2[[1]][[i]]
  data_file[sapply(data_file, is.null)]<-NA
  results2[[1]][[i]]<-data_file
}

# converting JSON into data frame
df<- data.table::rbindlist(results2[[1]])
df

df<- df[,-1:-8]
df

# defining column names

namesOfColumns <- c("CASE_NUMBER","BARRACK","ACC_DATE","ACC_TIME","ACC_TIME_CODE","DAY_OF_WEEK","ROAD","INTERSECT_ROAD","DIST_FROM_INTERSECT","DIST_DIRECTION","CITY_NAME","COUNTY_CODE","COUNTY_NAME","VEHICLE_COUNT","PROP_DEST","INJURY","COLLISION_WITH_1","COLLISION_WITH_2")
colnames(df)<-namesOfColumns
df

# Accidents on Sunday
accidentsSunday<- sqldf("select count(*) from df where DAY_OF_WEEK like '%SUNDAY%' ")
accidentsSunday

# Accidents with Injuries
accidentInjury<- sqldf("select count(*) from df where INJURY='YES' ")
accidentInjury

# Days of Accidents by Injuries
accidentInjuryDay<- sqldf("select DAY_OF_WEEK, count(*) from df where INJURY='YES' GROUP BY DAY_OF_WEEK ")
accidentInjuryDay

# Accidents on Sunday tapply
accidents_Sunday <- tapply(df$DAY_OF_WEEK,as.factor(df$DAY_OF_WEEK),length)
accidents_Sunday[4]


# Accidents with Injuries tapply
injury_Count <- tapply(df$INJURY,as.factor(df$INJURY),length)
injury_Count['YES']

# Days of Accidents by Injuries tapply
day <- function(x){
  
  injuries <- length(grep("YES",x))
  return (injuries)
}

accident_Injury_Day <- tapply(df$INJURY,df$DAY_OF_WEEK, day)
accident_Injury_Day


