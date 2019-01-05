library(caret)
library(GGally)
library(dplyr)
library(ggplot2)

cobra = read.csv('COBRA-YTD2017.csv')
head(cobra)
str(cobra)
summary(cobra)

cobra = transform(cobra, loc_type=as.numeric(loc_type))
cobra = transform(cobra, MaxOfnum_victims=as.numeric(MaxOfnum_victims))

cobra[!complete.cases(cobra$loc_type),'loc_type'] = mean(cobra$loc_type, na.rm = T)
cobra[!complete.cases(cobra$MaxOfnum_victims),'MaxOfnum_victims'] = mean(cobra$MaxOfnum_victims, na.rm = T)

#select numeric features
cobraNum = cobra %>% select('MI_PRINX','offense_id','beat','MinOfucr','MaxOfnum_victims','loc_type','x','y')
#correlation Matrix
cor(cobraNum)
#Correlation HeatMap
ggcorr(cobraNum)
#1. x - y
#2. minOfucr - x
#3. minOfucr - y
#4. minOfucr - beat
#5. minOfucr - offense_id

crimes=c("LARCENY-FROM VEHICLE", "LARCENY-NON VEHICLE","AUTO THEFT", "BURGLARY-RESIDENCE", "AGG ASSAULT",
           "ROBBERY-PEDESTRIAN","BURGLARY-NONRES","RAPE","ROBBERY-COMMERCIAL","ROBBERY-RESIDENCE","HOMICIDE")
ggplot(cobra, aes(x = UC2.Literal, fill=UC2.Literal))+ geom_bar()  + scale_x_discrete(limits = crimes) +
  labs(title = "Distribution of Crimes", x='Crimes', y="# of crimes")
