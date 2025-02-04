#Exercise 2
#Michael Manolakis - Econ 256 Data Vis - CRN 86179
#24 Jan 2025

#set working directory; checker for dependent directory
if (Sys.info()['sysname'] == "Windows") {
  setwd("C:/Users/Michael/Desktop/Spring 2025/3 - Econ 256/R Studio Files/Econ256/exercise2")
} else if (Sys.info()['sysname'] == "Linux") {
  setwd("~/Desktop/R Files/Econ256/exercise2")
} else if (Sys.info()['sysname'] == "Darwin") {
  setwd("~/Desktop/Spring 2025/3 - Econ 256/R Studio Files/exercise2")}
#Last line above is for macOS

#install.packages('tidyverse')
library(tidyverse)

mydata<-read_csv("states.csv")

#Plot the relationship between median home value and median rent
ggplot(mydata,aes(x=median_home_value,y=median_rent))+
  geom_point()+
  #adding regression line with 95% confidence interval
  geom_smooth(method='lm',se=TRUE,color='black')+
  xlab('Median Home Value')+
  ylab('Median Rent')+
  ggtitle('Median Home Value vs. Median Rent')+
  theme_minimal()+
  #adding in r^2 value
  annotate('text',x=150000,y=1750,label=paste0('R²=',round(summary(lm(
    median_rent~median_home_value,data=mydata))$r.squared,4)),size=5,hjust=0)

#Is there a correlation between these two variables?
#Yes, there is a positive correlation between median home value and median rent.
#States with higher median home values generally have higher median rents.

#Identify and highlight Hawaii on the plot
mydata2<-mutate(mydata,hawaii=as.factor(ifelse(state=="Hawaii",1,0)))

ggplot(mydata2,aes(x=median_home_value,y=median_rent,color=hawaii))+
  geom_point()+
  geom_smooth(method='lm',se=TRUE,color='black')+
  xlab('Median Home Price (USD')+
  ylab('Median Rent (USD)')+
  ggtitle('Median Price vs. Median Rent (Highlighting Hawaii)')+
  theme_minimal()+
  annotate('text',x=150000,y=1750,label=paste0('R²=',round(summary(lm(
    median_rent~median_home_value,data=mydata))$r.squared,4)),size=5,hjust=0)

#Is Hawaii relatively expensive for housing?
#Yes, Hawaii is among the most expensive states for housing, 
#with high median home values and rents.

#Plot the relationship between state population (in millions) and housing 
#units (in millions)
mydata3<-mydata2%>%
  mutate(popm=population/1000000,housm=total_housing_units/1000000)

ggplot(mydata3,aes(x=popm,y=housm,color=hawaii))+
  geom_point()+
  geom_smooth(method='lm',se=TRUE,color='black')+
  xlab('Population (in millions)')+
  ylab('Housing Units (in millions)')+
  ggtitle('State Population vs. Housing Units')+
  theme_minimal()+
  annotate('text',x=5,y=15,label=paste0('R²=',round(summary(lm(
    median_rent~median_home_value,data=mydata))$r.squared,4)),size=5,hjust=0)

#Add a comment: Does a state’s population and number of housing units have a 
#close relationship?
#Yes, there is a strong positive relationship between a state’s population and 
#the number of housing units.

