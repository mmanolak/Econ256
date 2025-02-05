#Exercise 3
#Michael Manolakis - Econ 256 Data Vis - CRN 86179
#27 Jan 2025

#set working directory; checker for dependent directory
if (Sys.info()['sysname'] == "Windows") {
  if (Sys.info()['nodename'] == "DegreeLaptop" || Sys.getenv('USERNAME') == "Degree Laptop") { 
    setwd("C:/Users/Degree Laptop/Desktop/Spring 2025/3 - Econ 256 (Data Vis)/exercise3")} 
  else if (Sys.info()['nodename'] == "Michael" || Sys.getenv('USERNAME') == "Michael") { 
    setwd("C:/Users/Michael/Desktop/Spring 2025/3 - Econ 256/R Studio Files/Econ256/exercise3")}
} else if (Sys.info()['sysname'] == "Linux") {
  setwd("~/Desktop/R Files/Econ256")
} else if (Sys.info()['sysname'] == "Darwin") {
  setwd("~/Desktop/Spring 2025/3 - Econ 256")}
#Last line above is for macOS

#Load necessary library
library(tidyverse)

#Load the dataset
mydata<-read_csv("acs_HI_2022.csv")
#sidenote, the table has ages up to 97 years old, so i was unsure if we needed
#to adjust for ages that were 66 and over or not. in my case i did not

#step 1: Summarize Income in Hawaii
#summary statistics for income
summary(mydata$INCOME)
#median income for Hawaii residents is: $33300

#a histogram for income distribution of Hawaii residents
ggplot(mydata,aes(x=INCOME))+
  geom_histogram(color="black",binwidth=15000,fill='steelblue') +
  ggtitle("Income Distribution of Hawaii Residents")+
  xlab("Income")+
  ylab("Number of Survey Respondents")

#step 2: Income Differences by Education and Age
#persons who graduated college
grads<-mydata%>%filter(COLLEGE==1)
#persons under 30
young<-mydata%>%filter(AGE<30)
#graduates under 30
younggrads<-mydata%>%filter(COLLEGE==1&AGE<30)

#median income for above subgroups
summary(grads$INCOME) 
summary(young$INCOME)
summary(younggrads$INCOME)

#Median income of college grads: $57980
#Median income of people under 30: $12000
#Median income of college grads under 30: $30440

#Step 3: Plot Income vs. Age
#Overall income vs age
ggplot(mydata,aes(x=AGE,y=INCOME))+
  geom_smooth(color="black",binwidth=15000,fill='darkgrey')+
  theme_minimal()+
  ggtitle("Income vs. Age in Hawaii")+
  xlab("Age")+
  ylab("Income")+
  annotate('text',x=50,y=21500,label=paste0('R²=',round(summary(lm(
    AGE~INCOME,data=mydata))$r.squared,4)),size=5,hjust=0)
#age that income overall peaks for is about 50 years old

#Income by age for college graduates
ggplot(grads,aes(x=AGE,y=INCOME))+
  geom_smooth(color="black",binwidth=15000,fill='darkgrey')+
  theme_minimal()+
  ggtitle("Income vs. Age for College Graduates")+
  xlab("Age")+
  ylab("Income")+
  annotate('text',x=60,y=27000,label=paste0('R²=',round(summary(lm(
    AGE~INCOME,data=grads))$r.squared,4)),size=5,hjust=0)
#age that income peaks for college graduates is about 51 years old

#Income by age for non-college graduates
nongrads<-mydata%>%
  filter(COLLEGE==0)

ggplot(nongrads,aes(x=AGE,y=INCOME))+
  geom_smooth(color="black",binwidth=15000,fill='darkgrey')+
  theme_minimal()+
  ggtitle("Income vs. Age for Non-College Graduates")+
  xlab("Age")+
  ylab("Income")+
  annotate('text',x=50,y=11000,label=paste0('R²=',round(summary(lm(
    AGE~INCOME,data=nongrads))$r.squared,4)),size=5,hjust=0)
#age that income peaks for non-graduates is about 50 years old

#Step 4: Calculate the income difference at age 40
#Filter data for age 40 with `group%>%filter(AGE==40)`
#mean and median income difference
average_diff_40<-mean((grads%>%filter(AGE==40))$INCOME,na.rm=TRUE)-
  mean((nongrads%>%filter(AGE==40))$INCOME,na.rm=TRUE)

median_diff_40<-median((grads%>%filter(AGE==40))$INCOME,na.rm=TRUE)-
  median((nongrads%>%filter(AGE==40))$INCOME,na.rm=TRUE)

#At age 40, college graduates based on the median make $43800 more, and based
#and based upon the average make about $39780 more

