#Exercise 5
#Michael Manolakis - Econ 256 Data Vis - CRN 86179
#9 May 2025

#Load necessary library
library(tidyverse)
#set working directory; checker for dependent directory
if (Sys.info()['sysname'] == "Windows") {
  if (Sys.info()['nodename'] == "DegreeLaptop" || Sys.getenv('USERNAME') == "Degree Laptop") { 
    setwd("C:/Users/Degree Laptop/Desktop/Spring 2025/3 - Econ 256 (Data Vis)/exercise5")} 
  else if (Sys.info()['nodename'] == "Michael" || Sys.getenv('USERNAME') == "Michael") { 
    setwd("C:/Users/Michael/Desktop/Spring 2025/3 - Econ 256/R Studio Files/Econ256/exercise5")}
} else if (Sys.info()['sysname'] == "Linux") {
  setwd("~/Desktop/R Files/Econ256/exercise5")
} else if (Sys.info()['sysname'] == "Darwin") {
  setwd("~/Desktop/Spring 2025/3 - Econ 256")}
#Last line above is for macOS

#loading csv files
IMF_GDP<-read_csv('imf_gdp.csv')
MedalCount<-read_csv('medalcount2020.csv')
Pop_UN<-read_csv('UNpopulations.csv')

IMF_GDP<-rename(IMF_GDP,'GDP in Billions'=gdp_billions)
Pop_UN<-rename(Pop_UN,'Population in Millions'=population_mils)

CountryStat<-left_join(MedalCount,IMF_GDP,by='country')
CountryStat<-left_join(CountryStat,Pop_UN,by='country')
#Missing data; No population for Grenada, no GDP for Syria

##Why does using inner join() result in a data set with fewer observations as 
##compared to left join()?
#Using inner_join() results in fewer observations because it only keeps rows 
#that exist in both datasets. left_join() keeps all rows from the left dataset, 
#even if there’s no match, filling missing values with NA.

CountryStat<-CountryStat%>%
  mutate(`GDP per Capita`=(`GDP in Billions`*1000)/`Population in Millions`,
         `Total Medals`=gold+silver+bronze,
         `Medals per Millions of People`=`Total Medals`/`Population in Millions`)

# Scatter plot: GDP per Capita vs. Medals per Million People
ggplot(CountryStat, aes(x=`GDP per Capita`, y=`Medals per Millions of People`))+
  geom_point(color="navy")+  # Blue points with transparency
  labs(title="Medals per Million vs. GDP per Capita",
       x="GDP per Capita (USD)",
       y="Medals per Million People")+
  theme_minimal()+
  scale_y_continuous(limits=c(0,4.5), breaks=seq(0,5,1))+
  scale_x_continuous(breaks=seq(0,90000,15000))+
  #95% Confidence interval Regression line
  geom_smooth(method='lm',se=TRUE,color='black')+
  #adding in r^2 value
  annotate('text',x=5000,y=4.2,label=paste0('R²=',round(summary(lm(
    `GDP per Capita`~`Medals per Millions of People`,data=CountryStat))
    $r.squared,4)),size=5,hjust=0)


# Count countries with GDP per Capita < $10,000 AND >3 Medals per Million People
LowGDPHighMedal<-CountryStat%>%
  filter(`GDP per Capita` <= 10000|is.na(`GDP per Capita`),
         `Medals per Millions of People`>=3)

##How many countries have a GDP per person below $10,000 AND won more than 3 
##medals per million people?
#With data provided, just Jamaica, however if Bermuda has less than $10k GDP
#per capita, then Bermuda would also be included. So at this time just 1 country
