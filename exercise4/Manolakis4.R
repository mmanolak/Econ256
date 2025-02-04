#Exercise 4
#Michael Manolakis - Econ 256 Data Vis - CRN 86179
#27 Jan 2025

#set working directory; checker for dependent directory
if (Sys.info()['sysname'] == "Windows") {
  if (Sys.info()['nodename'] == "DegreeLaptop" || Sys.getenv('USERNAME') == "Degree Laptop") { 
    setwd("C:/Users/Degree Laptop/Desktop/Spring 2025/3 - Econ 256 (Data Vis)/exercise4")
  } else { 
    setwd("C:/Users/Michael/Desktop/Spring 2025/3 - Econ 256/R Studio Files/Econ256/exercise4")}
} else if (Sys.info()['sysname'] == "Linux") {
  setwd("~/Desktop/R Files/Econ256")
} else if (Sys.info()['sysname'] == "Darwin") {
  setwd("~/Desktop/Spring 2025/3 - Econ 256")}
#Last line above is for macOS


library(tidyverse)
options(scipen=9999)

mydata<-read_csv("R13805626_SL060.csv")
mydata2<-select(mydata,Geo_FIPS,Geo_QName,SE_A00001_001)
mydata3<-mydata2%>%
  filter(Geo_FIPS!=72)%>%
  filter(Geo_FIPS!=11)

ggplot(data=mydata3,mapping=aes(x=SE_A00001_001))+
  geom_histogram()
  




