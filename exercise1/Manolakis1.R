##Exercise 1 for Michael Manolakis, Econ 256, CRN 86179

##local information
library(tidyverse)

#windowsOS checker for dependent directory
if (Sys.info()['sysname'] == "Windows") {
    setwd("C:/Users/Michael/Desktop/Spring 2025/3 - Econ 256/R Studio Files/exercise1")
#LinuxOS Checker
} else if (Sys.info()['sysname'] == "Linux") {
    setwd("~/Desktop/Spring 2025/3 - Econ 256/R Studio Files/exercise1")
  #macOS check
} else if (Sys.info()['sysname'] == "Darwin") {
    setwd("~/Desktop/Spring 2025/3 - Econ 256/R Studio Files/exercise1")}


mydata<-read_csv("states.csv")
summary(mydata)

#Q1 What type of information does this data set contain?
  ##This data set contains information on the following; State, Population, 
  ##Total Housing Units, Median Rent, Median Home Value, Median Income.

#Q2 What is the summary() function doing?
  ##It states out statistical analysis break downs for all relevant data in the
  ##data set with regards to their respective title. It gives minimum, 1st
  ##Quartile, Median, Mean, 3rd Quartile and Maximum values.

#Q3 List the “objects” you defined in this code
  ##The object defined in this code is mydata, in which it uses the state level
  ##data set. 

mydata