#Exercise 10
#Michael Manolakis - Econ 256 Data Vis - CRN 86179
#14 March 2025

#Load necessary library
library(tidyverse)

if (Sys.info()['sysname'] == "Windows") {
  if (Sys.info()['nodename'] == "DegreeLaptop" || Sys.getenv('USERNAME') == "Degree Laptop") { 
    setwd("C:/Users/Degree Laptop/Desktop/Spring 2025/3 - Econ 256 (Data Vis)/exercise10")} 
  else if (Sys.info()['nodename'] == "Michael" || Sys.getenv('USERNAME') == "Michael") { 
    setwd("C:/Users/Michael/Desktop/Spring 2025/3 - Econ 256/R Studio Files/Econ256/exercise10")}
} else if (Sys.info()['sysname'] == "Linux") {
  setwd("~/Desktop/R Files/Econ256/exercise10")
} else if (Sys.info()['sysname'] == "Darwin") {
  setwd("~/Desktop/Spring 2025/3 - Econ 256")}
#Last line above is for macOS

cookoff<-read_csv('cookoff.csv')

#Run regression with 'spicy' as the dependent variable
model<-lm(spice~redpeppers+blackpeppers, data=cookoff)

#I expect Red Peppers to have a value of 5 and Black Peppers to have 1
#The P values for both the Black and Red peppers are extremely small,
#statistically significant at the 0.001 level (p < 0.001).


#Display regression summary
summary(model)
