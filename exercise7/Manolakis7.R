#Exercise 7
#Michael Manolakis - Econ 256 Data Vis - CRN 86179
#27 Jan 2025

#Load necessary library
library(tidyverse)
#set working directory; checker for dependent directory
if (Sys.info()['sysname'] == "Windows") {
  setwd("C:/Users/Michael/Desktop/Spring 2025/3 - Econ 256/R Studio Files/Econ256/exercise7")
} else if (Sys.info()['sysname'] == "Linux") {
  setwd("~/Desktop/Spring 2025/3 - Econ 256/R Studio Files/exercise7")
} else if (Sys.info()['sysname'] == "Darwin") {
  setwd("~/Desktop/Spring 2025/3 - Econ 256/R Studio Files/exercise7")}
#Last line above is for macOS


