#Exercise 12
#Michael Manolakis - Econ 256 Data Vis - CRN 86179
#14 March 2025

#Load necessary library
library(tidyverse)
library(plotly)

if (Sys.info()['sysname'] == "Windows") {
  if (Sys.info()['nodename'] == "DegreeLaptop" || Sys.getenv('USERNAME') == "Degree Laptop") { 
    setwd("C:/Users/Degree Laptop/Desktop/Spring 2025/3 - Econ 256 (Data Vis)/exercise12")} 
  else if (Sys.info()['nodename'] == "Michael" || Sys.getenv('USERNAME') == "Michael") { 
    setwd("C:/Users/Michael/Desktop/Spring 2025/3 - Econ 256/R Studio Files/Econ256/exercise12")}
} else if (Sys.info()['sysname'] == "Linux") {
  setwd("~/Desktop/R Files/Econ256/exercise12")
} else if (Sys.info()['sysname'] == "Darwin") {
  setwd("~/Desktop/Spring 2025/3 - Econ 256")}
#Last line above is for macOS

read_csv("vaccinations.csv") %>%
  plot_ly(x = ~date, y = ~sharefullyvac, split = ~geog,
        type = "scatter", mode = "lines") %>%
  layout(title = "Share of Population Fully Vaccinated Over Time",
         xaxis = list(title = "Date"),
         yaxis = list(title = "Percent Fully Vaccinated"))


