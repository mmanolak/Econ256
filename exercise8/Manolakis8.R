#Exercise 8
#Michael Manolakis - Econ 256 Data Vis - CRN 86179
#3 March 2025

#Load necessary library
library(tidyverse)
library(sf)

#set working directory; checker for dependent directory
if (Sys.info()['sysname'] == "Windows") {
  if (Sys.info()['nodename'] == "DegreeLaptop" || Sys.getenv('USERNAME') == "Degree Laptop") { 
    setwd("C:/Users/Degree Laptop/Desktop/Spring 2025/3 - Econ 256 (Data Vis)/exercise8")} 
  else if (Sys.info()['nodename'] == "Michael" || Sys.getenv('USERNAME') == "Michael") { 
    setwd("C:/Users/Michael/Desktop/Spring 2025/3 - Econ 256/R Studio Files/Econ256/exercise8")}
} else if (Sys.info()['sysname'] == "Linux") {
  setwd("~/Desktop/R Files/Econ256/exercise8")
} else if (Sys.info()['sysname'] == "Darwin") {
  setwd("~/Desktop/Spring 2025/3 - Econ 256")}
#Last line above is for macOS

lis16<-read_csv('listings_2016.csv')
lis18<-read_csv('listings_2018.csv')
oahu<-read_sf(dsn='oahu', layer='oahu')

#Plot 2016 Air BnB Listings
ggplot()+
  geom_sf(data=oahu, fill="lightgray", color="black")+
  geom_point(data=lis16, aes(x=longitude, y=latitude), color="#ed6464", size=1.5, alpha=0.5)+
  theme_void()+
  ggtitle("Air BnB Listings in Oahu (2016)")+
  labs(caption="Data sources: US Census Bureau and Inside Airbnb")

#Plot 2018 Air BnB Listings
ggplot()+
  geom_sf(data=oahu, fill="lightgray", color="black")+
  geom_point(data=lis18, aes(x=longitude, y=latitude), color="#ed6464", size=1.5, alpha=0.5)+
  theme_void()+
  ggtitle("Air BnB Listings in Oahu (2018)") +
  labs(caption="Data sources: US Census Bureau and Inside Airbnb")

ggsave("Manolakis-map2016.png")
ggsave("Manolakis-map2018.png")

#Did the number of Air BnB listings on Oahu increase or decrease from 2016 to 
#2018? What is one neighborhood where there was a significant change in the 
#number of listings?

#There was a visible overall increase in Air BnB listings in 2018 from 2016
#Areas that had notable increases would be in central Oahu, and Ewa to Kapolei area. 