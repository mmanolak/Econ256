#Exercise 13
#Michael Manolakis - Econ 256 Data Vis - CRN 86179
#14 March 2025

#Load necessary library
library(tidyverse)
library(sf)
library(leaflet)

if (Sys.info()['sysname'] == "Windows") {
  if (Sys.info()['nodename'] == "DegreeLaptop" || Sys.getenv('USERNAME') == "Degree Laptop") { 
    setwd("C:/Users/Degree Laptop/Desktop/Spring 2025/3 - Econ 256 (Data Vis)/exercise13")} 
  else if (Sys.info()['nodename'] == "Michael" || Sys.getenv('USERNAME') == "Michael") { 
    setwd("C:/Users/Michael/Desktop/Spring 2025/3 - Econ 256/R Studio Files/Econ256/exercise13")}
} else if (Sys.info()['sysname'] == "Linux") {
  setwd("~/Desktop/R Files/Econ256/exercise13")
} else if (Sys.info()['sysname'] == "Darwin") {
  setwd("~/Desktop/Spring 2025/3 - Econ 256")}
#Last line above is for macOS

l2018<-read_csv('listings_2018.csv')
oahu<-read_sf(dsn='oahu', layer='oahu')

#Plot 2018 Air BnB Listings
ggplot()+
  geom_sf(data=oahu, fill="lightgray", color="black")+
  geom_point(data=l2018, aes(x=longitude, y=latitude), color="#ed6464", size=1.5, alpha=0.5)+
  theme_void()+
  ggtitle("Air BnB Listings in Oahu (2018)") +
  labs(caption="Data sources: US Census Bureau and Inside Airbnb")

#Convert to sf object with CRS
l2018sf<-st_as_sf(x=l2018, coords=c("longitude", "latitude"), crs=4326)

#Interactive map with tooltips
leaflet(l2018sf) %>%
  addProviderTiles("Esri.WorldGrayCanvas") %>%
  addCircles(popup = ~name)
