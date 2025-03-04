#Exercise 7
#Michael Manolakis - Econ 256 Data Vis - CRN 86179
#3 March 2025

#Load necessary library
library(tidyverse)
library(sf)
#set working directory; checker for dependent directory
if (Sys.info()['sysname'] == "Windows") {
  if (Sys.info()['nodename'] == "DegreeLaptop" || Sys.getenv('USERNAME') == "Degree Laptop") { 
    setwd("C:/Users/Degree Laptop/Desktop/Spring 2025/3 - Econ 256 (Data Vis)/exercise7")} 
  else if (Sys.info()['nodename'] == "Michael" || Sys.getenv('USERNAME') == "Michael") { 
    setwd("C:/Users/Michael/Desktop/Spring 2025/3 - Econ 256/R Studio Files/Econ256/exercise7")}
} else if (Sys.info()['sysname'] == "Linux") {
  setwd("~/Desktop/R Files/Econ256")
} else if (Sys.info()['sysname'] == "Darwin") {
  setwd("~/Desktop/Spring 2025/3 - Econ 256")}
#Last line above is for macOS

#Load Data
results<-read_csv("election2020.csv")
counties<-read_sf(dsn="Counties", layer="Counties")

#Removing double counting
results<-results %>%
  mutate(winner=ifelse(Biden > Trump, "Biden", "Trump"))

#Remove Alaska and Hawaii
results<-results %>%
  filter(state!="ALASKA" & state!="HAWAII")

#Ensure counties data also excludes Alaska & Hawaii, remove redundant zero
counties<-counties %>%
  filter(STATE_NAME!="Alaska" & STATE_NAME!="Hawaii") %>%
  mutate(FIPS = str_remove(FIPS,"^0"))

#Convert FIPS in results to character for matching
results<-results %>%
  rename(FIPS=county_fips) %>%
  mutate(FIPS=as.character(FIPS))

#Ensure both datasets have FIPS in the same format
counties<-counties %>%
  mutate(FIPS=as.character(as.numeric(FIPS)))

results <- results %>%
  mutate(FIPS=as.character(as.numeric(FIPS)))

#Perform the join (keeping counties first to retain spatial features)
merged<-left_join(counties, results, by="FIPS")

#Plot the map
ggplot(data = merged, aes(fill=winner)) +
  geom_sf() +
  theme_void() +
  scale_fill_manual(values=c("#083A90", "#E81B23")) +
  labs(fill="Winning Candidate")

#saving the map as a png
ggsave("Manolakis-map.png",height=7,width=10,bg="white")
