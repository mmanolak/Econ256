#Exercise 11
#Michael Manolakis - Econ 256 Data Vis - CRN 86179
#4 April 2025

#Load necessary library
library(tidyverse)

if (Sys.info()['sysname'] == "Windows") {
  if (Sys.info()['nodename'] == "DegreeLaptop" || Sys.getenv('USERNAME') == "Degree Laptop") { 
    setwd("C:/Users/Degree Laptop/Desktop/Spring 2025/3 - Econ 256 (Data Vis)/exercise11")} 
  else if (Sys.info()['nodename'] == "Michael" || Sys.getenv('USERNAME') == "Michael") { 
    setwd("C:/Users/Michael/Desktop/Spring 2025/3 - Econ 256/R Studio Files/Econ256/exercise11")}
} else if (Sys.info()['sysname'] == "Linux") {
  setwd("~/Desktop/R Files/Econ256/exercise11")
} else if (Sys.info()['sysname'] == "Darwin") {
  setwd("~/Desktop/Spring 2025/3 - Econ 256")}
#Last line above is for macOS


# Load and create variables
pop <- read_csv("populations.csv") %>%
  mutate(
    neworleans = ifelse(city == "New Orleans", 1, 0),
    poststorm = ifelse(year > 2005, 1, 0),
    interaction = neworleans * poststorm)

#Run Difference-in-Differences regression & view regression summary
summary(lm(population ~ neworleans + poststorm + interaction, data = pop))

#What is your estimate of how many people left New Orleans because of Katrina?
#The Estimate of people that left New Orleans is about 147,000 people

#Graph population over time by city
ggplot(data = pop, aes(x = year, y = population, color = city))+
  geom_line(size = 1.2)+
  labs(title = "Population Over Time by City",
       x = "Year", y = "Population", color = "City")+
  scale_y_continuous(labels = scales::comma)+
  theme_minimal()

#Using ggplot, create a graph that shows population over time, with one line for
#each city. Is the parallel trend assumption reasonable here?
#The trend is not parallel, as the population of New Orleans lowered when 
#compared to Shreveport.