#Sisqo Graph
#Michael Manolakis - Econ 256 Data Vis - CRN 86179
#21 Feb 2025

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

#glimpse(billboard)

data1=billboard

billboard_longer<-billboard |> 
  pivot_longer(
    cols=starts_with("wk"), 
    names_to="week", 
    values_to="rank",
    values_drop_na=TRUE
  ) |> 
  mutate(
    week=parse_number(week)
  )

sisqo_songs<-billboard_longer |> 
  filter(artist=="Sisqo")

ggplot(sisqo_songs, aes(x=week, y=rank, group = track, color = track))+
  geom_line(size = 1)+
  scale_y_reverse()+
  scale_color_manual(values=c("red", "dark green", "blue")) +
  labs(title = "Billboard Rankings of Sisqo Songs", x="Week", y="Rank")+
  theme_minimal()
