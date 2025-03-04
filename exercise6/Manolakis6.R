#Exercise 6
#Michael Manolakis - Econ 256 Data Vis - CRN 86179
#21 Feb 2025

#Load necessary library
library(tidyverse)
#set working directory; checker for dependent directory
if (Sys.info()['sysname'] == "Windows") {
  if (Sys.info()['nodename'] == "DegreeLaptop" || Sys.getenv('USERNAME') == "Degree Laptop") { 
    setwd("C:/Users/Degree Laptop/Desktop/Spring 2025/3 - Econ 256 (Data Vis)/exercise6")} 
  else if (Sys.info()['nodename'] == "Michael" || Sys.getenv('USERNAME') == "Michael") { 
    setwd("C:/Users/Michael/Desktop/Spring 2025/3 - Econ 256/R Studio Files/Econ256/exercise6")}
} else if (Sys.info()['sysname'] == "Linux") {
  setwd("~/Desktop/R Files/Econ256")
} else if (Sys.info()['sysname'] == "Darwin") {
  setwd("~/Desktop/Spring 2025/3 - Econ 256")}
#Last line above is for macOS

latlon<-read_csv('latlon.csv')
election<-read_csv('election2020.csv')

#rotate long, make tiddy data
rlong<-pivot_longer(election,cols=c('Trump','Biden'), names_to='canidate',
                    values_to='votes')

#filtering out, selecting only the winning county votes
rlong<-rlong %>%
  group_by(county_fips)%>%
  filter(votes==max(votes))

#removing Alaska and Hawaii to make map less spaced out
combo <- inner_join(rlong, latlon, by='county_fips')%>%
  filter(state!='ALASKA', state!='HAWAII')

#statement of labels, loctaions and purposes, relating to size
ggplot(data=combo, aes(x=longitude, y=latitude, color=canidate, size=votes/1000))+
  #adding transparency to make the counties that overlapped more visually appealing
  geom_point(alpha=0.75)+
  theme_void()+
  #republican red and democrat blue
  scale_color_manual(values=c('#083A90', '#E81B23'))+
  labs(color='Canidate', size='Votes (in thousands)')+
  theme(text=element_text(size=12))+
  scale_size_area(max_size=40)

