#Exercise 4
#Michael Manolakis - Econ 256 Data Vis - CRN 86179
#12 February 2025

#set working directory; checker for dependent directory
if (Sys.info()['sysname'] == "Windows") {
  if (Sys.info()['nodename'] == "DegreeLaptop" || Sys.getenv('USERNAME') == "Degree Laptop") { 
    setwd("C:/Users/Degree Laptop/Desktop/Spring 2025/3 - Econ 256 (Data Vis)/exercise4")} 
  else if (Sys.info()['nodename'] == "Michael" || Sys.getenv('USERNAME') == "Michael") { 
    setwd("C:/Users/Michael/Desktop/Spring 2025/3 - Econ 256/R Studio Files/Econ256/exercise4")}
} else if (Sys.info()['sysname'] == "Linux") {
  setwd("~/Desktop/R Files/Econ256/exercise4")
} else if (Sys.info()['sysname'] == "Darwin") {
  setwd("~/Desktop/Spring 2025/3 - Econ 256")}
#Last line above is for macOS

#install.packages(c("ggplot2", "dplyr", "tidyverse"))
library(tidyverse)

mydata<-read_csv("Puetro_Rico_Income+Education.csv")
#Removing unnecessary categories from mydata into mydata2
mydata2<-select(mydata,Geo_FIPS,Geo_NAME,starts_with("SE"))
#Renaming categories from mydata2 into mydata3
mydata3<-rename(mydata2, "Population"=SE_A00001_001,
                "Median Income"=SE_A14006_001,
                "Population Over 25"=SE_A12002_001,
                "Less than High School"=SE_A12002_002,
                "High School Grad"=SE_A12002_003,
                "Some College"=SE_A12002_004,
                "Bachelor Grad"=SE_A12002_005,
                "Masters Grad"=SE_A12002_006,
                "Professional Degree"=SE_A12002_007,
                "PhD"=SE_A12002_008)

#line to remove municipio from the names of the counties/areas in Puerto Rico
mydata3<-mydata3%>%
  mutate(Geo_NAME=str_replace(Geo_NAME, regex(" municipio", 
                                              ignore_case=TRUE), ""))

mydata3<-mydata3%>%
  mutate(Bach.Per=`Bachelor Grad`/`Population Over 25`,
         Mas.Per=`Masters Grad`/`Population Over 25`,
         Pro.Per=`Professional Degree`/`Population Over 25`,
         PhD.Per=PhD/`Population Over 25`)

summary(mydata3)

mydata3<- mydata3%>%
  mutate(AtLeastBach.Per=Bach.Per+Mas.Per+Pro.Per+PhD.Per)

#scatter plot for median income vs bachelor's
ggplot(mydata3, aes(x=`Median Income`, y=AtLeastBach.Per)) +
  geom_point(color="Navy")+
  labs(title = "Puetro Rico-Income vs Bachelor's (2023)",
       x = "Median Household Income",
       y = "At Least a Bachelor's Degree")+
  theme_minimal()+
  #adding in r^2 value
  annotate('text',x=30000,y=0.48,label=paste0('R²=',round(summary(lm(
    `Median Income`~AtLeastBach.Per,data=mydata3))$r.squared,4)),size=5,hjust=0)+ 
  #adding regression line with 95% confidence interval
  geom_smooth(method='lm',se=TRUE,color='black')

# Create a summary table with the highest and lowest counties for each category
education_summary<-bind_rows(
  mydata3%>% 
    arrange(desc(Bach.Per))%>%
    slice(1)%>% mutate(Category="Highest Bachelor's"),
  mydata3%>% 
    arrange(Bach.Per)%>% 
    slice(1)%>% mutate(Category="Lowest Bachelor's"),
  mydata3%>%
    arrange(desc(Mas.Per))%>% 
    slice(1)%>% mutate(Category = "Highest Master's"),
  mydata3%>% 
    arrange(Mas.Per)%>% 
    slice(1)%>% mutate(Category = "Lowest Master's"),
  mydata3%>% 
    arrange(desc(Pro.Per))%>% 
    slice(1)%>% mutate(Category = "Highest Professional"),
  mydata3%>% 
    arrange(Pro.Per)%>% 
    slice(1)%>% mutate(Category = "Lowest Professional"),
  mydata3%>% 
    arrange(desc(PhD.Per))%>% 
    slice(1)%>% mutate(Category = "Highest PhD"),
  mydata3%>% 
    arrange(PhD.Per)%>% 
    slice(1)%>% mutate(Category = "Lowest PhD"))%>% 
  select(Category, Geo_NAME, Bach.Per, Mas.Per, Pro.Per, PhD.Per)

max_bach_share <- max(mydata3$AtLeastBach.Per, na.rm = TRUE)
min_bach_share <- min(mydata3$AtLeastBach.Per, na.rm = TRUE)

# Print the summary table
print(education_summary)

#please switch to plots to see the graph, assuming you ran all the code at once
#
#For the education variable you just created, what is the maximum value among
#your counties? what is the minimum value?
#The highest percentages of Bachelor's, Master's, and Professional degrees are 
#in Guaynabo.
#The highest percentage of PhDs is in Vieques.
#The lowest Bachelor's share is in Vieques, while Master's and PhD lowest shares 
#are in Culebra.
#
#What county in your state has the highest share of people with at least a 
#bachelor’s degree?
# The county with the most Bachelor’s degree holders (not percent) is San Juan.
