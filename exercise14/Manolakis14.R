#Exercise 14
#Michael Manolakis - Econ 256 Data Vis - CRN 86179
#16 April 2025

#Load necessary library
library(tidyverse)
library(sf)
options(scipen=9999)

if (Sys.info()['sysname'] == "Windows") {
  if (Sys.info()['nodename'] == "DegreeLaptop" || Sys.getenv('USERNAME') == "Degree Laptop") { 
    setwd("C:/Users/Degree Laptop/Desktop/Spring 2025/3 - Econ 256 (Data Vis)/exercise14")} 
  else if (Sys.info()['nodename'] == "Michael" || Sys.getenv('USERNAME') == "Michael") { 
    setwd("C:/Users/Michael/Desktop/Spring 2025/3 - Econ 256/R Studio Files/Econ256/exercise14")}
} else if (Sys.info()['sysname'] == "Linux") {
  setwd("~/Desktop/R Files/Econ256/exercise14")
} else if (Sys.info()['sysname'] == "Darwin") {
  setwd("~/Desktop/Spring 2025/3 - Econ 256")}
#Last line above is for macOS

read_csv("vanhousing.csv")-> vanhousing
read_sf(dsn="vantracts", layer="vantracts") %>%
  left_join(
    vanhousing %>%
      group_by(tract) %>%
      summarize(medprice = median(price, na.rm = TRUE)), by = "tract"
  ) -> vantracts

#Histogram of house prices
ggplot(data = vanhousing, aes(x = price)) +
  geom_histogram(bins = 50, fill = "steelblue", color = "white") +
  scale_x_continuous(breaks = seq(0, 5000000, by = 1000000),
                     labels = c("0", "1 Mil", "2 Mil", "3 Mil", "4 Mil", "5 Mil")) +
  ggtitle("Distribution of House Prices in Vancouver") +
  xlab("Price") +
  ylab("Count")

#Average price by year and time series plot
vanhousing %>%
  group_by(year_of_sale) %>%
  summarise(mean_price = mean(price, na.rm = TRUE)) %>%
  ggplot(aes(x = year_of_sale, y = mean_price)) +
  geom_line(color = "darkred", size = 1) +
  scale_x_continuous(breaks = seq(2005, 2015, by = 2)) +
  xlab("Year of Sale") +
  ylab("Mean Price") +
  ggtitle("Change in Mean House Price Over Time")

#Correlation matrix
vanhousing %>%
  select(price, Beds, FullBath, `area_sqft`) %>%
  cor(use = "complete.obs") %>%
  round(2) -> cor_matrix

#Compile key results into a list
van_summary <- list(
  correlation_matrix = cor_matrix,
  
  median_price_west_east = vanhousing %>%
    group_by(west) %>%
    summarize(medprice = median(price, na.rm = TRUE)),
  
  median_price_by_beds = vanhousing %>%
    group_by(Beds) %>%
    summarise(median_price = median(price, na.rm = TRUE)) %>%
    arrange(Beds),
  
  top10_largest_homes = vanhousing %>%
    arrange(desc(`area_sqft`)) %>%
    slice_head(n = 10) %>%
    select(`area_sqft`, addressshort, price, Beds),
  
  summary_stats = tibble(
    median_price = median(vanhousing$price, na.rm = TRUE),
    mean_price = mean(vanhousing$price, na.rm = TRUE),
    sd_price = sd(vanhousing$price, na.rm = TRUE),
    median_beds = median(vanhousing$Beds, na.rm = TRUE),
    median_baths = median(vanhousing$FullBath, na.rm = TRUE),
    median_sqft = median(vanhousing$`area_sqft`, na.rm = TRUE)
  ),
  
  regression_beds = summary(lm(price ~ Beds, data = vanhousing)),
  regression_baths = summary(lm(price ~ FullBath, data = vanhousing)),
  regression_full = summary(lm(price ~ Beds + FullBath + west, data = vanhousing))
)
