#Exercise 9
#Michael Manolakis - Econ 256 Data Vis - CRN 86179
#14 March 2025

#Load necessary library
library(tidyverse)
#set working directory; checker for dependent directory
if (Sys.info()['sysname'] == "Windows") {
  if (Sys.info()['nodename'] == "DegreeLaptop" || Sys.getenv('USERNAME') == "Degree Laptop") { 
    setwd("C:/Users/Degree Laptop/Desktop/Spring 2025/3 - Econ 256 (Data Vis)/exercise9")} 
  else if (Sys.info()['nodename'] == "Michael" || Sys.getenv('USERNAME') == "Michael") { 
    setwd("C:/Users/Michael/Desktop/Spring 2025/3 - Econ 256/R Studio Files/Econ256/exercise9")}
} else if (Sys.info()['sysname'] == "Linux") {
  setwd("~/Desktop/R Files/Econ256/exercise9")
} else if (Sys.info()['sysname'] == "Darwin") {
  setwd("~/Desktop/Spring 2025/3 - Econ 256")}
#Last line above is for macOS

airbnb_data <- read_csv("oahulistings.csv")

# Run summary calculations
summary_stats <- tibble(
  total_listings_2016 = nrow(airbnb_data %>% filter(year == 2016)),
  total_listings_2018 = nrow(airbnb_data %>% filter(year == 2018)),
  zip_most_listings = airbnb_data %>%
    filter(year == 2018) %>%
    count(zipcode, sort = TRUE) %>%
    slice(1) %>%
    pull(zipcode),
  zip_most_listings_count = airbnb_data %>%
    filter(year == 2018) %>%
    count(zipcode, sort = TRUE) %>%
    slice(1) %>%
    pull(n),
  num_apartments = airbnb_data %>%
    filter(year == 2018, property_type == "Apartment") %>%
    nrow(),
  num_condos = airbnb_data %>%
    filter(year == 2018, property_type == "Condominium") %>%
    nrow(),
  num_houses = airbnb_data %>%
    filter(year == 2018, property_type == "House") %>%
    nrow(),
  avg_price_2018 = mean(airbnb_data$price[airbnb_data$year == 2018], 
    na.rm = TRUE),
  median_price_2018 = median(airbnb_data$price[airbnb_data$year == 2018], 
    na.rm = TRUE),
  min_price_2018 = min(airbnb_data$price[airbnb_data$year == 2018], na.rm = TRUE),
  max_price_2018 = max(airbnb_data$price[airbnb_data$year == 2018], na.rm = TRUE),
  price_per_person = coef(lm(price ~ accommodates, data = airbnb_data %>% 
    filter(year == 2018)))[2],
  evolve_unique_listings = airbnb_data %>%
    filter(year == 2018, host_name == "Evolve Vacation Rental") %>%
    summarise(unique_listings = n_distinct(id)) %>%
    pull(unique_listings),
  evolve_unique_zipcodes = airbnb_data %>%
    filter(year == 2018, host_name == "Evolve Vacation Rental") %>%
    summarise(unique_zipcodes = n_distinct(zipcode)) %>%
    pull(unique_zipcodes)
)

# Print summary
print(summary_stats)

# Display results in a readable format
cat("\nAirbnb Statistics for Oahu:\n",
    "In 2016, there were", summary_stats$total_listings_2016, 
    "Airbnb listings. By 2018, that number grew to", summary_stats$total_listings_2018, ".\n",
    "The zip code with the most Airbnb listings in 2018 was", 
    summary_stats$zip_most_listings, "with", summary_stats$zip_most_listings_count, "listings.\n",
    "In 2018, there were", summary_stats$num_apartments, "Apartments,", summary_stats$num_condos, 
    "Condominiums, and", summary_stats$num_houses, "Houses listed.\n",
    "The average nightly price in 2018 was $", round(summary_stats$avg_price_2018, 2), 
    ", the median was $", round(summary_stats$median_price_2018, 2), ", and the range was $", 
    summary_stats$min_price_2018, "- $", summary_stats$max_price_2018, ".\n",
    "A regression analysis suggests that price increases by $", 
    round(summary_stats$price_per_person, 2), "for each additional person the unit can accommodate.\n",
    "Evolve Vacation Rental managed", summary_stats$evolve_unique_listings, 
    "unique listings across", summary_stats$evolve_unique_zipcodes, "different zip codes in 2018.\n")
