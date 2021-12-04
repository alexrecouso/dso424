# install tidycensus library
# the load it
library(tidycensus)
library(readr)
library(tidyverse)
library(plotly)

# Install/add census API to you R environment:
?census_api_key
census_api_key(key=".........")
census_api_key(key=read_lines("censusAPI.txt"))

# we can access acs1, acs3, acs5, sf1 and sf3 data
# acs1(3,5) = 1(3,5) year american community survey data
# sf1 and sf3 = decenial census data (sf = summary files)

# What are the variables in acs that can be imported in R?

load_variables(year=2019,dataset="acs1") %>% view()
load_variables(year=2010,dataset="sf1") %>% view()


# Household income by state
?get_acs

acs.variables=c(MedianIncome="B19013_001",
                MedianRent="B25064_001")

data = get_acs(geography="state",variables=acs.variables)
# moe = margin of error
?get_acs

# present results not ina  long format (tidy format)
# butin a wide format
get_acs(geography="state",variables=acs.variables,output = "wide")

# explore household_income
data %>%  head
data %>%  tail

# household income by county
data = get_acs(geography="state",variables=acs.variables[1],
               geometry = TRUE)

data %>% head

# How to get data for different years?
# let's use purrr package (contains functions for analyzing data stored
# in lists)

# Use  function map_dfr to iterate through different years and 
# present teh results in a data frame

?map_dfr

# define period for which you want data from acs:
start_year = 2005
end_year = 2019
years = end_year : start_year
# convert vector years to list years
years = as.list(years)
names(years) = as.character(end_year : start_year)


rent = map_dfr(years,~get_acs(geography="state",
                       variables=acs.variables[2],
                      year =.x,survey="acs1"),.id="years")

# Plot rents by year and by state
rent %>%  head
# convert years in rents table to numeric:
rent$years = as.numeric(rent$years)
rent %>% ggplot(aes(x = years, y = estimate,color=NAME)) + 
  geom_line() + geom_point() + theme_bw()

# To make graph interactive we need plotly library

p = rent %>% ggplot(aes(x = years, y = estimate,color=NAME)) + 
  geom_line() + geom_point() + theme_bw()

ggplotly(p)


