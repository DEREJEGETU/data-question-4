#Installing  packages 
install.packages("tidyverse")
library(tidyverse)

install.packages("dplyr")
library(dplyr)

install.packages("purrr")
library(purrr)

install.packages("ggplot2")
library(ggplot2)

install.packages("readr")
library(readr)

install.packages("readxl")
library(readxl)

install.packages("stringr")
library(stringr)

install.packages("lubridate")
library("lubridate")

install.packages("ggmap")

# checking if each packages are installed and returs a logical value.
any(grepl("purrr",  installed.packages()))

any(grepl("readxl",  installed.packages()))

# Reading the data sets


#read_excel(path, sheet = NULL, range = NULL, col_names = TRUE,
    #       col_types = NULL, na = "", trim_ws = TRUE, skip = 0, n_max = Inf,
    #       guess_max = min(1000, n_max))

# Importing,filtering by state(TN)

zip_code<-read_excel("data/zip_code_database.xlsx")
View(zip_code)

TN_zip<-zip_code %>% 
select(state,zip,county,latitude,longitude,irs_estimated_population_2014) %>% 
  filter(zip_code$state=="TN")

View(TN_zip)

glimpse(TN_zip)

summary(TN_zip)

any(is.na(TN_zip))
sum(is.na(TN_zip))

is.na(TN_zip$state)
sum(is.na(TN_zip$state))

is.na(TN_zip$zip)
sum(is.na(TN_zip$zip))

is.na(TN_zip$county)
sum(is.na(TN_zip$county))

#which(is.na(TN_zip$county))
#sum(complete.cases(TN_zip))

TN_zip %>% 
filter(is.na(county))



# 38227 zip code's county name is missed and hence we replace it by "Obion"
TN_zip$county[is.na(TN_zip$county)] <- "Obion County"

# we have also observed "0" in irs_estimated_pop and we tried to cross check them 
#we persume that they might be zip codes for companies. 

##ggmap test-the package is installed above
TN_map<-get_map(location=c(lon=-86.660156, lat=35.86011935), zoom=5)
ggmap(TN_map) + geom_point(aes(TN_zip$longitude,TN_zip$latitude), data=TN_zip)

achievement<-read.csv("data/achievement_profile_data_with_CORE.csv")
View(achievement)
glimpse(achievement)
any(is.na(achievement))
sum(is.na(achievement))
names(achievement)
#boxplot(achievement$Science)
#hist(achievement$Science)

irs11<-read_excel("data/IRS_data/11zp43tn.xls", range = cell_rows(4:5))

View(irs11)
names(irs11)
glimpse(irs11)
dim(irs11)
