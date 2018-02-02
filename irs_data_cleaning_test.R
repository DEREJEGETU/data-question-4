install.packages("maps")
install.packages("mapdata")
install.packages("choroplethr")
install.packages("choroplethrMaps")
install.packages("plotly")
install.packages("ggmap")
install.packages("viridis")

library(maps)
library(mapdata)
library(readxl)
library(tidyverse)
library(plotly)
library(ggplot2)
library(ggmap)
library(viridis)
library(dplyr)


###---Set working directory to location that is GIT compatible---
setwd("C:/Users/kenne/GIT/data-question-4-data-question-4-impact-squad/data")
##--Use to verify working directory-- getwd()


############READ/CLEAN COUNTY POPULATION DATA################
population <- read_excel("County_Population.xlsx")

population$county <- gsub(' [A-z ]*', '' , population$county)

population$county <- sapply(population$county, tolower)

population$county <- gsub("van", "vanburen", population$county, fixed = TRUE)

population$county <- gsub("sullivanburen", "sullivan", population$county, fixed = TRUE)

##View(population)

############READ/CLEAN EDUCATION DATA################

Achievement_Data <- read.csv("achievement_profile_data_with_CORE.csv")

###---Examine the structure of the Education_Data df to locate areas for cleaning and data type casting---
str(Achievement_Data)  

##---Rename columns to be more meaningful and understandable---
names(Achievement_Data) <- c("system", "system_name","Algebra1", "Algebra2", "Biology1", "Chemistry", "Elementary_ENG", "English1", "English2"
                           , "English3", "Elementary_Math", "Elementary_Science", "Enrollment", "Pct_Black", "Pct_Hispanic", "Pct_Native_American", "Pct_EnglishLearners", 
                           "Pct_Disabled_Students", "Pct_Econ_Disadvantaged", "Per_Pupil_Expenditures", "Pct_Blk_Hspnc_NtvAmrcn", "Act_Coposite", "Pct_Chronically_Absent",
                           "Pct_Suspended", "Pct_Expelled", "Pct_Graduated", "Pct_Dropout", "CORE_region")

colnames(Achievement_Data)[2] <- "county"

Achievement_Data$county <- gsub(' [A-z ]*', '' , Achievement_Data$county)

Achievement_Data$county <- gsub('-[A-z ]*', '' , Achievement_Data$county)

Achievement_Data$county <- gsub("\\..*", '' , Achievement_Data$county)

Achievement_Data$county <- sapply(Achievement_Data$county, tolower)

Achievement_Data$county <- gsub("van", "vanburen", Achievement_Data$county, fixed = TRUE)

Achievement_Data$county <- gsub("sullivanburen", "sullivan", Achievement_Data$county, fixed = TRUE)

##View(Achievement_Data$county)
##View(Achievement_Data)
############READ/CLEAN ZIP################
zips_xl <- read_excel("zip_code_database.xlsx")

zips_df <- data.frame(zips_xl) %>%
  select(zip_code = zip, state, county, latitude, longitude) %>%
  filter(state == 'TN') %>%
  select(-state) %>%
  group_by(county) %>%
  mutate(county_lat = (mean(latitude))) %>%
  mutate(county_lon = (mean(longitude)))

##View(zips_df)
###########READ DISTRICT/COUNTY CROSSWALK#################
crosswalk_xl <- read_excel("data_district_to_county_crosswalk.xlsx")

crosswalk_df <- data.frame(crosswalk_xl) %>%
  select(county = County.Name, system = District.Number)

View(crosswalk_df)
###########READ/CLEAN IRS##################
# Create vectors of better column names, leaving blanks for those we don't want to keep (why, yes -- there probably IS a better way...).
column_names_2011 <- c('zip_code','AGI_range','return_count','joint_return_count','paid_preparer_return_count','exemption_count','dependent_count','AGI_amount','salary_and_wages_count','salary_and_wages_amount','taxable_interest_count','taxable_interest_amount','ordinary_dividends_count','ordinary_dividends_amount','','','business_income_count','business_income_amount','farm_income_count','net_capital_gain_count','net_capital_gain_amount','taxable_IRA_distributions_count','taxable_IRA_distributions_amount','pension_and_annuity_income_count','pension_and_annuity_income_amount','unemployment_income_count','unemployment_income_amount','social_security_count','social_security_amount','','','itemized_deductions_count','itemized_deductions_amount','state_and_local_income_tax_count','state_and_local_income_tax_amount','state_and_local_sales_tax_count','state_and_local_sales_tax_amount','','','taxes_paid_count','taxes_paid_amount','mortgage_interest_count','mortgage_interest_amount','charitable_contributions_count','charitable_contributions_amount','taxable_income_count','taxable_income_amount','total_tax_credits_count','total_tax_credits_amount','','','','','','','','','earned_income_credit_count','earned_income_credit_amount','excess_earned_income_credit_count','excess_earned_income_credit_amount','','','','','tax_liability_count','tax_liability_amount','balance_due_count','balance_due_amount','refund_count','refund_amount')
column_names_2012 <- c('zip_code','AGI_range','return_count','','joint_return_count','','paid_preparer_return_count','exemption_count','dependent_count','AGI_amount','salary_and_wages_count','salary_and_wages_amount','taxable_interest_count','taxable_interest_amount','ordinary_dividends_count','ordinary_dividends_amount','','','business_income_count','business_income_amount','farm_income_count','net_capital_gain_count','net_capital_gain_amount','taxable_IRA_distributions_count','taxable_IRA_distributions_amount','pension_and_annuity_income_count','pension_and_annuity_income_amount','unemployment_income_count','unemployment_income_amount','social_security_count','social_security_amount','','','itemized_deductions_count','','itemized_deductions_amount','state_and_local_income_tax_count','state_and_local_income_tax_amount','state_and_local_sales_tax_count','state_and_local_sales_tax_amount','','','taxes_paid_count','taxes_paid_amount','mortgage_interest_count','mortgage_interest_amount','charitable_contributions_count','charitable_contributions_amount','taxable_income_count','taxable_income_amount','','','total_tax_credits_count','total_tax_credits_amount','','','','','','','earned_income_credit_count','earned_income_credit_amount','excess_earned_income_credit_count','excess_earned_income_credit_amount','','','','','tax_liability_count','tax_liability_amount','balance_due_count','balance_due_amount','refund_count','refund_amount')
column_names_2013 <- c('zip_code','AGI_range','return_count','','joint_return_count','','paid_preparer_return_count','exemption_count','dependent_count','AGI_amount','','','salary_and_wages_count','salary_and_wages_amount','taxable_interest_count','taxable_interest_amount','ordinary_dividends_count','ordinary_dividends_amount','','','','','business_income_count','business_income_amount','net_capital_gain_count','net_capital_gain_amount','taxable_IRA_distributions_count','taxable_IRA_distributions_amount','pension_and_annuity_income_count','pension_and_annuity_income_amount','farm_income_count','unemployment_income_count','unemployment_income_amount','social_security_count','social_security_amount','','','','','','','','','','','','','','','','','','','itemized_deductions_count','itemized_deductions_amount','','state_and_local_income_tax_count','state_and_local_income_tax_amount','state_and_local_sales_tax_count','state_and_local_sales_tax_amount','','','taxes_paid_count','taxes_paid_amount','mortgage_interest_count','mortgage_interest_amount','charitable_contributions_count','charitable_contributions_amount','taxable_income_count','taxable_income_amount','','','','','total_tax_credits_count','total_tax_credits_amount','','','','','','','','','','','','','','','','','earned_income_credit_count','earned_income_credit_amount','excess_earned_income_credit_count','excess_earned_income_credit_amount','','','','','','','tax_liability_count','tax_liability_amount','','','','','balance_due_count','balance_due_amount','refund_count','refund_amount')
column_names_2014 <- c('zip_code','AGI_range','return_count','','joint_return_count','','paid_preparer_return_count','exemption_count','dependent_count','','','','AGI_amount','','','salary_and_wages_count','salary_and_wages_amount','taxable_interest_count','taxable_interest_amount','ordinary_dividends_count','ordinary_dividends_amount','','','','','business_income_count','business_income_amount','net_capital_gain_count','net_capital_gain_amount','taxable_IRA_distributions_count','taxable_IRA_distributions_amount','pension_and_annuity_income_count','pension_and_annuity_income_amount','farm_income_count','unemployment_income_count','unemployment_income_amount','social_security_count','social_security_amount','','','','','','','','','','','','','','','','','','','itemized_deductions_count','itemized_deductions_amount','','state_and_local_income_tax_count','state_and_local_income_tax_amount','state_and_local_sales_tax_count','state_and_local_sales_tax_amount','','','taxes_paid_count','taxes_paid_amount','mortgage_interest_count','mortgage_interest_amount','charitable_contributions_count','charitable_contributions_amount','taxable_income_count','taxable_income_amount','','','','','','','total_tax_credits_count','total_tax_credits_amount','','','','','','','','','','','','','','','','','','','','','','','earned_income_credit_count','earned_income_credit_amount','excess_earned_income_credit_count','excess_earned_income_credit_amount','','','','','','','','','tax_liability_count','tax_liability_amount','','','','','balance_due_count','balance_due_amount','refund_count','refund_amount')
column_names_2015 <- c('zip_code','AGI_range','return_count','','joint_return_count','','paid_preparer_return_count','exemption_count','dependent_count','','','','','','','','AGI_amount','','','salary_and_wages_count','salary_and_wages_amount','taxable_interest_count','taxable_interest_amount','ordinary_dividends_count','ordinary_dividends_amount','','','','','business_income_count','business_income_amount','net_capital_gain_count','net_capital_gain_amount','taxable_IRA_distributions_count','taxable_IRA_distributions_amount','pension_and_annuity_income_count','pension_and_annuity_income_amount','farm_income_count','unemployment_income_count','unemployment_income_amount','social_security_count','social_security_amount','','','','','','','','','','','','','','','','','','','itemized_deductions_count','itemized_deductions_amount','','state_and_local_income_tax_count','state_and_local_income_tax_amount','state_and_local_sales_tax_count','state_and_local_sales_tax_amount','','','taxes_paid_count','taxes_paid_amount','mortgage_interest_count','mortgage_interest_amount','charitable_contributions_count','charitable_contributions_amount','taxable_income_count','taxable_income_amount','','','','','','','total_tax_credits_count','total_tax_credits_amount','','','','','','','','','','','','','','','','','','','','','','','earned_income_credit_count','earned_income_credit_amount','excess_earned_income_credit_count','excess_earned_income_credit_amount','','','','','','','','','tax_liability_count','tax_liability_amount','','','','','balance_due_count','balance_due_amount','refund_count','refund_amount')

# process_irs_data_xl is a function that takes the filepath of an Excel file containing IRS data, a vector containing column names, and the list of already-processed IRS dataframes, and converts the Excel file to a dataframe.
process_irs_data_xl <- function (filepath, column_names, year) {
  
  # Read the data from the spreadsheet in using column_names and ignoring footnotes.
  new_irs_data_xl = read_excel(filepath, range = cell_rows(7:4725), col_names = column_names)
  
  # Convert to dataframe, then strip out the columns we don't want, then filter out the blank rows, then fill in the blanks of AGI Range with the word "Total".
  new_irs_data_df <- data.frame(new_irs_data_xl) %>%
    select(-contains('X__')) %>%
    filter(zip_code != '') %>%
    replace_na(list(AGI_range = 'Total')) %>%
    mutate(year = year) %>%
    merge(zips_df) %>%
    select(county, everything())
  
  #View(new_irs_data_df)
  return(new_irs_data_df)
}

# Process the IRS data Excel sheets using process_irs_data_xl:
IRS_2011_df <- process_irs_data_xl("11zp43tn.xls", column_names_2011, 2011)
IRS_2012_df <- process_irs_data_xl("12zp43tn.xls", column_names_2012, 2012)
IRS_2013_df <- process_irs_data_xl("13zp43tn.xls", column_names_2013, 2013)
IRS_2014_df <- process_irs_data_xl("14zp43tn.xls", column_names_2014, 2014)
IRS_2015_df <- process_irs_data_xl("15zp43tn.xls", column_names_2015, 2015)

IRS_data_by_year_df <- IRS_2011_df

for (df in list(IRS_2012_df, IRS_2013_df, IRS_2014_df, IRS_2015_df)) {
  IRS_data_by_year_df <- rbind(df, IRS_data_by_year_df)
}

##View(IRS_2015_df)
###########READ/CLEAN EDUCATION##################
education_xl <- read_excel("data_2015_district_base.xlsx")

education_df <- data.frame(education_xl) %>%
  # filter(subgroup == 'All Students', grade == 'All Grades') %>%
  select(-year, -school, -school_name) %>%
  merge(crosswalk_df) %>%
  mutate(n_bsc_and_below = round(as.numeric(pct_bsc_and_below)/100 * valid_tests), n_prof_and_above = round(as.numeric(pct_prof_adv)/100 * valid_tests)) %>%
  select(county, everything(), -system) %>%
  replace(. == "*"| . == "**", NA) %>%
  map_at(.at = c(6:18), as.numeric) %>%
  as.data.frame() # not sure why, but I have to convert back to dataframe...


##View(education_df)
##############READ/CLEAN GRADUATION###################
graduation_xl <- read_excel("data_graduation_cohort_2015-16.xlsx", sheet = 'Graduation Cohort Data')

graduation_df <- data.frame(graduation_xl) %>%
  select(system = District.ID, district_name = District.Name, school_name = School.Name, graduate_count = X2015.Graduate.Count, cohort_count = X2015.Cohort.Count, graduation_rate = X2015.Graduation.Rate) %>%
  merge(crosswalk_df)

##View(graduation_df)
##----------------Rollups----------------
###########ROLLUP IRS 2015 TO COUNTY LEVEL##############
IRS_2015_by_county_df <- IRS_data_by_year_df %>% 
  filter(AGI_range == 'Total', year == 2015) %>% 
  group_by(county) %>% 
  summarize_at(vars(return_count:refund_amount), sum) %>% 
  merge(zips_df %>% group_by(county) %>% select(county, county_lat, county_lon) %>% distinct())

##View(IRS_2015_by_county_df)
#############ROLLUP EDUCATION 2015 TO COUNTY LEVEL################
education_by_county_2015_df <- 
  education_df %>%
  filter(subgroup == 'All Students', grade == 'All Grades') %>%
  group_by(county, subject) %>%
  summarize_at(vars(valid_tests:n_prof_and_above), sum)

##View(education_by_county_2015_df)

#############ROLLUP GRADUATION TO COUNTY LEVEL##############
grad_by_county_df <- graduation_df %>%
  filter(school_name == "All Schools") %>%
  group_by(county) %>%
  summarize_at(vars(graduate_count, cohort_count), sum) %>%
  mutate(county_grad_rate = graduate_count/cohort_count*100)

View(grad_by_county_df)
##Regression_Lines Unemployment------------------------------------------------------------------------------------

IRS_2015_vs_Achievement <- merge(IRS_2015_by_county_df, Achievement_Data, by=c("county"))
IRS_2015_vs_Achievement <- merge(IRS_2015_vs_Achievement, grad_by_county_df, by=c("county"))

IRS_2015_vs_Achievement$Unemployement_return_pct <- (IRS_2015_vs_Achievement$unemployment_income_count/IRS_2015_vs_Achievement$return_count) * 100

IRS_2015_vs_Achievement$Mortgage_Inrst_Ct_Pct <- (IRS_2015_vs_Achievement$mortgage_interest_count/IRS_2015_vs_Achievement$return_count) * 100

IRS_2015_vs_Achievement$Itemized_deductions_Pct <- (IRS_2015_vs_Achievement$itemized_deductions_count/IRS_2015_vs_Achievement$return_count) * 100

View(IRS_2015_vs_Achievement)

plot(x=IRS_2015_vs_Achievement$Unemployement_return_pct, y=IRS_2015_vs_Achievement$Pct_Dropout)

  plot(x=IRS_2015_vs_Achievement$Mortgage_Inrst_Ct_Pct, y=IRS_2015_vs_Achievement$Pct_Dropout)
  
    plot(x=IRS_2015_vs_Achievement$Itemized_deductions_Pct, y=IRS_2015_vs_Achievement$Pct_Dropout)
  

plot(x=IRS_2015_vs_Achievement$Unemployement_return_pct, y=IRS_2015_vs_Achievement$Algebra2)

  plot(x=IRS_2015_vs_Achievement$Mortgage_Inrst_Ct_Pct, y=IRS_2015_vs_Achievement$Algebra2)
  
    plot(x=IRS_2015_vs_Achievement$Itemized_deductions_Pct, y=IRS_2015_vs_Achievement$Algebra2)
  
      plot(x=IRS_2015_vs_Achievement$AGI_amount, y=IRS_2015_vs_Achievement$Act_Coposite)
    

plot(x=IRS_2015_vs_Achievement$Unemployement_return_pct, y=IRS_2015_vs_Achievement$English3)

  plot(x=IRS_2015_vs_Achievement$Mortgage_Inrst_Ct_Pct, y=IRS_2015_vs_Achievement$English3)

plot(x=IRS_2015_vs_Achievement$Unemployement_return_pct, y=IRS_2015_vs_Achievement$Chemistry)

  plot(x=IRS_2015_vs_Achievement$Mortgage_Inrst_Ct_Pct, y=IRS_2015_vs_Achievement$Chemistry)

plot(x=IRS_2015_vs_Achievement$Unemployement_return_pct, y=IRS_2015_vs_Achievement$Biology)

  plot(x=IRS_2015_vs_Achievement$Mortgage_Inrst_Ct_Pct, y=IRS_2015_vs_Achievement$Biology)

plot(x=IRS_2015_vs_Achievement$Unemployement_return_pct, y=IRS_2015_vs_Achievement$Pct_Graduated)

  plot(x=IRS_2015_vs_Achievement$Mortgage_Inrst_Ct_Pct, y=IRS_2015_vs_Achievement$Pct_Graduated)

plot(x=IRS_2015_vs_Achievement$Unemployement_return_pct, y=IRS_2015_vs_Achievement$Pct_Chronically_Absent)

  plot(x=IRS_2015_vs_Achievement$Mortgage_Inrst_Ct_Pct, y=IRS_2015_vs_Achievement$Pct_Chronically_Absent)

plot(x=IRS_2015_vs_Achievement$Unemployement_return_pct, y=IRS_2015_vs_Achievement$Act_Coposite)

  plot(x=IRS_2015_vs_Achievement$Mortgage_Inrst_Ct_Pct, y=IRS_2015_vs_Achievement$Act_Coposite)

plot(x=IRS_2015_vs_Achievement$Unemployement_return_pct, y=IRS_2015_vs_Achievement$Pct_Expelled)

  plot(x=IRS_2015_vs_Achievement$Mortgage_Inrst_Ct_Pct, y=IRS_2015_vs_Achievement$Pct_Expelled)


##Regression_Lines Enrollment------------------------------------------------------------------------------------

  View(graduation_df)
  View(IRS_2015_vs_Achievement)
  

options(scipen=999)
plot(x=IRS_2015_vs_Achievement$Enrollment, y=IRS_2015_vs_Achievement$Pct_Graduated,  main="All Demographics", ylab="Graduation Rate", xlab="Enrollment Level")
  
  High_Enrollment <- 
    IRS_2015_vs_Achievement %>%
    filter(IRS_2015_vs_Achievement$Enrollment >= 40000)

  View(High_Enrollment[c("county", "Pct_Graduated")])



###########DEFINE COUNTY EDUCATION DATA##################

TN_counties <- map_data("county", "tennessee")

TN_counties$county <- gsub(" ", "", TN_counties$county, fixed = TRUE)

colnames(TN_counties)[6] <- "county"

##View(TN_counties)

###########CREATE COUNTY ENROLLMENT CHOROPLETH DFs##################

Enrollment_Pool1 <- right_join(Achievement_Data, TN_counties, by="county", sort = FALSE)

Enrollment_Pool <- left_join(Enrollment_Pool1, population, by = "county", sort=FALSE)

Enrollment_Pool$Enrollment_Pct <- (Enrollment_Pool$Population/Enrollment_Pool$Enrollment)

View(Enrollment_Pool)

###########CREATE COUNTY ENROLLMENT CHOROPLETH MAPS##################

TN_Map_Pct <- ggplot(Enrollment_Pool, aes(long, lat, group = county, fill = Enrollment_Pct)) +
          geom_polygon() +
          coord_equal() +
          scale_fill_viridis(direction = -1, na.value = "gray") +
  theme(plot.title = element_text(hjust = 0.5),axis.title.x=element_blank(),axis.title.y=element_blank(),
        axis.text.x=element_blank(),axis.text.y=element_blank(),
        axis.ticks.x=element_blank(),axis.ticks.y=element_blank())+
        ggtitle("County Enrollment Levels (Pct)")

TN_Map_Pct

TN_Map <- ggplot(Enrollment_Pool, aes(long, lat, group = county, fill = Enrollment)) +
  geom_polygon() +
  coord_equal() +
  scale_fill_viridis(direction = -1, na.value = "gray") +
  theme(plot.title = element_text(hjust = 0.5),axis.title.x=element_blank(),axis.title.y=element_blank(),
        axis.text.x=element_blank(),axis.text.y=element_blank(),
        axis.ticks.x=element_blank(),axis.ticks.y=element_blank())+
  ggtitle("County Enrollment Levels")

TN_Map

###########CREATE PCT GRADUATED CHOROPLETH MAP##################

TN_Map <- ggplot(Enrollment_Pool, aes(long, lat, group = county, fill = Pct_Graduated)) +
  geom_polygon() +
  coord_equal() +
  scale_fill_viridis(direction = -1, na.value = "gray") +
  theme(plot.title = element_text(hjust = 0.5),axis.title.x=element_blank(),axis.title.y=element_blank(),
        axis.text.x=element_blank(),axis.text.y=element_blank(),
        axis.ticks.x=element_blank(),axis.ticks.y=element_blank())+
  ggtitle("County Pct Graduation Levels")

TN_Map

