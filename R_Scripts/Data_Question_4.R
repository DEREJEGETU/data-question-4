##---Install Packages---##
install.packages("tidyverse")
install.packages("dplyr")
install.packages("readr")
install.packages("readxl")
install.packages("tidyr")
install.packages("purrr")
##----------------------##

#########---Load all required libraries---#########
library(dplyr)
library(readr)
library(readxl)
library(tidyr)
library(purrr)
#########---------------------------------#########

##--Overhead----------------------------------------------------------------------------------------------------------------------------------------------------------------  

###---Set working directory to location that is GIT compatible---
  setwd("C:/Users/kenne/GIT/data-question-4-data-question-4-impact-squad/data")
    ##--Use to verify working directory-- getwd()

##----------------------------------------------------------------------------------------------------------------------------------------------------------------  

###---Read in achievement_profile_data_with_CORE.csv for data cleaning---
  Education_Data <- read.csv("achievement_profile_data_with_CORE.csv")
  
###---Examine the structure of the Education_Data df to locate areas for cleaning and data type casting---
  str(Education_Data)  

##---Rename columns to be more meaningful and understandable---
  names(Education_Data) <- c("system", "system_name","Algebra1", "Algebra2", "Biology1", "Chemistry", "Elementary_ENG", "English1", "English2"
  , "English3", "Elementary_Math", "Elementary_Science", "Enrollment", "Pct_Black", "Pct_Hispanic", "Pct_Native_American", "Pct_EnglishLearners", 
  "Pct_Disabled_Students", "Pct_Econ_Disadvantaged", "Per_Pupil_Expenditures", "Pct_Blk_Hspnc_NtvAmrcn", "Act_Coposite", "Pct_Chronically_Absent",
  "Pct_Suspended", "Pct_Expelled", "Pct_Graduated", "Pct_Dropout", "CORE_region")

##----------------------------------------------------------------------------------------------------------------------------------------------------------------  
  
##IRS Data Cleaning----------------------------------------------------------------------------------------------------------------------------------------------------------------  
  
##IRIS_2011----------------------------------------------------------------------------------------------------------------------------------------------------------------  
  
##---Read in 2011 IRS data set and remove top 6 rows and footnotes:
  IRS_2011 <- data.frame(read_excel("11zp43tn.xls", range = cell_rows(7:4725), col_names = FALSE))
  

##---Rename columns to be more meaningful and understandable:  
  names(IRS_2011) <-  c('zip_code','AGI_range','return_count','joint_return_count','paid_preparer_return_count','exemption_count','dependent_count','AGI_amount','salary_and_wages_count','salary_and_wages_amount','taxable_interest_count','taxable_interest_amount','ordinary_dividends_count','ordinary_dividends_amount','x','x','business_income_count','business_income_amount','farm_income_count','net_capital_gain_count','net_capital_gain_amount','taxable_IRA_distributions_count','taxable_IRA_distributions_amount','pension_and_annuity_income_count','pension_and_annuity_income_amount','unemployment_income_count','unemployment_income_amount','social_security_count','social_security_amount','x','x','itemized_deductions_count','itemized_deductions_amount','state_and_local_income_tax_count','state_and_local_income_tax_amount','state_and_local_sales_tax_count','state_and_local_sales_tax_amount','x','x','taxes_paid_count','taxes_paid_amount','mortgage_interest_count','mortgage_interest_amount','charitable_contributions_count','charitable_contributions_amount','taxable_income_count','taxable_income_amount','total_tax_credits_count','total_tax_credits_amount','x','x','x','x','x','x','x','x','earned_income_credit_count','earned_income_credit_amount','excess_earned_income_credit_count','excess_earned_income_credit_amount','x','x','x','x','tax_liability_count','tax_liability_amount','balance_due_count','balance_due_amount','refund_count','refund_amount')
  
  
##---Remove columns that do not exist in data dictionary:
  IRS_2011 <- IRS_2011[, -which(names(IRS_2011) %in% c("x"))]
  
  
##---Remove rows that are all NA:
  ind <- apply(IRS_2011, 1, function(x) all(is.na(x)))
  IRS_2011 <- IRS_2011[!ind, ]  

  
##---Replace remaining cells that are NA with the value 'Total':
  IRS_2011[is.na(IRS_2011)] <- "Total"
  
##---Review the results to test IRS_2011:
  View(IRS_2011)
  

##----------------------------------------------------------------------------------------------------------------------------------------------------------------  
##IRS_2012----------------------------------------------------------------------------------------------------------------------------------------------------------------  
  
##---Read in 2012 IRS data set and remove top 6 rows and footnotes
  IRS_2012 <- data.frame(read_excel("12zp43tn.xls", range = cell_rows(7:4725), col_names = FALSE))  

  
##---Rename columns to be more meaningful and understandable:  
  names(IRS_2012) <- c('zip_code','AGI_range','return_count','x','joint_return_count','x','paid_preparer_return_count','exemption_count','dependent_count','AGI_amount','salary_and_wages_count','salary_and_wages_amount','taxable_interest_count','taxable_interest_amount','ordinary_dividends_count','ordinary_dividends_amount','x','x','business_income_count','business_income_amount','farm_income_count','net_capital_gain_count','net_capital_gain_amount','taxable_IRA_distributions_count','taxable_IRA_distributions_amount','pension_and_annuity_income_count','pension_and_annuity_income_amount','unemployment_income_count','unemployment_income_amount','social_security_count','social_security_amount','x','x','itemized_deductions_count','x','itemized_deductions_amount','state_and_local_income_tax_count','state_and_local_income_tax_amount','state_and_local_sales_tax_count','state_and_local_sales_tax_amount','x','x','taxes_paid_count','taxes_paid_amount','mortgage_interest_count','mortgage_interest_amount','charitable_contributions_count','charitable_contributions_amount','taxable_income_count','taxable_income_amount','x','x','total_tax_credits_count','total_tax_credits_amount','x','x','x','x','x','x','earned_income_credit_count','earned_income_credit_amount','excess_earned_income_credit_count','excess_earned_income_credit_amount','x','x','x','x','tax_liability_count','tax_liability_amount','balance_due_count','balance_due_amount','refund_count','refund_amount')

  
##---Remove columns that do not exist in data dictionary:
  IRS_2012 <- IRS_2012[, -which(names(IRS_2012) %in% c("x"))]
  
  
##---Remove rows that are all NA:
  ind <- apply(IRS_2012, 1, function(x) all(is.na(x)))
  IRS_2012 <- IRS_2012[!ind, ]  
  
  
##---Replace remaining cells that are NA with the value 'Total':
  IRS_2012[is.na(IRS_2012)] <- "Total"
  
##---Review the results to test IRS_2012:
  View(IRS_2012)  
  
  
##----------------------------------------------------------------------------------------------------------------------------------------------------------------  
##IRS_2013----------------------------------------------------------------------------------------------------------------------------------------------------------------  
  
##---Read in 2013 IRS data set and remove top 6 rows and footnotes
  IRS_2013 <- data.frame(read_excel("13zp43tn.xls", range = cell_rows(7:4725), col_names = FALSE))

  
##---Rename columns to be more meaningful and understandable:  
  names(IRS_2013) <- c('zip_code','AGI_range','return_count','x','joint_return_count','x','paid_preparer_return_count','exemption_count','dependent_count','AGI_amount','x','x','salary_and_wages_count','salary_and_wages_amount','taxable_interest_count','taxable_interest_amount','ordinary_dividends_count','ordinary_dividends_amount','x','x','x','x','business_income_count','business_income_amount','net_capital_gain_count','net_capital_gain_amount','taxable_IRA_distributions_count','taxable_IRA_distributions_amount','pension_and_annuity_income_count','pension_and_annuity_income_amount','farm_income_count','unemployment_income_count','unemployment_income_amount','social_security_count','social_security_amount','x','x','x','x','x','x','x','x','x','x','x','x','x','x','x','x','x','x','itemized_deductions_count','itemized_deductions_amount','x','state_and_local_income_tax_count','state_and_local_income_tax_amount','state_and_local_sales_tax_count','state_and_local_sales_tax_amount','x','x','taxes_paid_count','taxes_paid_amount','mortgage_interest_count','mortgage_interest_amount','charitable_contributions_count','charitable_contributions_amount','taxable_income_count','taxable_income_amount','x','x','x','x','total_tax_credits_count','total_tax_credits_amount','x','x','x','x','x','x','x','x','x','x','x','x','x','x','x','x','earned_income_credit_count','earned_income_credit_amount','excess_earned_income_credit_count','excess_earned_income_credit_amount','x','x','x','x','x','x','tax_liability_count','tax_liability_amount','x','x','x','x','balance_due_count','balance_due_amount','refund_count','refund_amount')

##---Remove columns that do not exist in data dictionary:
  IRS_2013 <- IRS_2013[, -which(names(IRS_2013) %in% c("x"))]
  
  
##---Remove rows that are all NA:
  ind <- apply(IRS_2013, 1, function(x) all(is.na(x)))
  IRS_2013 <- IRS_2013[!ind, ]  
  
  
##---Replace remaining cells that are NA with the value 'Total':
  IRS_2013[is.na(IRS_2013)] <- "Total"
  
##---Review the results to test IRS_2013:
  View(IRS_2013)    
  
  
##----------------------------------------------------------------------------------------------------------------------------------------------------------------  
##IRS_2014----------------------------------------------------------------------------------------------------------------------------------------------------------------  
  
  
##---Read in 2014 IRS data set and remove top 6 rows and footnotes
  IRS_2014 <- data.frame(read_excel("14zp43tn.xls", range = cell_rows(7:4725), col_names = FALSE))


##---Rename columns to be more meaningful and understandable:  
  names(IRS_2014) <- c('zip_code','AGI_range','return_count','x','joint_return_count','x','paid_preparer_return_count','exemption_count','dependent_count','x','x','x','AGI_amount','x','x','salary_and_wages_count','salary_and_wages_amount','taxable_interest_count','taxable_interest_amount','ordinary_dividends_count','ordinary_dividends_amount','x','x','x','x','business_income_count','business_income_amount','net_capital_gain_count','net_capital_gain_amount','taxable_IRA_distributions_count','taxable_IRA_distributions_amount','pension_and_annuity_income_count','pension_and_annuity_income_amount','farm_income_count','unemployment_income_count','unemployment_income_amount','social_security_count','social_security_amount','x','x','x','x','x','x','x','x','x','x','x','x','x','x','x','x','x','x','itemized_deductions_count','itemized_deductions_amount','x','state_and_local_income_tax_count','state_and_local_income_tax_amount','state_and_local_sales_tax_count','state_and_local_sales_tax_amount','x','x','taxes_paid_count','taxes_paid_amount','mortgage_interest_count','mortgage_interest_amount','charitable_contributions_count','charitable_contributions_amount','taxable_income_count','taxable_income_amount','x','x','x','x','x','x','total_tax_credits_count','total_tax_credits_amount','x','x','x','x','x','x','x','x','x','x','x','x','x','x','x','x','x','x','x','x','x','x','earned_income_credit_count','earned_income_credit_amount','excess_earned_income_credit_count','excess_earned_income_credit_amount','x','x','x','x','x','x','x','x','tax_liability_count','tax_liability_amount','x','x','x','x','balance_due_count','balance_due_amount','refund_count','refund_amount')

##---Remove columns that do not exist in data dictionary:
  IRS_2014 <- IRS_2014[, -which(names(IRS_2014) %in% c("x"))]
  
  
##---Remove rows that are all NA:
  ind <- apply(IRS_2014, 1, function(x) all(is.na(x)))
  IRS_2014 <- IRS_2013[!ind, ]  
  
  
##---Replace remaining cells that are NA with the value 'Total':
  IRS_2014[is.na(IRS_2014)] <- "Total"
  
  
##---Review the results to test IRS_2014:
  View(IRS_2014) 

  
##----------------------------------------------------------------------------------------------------------------------------------------------------------------  
##IRS_2015----------------------------------------------------------------------------------------------------------------------------------------------------------------  
  
    
##---Read in 2015 IRS data set and remove top 6 rows and footnotes
  IRS_2015 <- data.frame(read_excel("15zp43tn.xls", range = cell_rows(7:4725), col_names = FALSE))


##---Rename columns to be more meaningful and understandable:  
  names(IRS_2015) <- column_names_2015 <- c('zip_code','AGI_range','return_count','x','joint_return_count','x','paid_preparer_return_count','exemption_count','dependent_count','x','x','x','x','x','x','x','AGI_amount','x','x','salary_and_wages_count','salary_and_wages_amount','taxable_interest_count','taxable_interest_amount','ordinary_dividends_count','ordinary_dividends_amount','x','x','x','x','business_income_count','business_income_amount','net_capital_gain_count','net_capital_gain_amount','taxable_IRA_distributions_count','taxable_IRA_distributions_amount','pension_and_annuity_income_count','pension_and_annuity_income_amount','farm_income_count','unemployment_income_count','unemployment_income_amount','social_security_count','social_security_amount','x','x','x','x','x','x','x','x','x','x','x','x','x','x','x','x','x','x','itemized_deductions_count','itemized_deductions_amount','x','state_and_local_income_tax_count','state_and_local_income_tax_amount','state_and_local_sales_tax_count','state_and_local_sales_tax_amount','x','x','taxes_paid_count','taxes_paid_amount','mortgage_interest_count','mortgage_interest_amount','charitable_contributions_count','charitable_contributions_amount','taxable_income_count','taxable_income_amount','x','x','x','x','x','x','total_tax_credits_count','total_tax_credits_amount','x','x','x','x','x','x','x','x','x','x','x','x','x','x','x','x','x','x','x','x','x','x','earned_income_credit_count','earned_income_credit_amount','excess_earned_income_credit_count','excess_earned_income_credit_amount','x','x','x','x','x','x','x','x','tax_liability_count','tax_liability_amount','x','x','x','x','balance_due_count','balance_due_amount','refund_count','refund_amount')

  
##---Remove columns that do not exist in data dictionary:
  IRS_2015 <- IRS_2015[, -which(names(IRS_2015) %in% c("x"))]
  
  
##---Remove rows that are all NA:
  ind <- apply(IRS_2015, 1, function(x) all(is.na(x)))
  IRS_2015 <- IRS_2015[!ind, ]  
  
  
##---Replace remaining cells that are NA with the value 'Total':
  IRS_2015[is.na(IRS_2015)] <- "Total"
  
  
##---Review the results to test IRS_2015:
  View(IRS_2015)   
  
  
##----------------------------------------------------------------------------------------------------------------------------------------------------------------  
  
##---Read in zip_code_database.xlsx file:
  
  zip_code<-read_excel("zip_code_database.xlsx")
  

##---Define ZipCode data for the state of TN:
  
  TN_zip<-zip_code %>% 
    select(state,zip,county,latitude,longitude,irs_estimated_population_2014) %>% 
    filter(zip_code$state=="TN")
  
  
##---Test the TN_zip df for existence of nulls:

  any(is.na(TN_zip))
  sum(is.na(TN_zip))

  
##---Test the county column from the TN_zip df for existence of NA(s):
  
  sum(is.na(TN_zip$county))
  
  
##---Filter the county column in the TN_zip df for the record that contains the NA:
  
  TN_zip %>% 
    filter(is.na(county))

  
##---38227 zip code's county name is missed and hence we replace it by "Obion":
  TN_zip$county[is.na(TN_zip$county)] <- "Obion County" 

  
##---Test the TN_zip df again for NA(s), the function should return 'FALSE':
  
  any(is.na(TN_zip))
  
  
##----------------------------------------------------------------------------------------------------------------------------------------------------------------  
  
  
#########-------------Notes---------------#########  

  
#########---------------------------------#########