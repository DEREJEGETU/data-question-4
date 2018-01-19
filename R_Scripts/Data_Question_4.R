##---Install Packages---##
install.packages("tidyverse")
install.packages("dplyr")
install.packages("readr")
install.packages("readxl")
install.packages("tidyr")
##----------------------##

#########---Load all required libraries---#########
library(dplyr)
library(readr)
library(readxl)
library(tidyr)
#########---------------------------------#########

###---Set working directory to location that is GIT compatible---
  setwd("C:/Users/kenne/GIT/data-question-4-data-question-4-impact-squad/data")
    ##--Use to verify working directory-- getwd()
  
###---Read in achievement_profile_data_with_CORE.csv for data cleaning---
  Education_Data <- read.csv("achievement_profile_data_with_CORE.csv")

##---Rename columns to be more meaningful and understandable---
  names(Education_Data) <- c("system", "system_name","Algebra1", "Algebra2", "Biology1", "Chemistry", "Elementary_ENG", "English1", "English2"
  , "English3", "Elementary_Math", "Elementary_Science", "Enrollment", "Pct_Black", "Pct_Hispanic", "Pct_Native_American", "Pct_EnglishLearners", 
  "Pct_Disabled_Students", "Pct_Econ_Disadvantaged", "Per_Pupil_Expenditures", "Pct_Blk_Hspnc_NtvAmrcn", "Act_Coposite", "Pct_Chronically_Absent",
  "Pct_Suspended", "Pct_Expelled", "Pct_Graduated", "Pct_Dropout", "CORE_region")

###--Examine the structure of the Education_Data df to locate areas for cleaning and data type casting---
  str(Education_Data)

##--Read in 2015 IRS data set and remove top 6 rows and footnotes
  IRS_2015 <- data.frame(read_excel("15zp43tn.xls", range = cell_rows(7:4725), col_names = FALSE))
  
##--Remove rows that are all NA---
  ind <- apply(IRS_2015, 1, function(x) all(is.na(x)))
  IRS_2015 <- IRS_2015[!ind, ]
  
##--Replace cells that are NA with the value 'Total'--- 
  IRS_2015[is.na(IRS_2015)] <- "Total"
  
##--Review the results to test---  
  View(IRS_2015)
  
###--Examine the structure of the IRS_2015 df to locate areas for more cleaning and data type casting---
  str(IRS_2015)
  


#########-------------Notes---------------#########  
###--Remove every row where column 1 is ' '
  
###--Remove every cell that has **
  
###--Replace every ' ' with 'Total'
  
###--Set column name appropriately
  
##--Rename columns to be more meaningful and understandable in IRS_2015
  names(IRS_2015) <- c("Zip_Code", "AGI_Range","Return_Count", "Exemption_Count", "Dependent_Count", "AGI_Amount", "Salary_And_Wages_Count", "Salary_And_Wages_Amount", "Taxable_Interest_Count"
                       , "Taxable_Interest_Amount", "Ordinary_Dividends_Count", "Ordinary_Dividends_Amount", "Business_Income_Amount", "Farm_Income_Count", "Farm_Income_Amount", "Net_Capital_Gain_Count", "Net_Capital_Gain_Amount", 
                       "Taxable_IRA_Distributions_Count", "Taxable_IRA_Distributions_Amount", "Pension_And_Annuity_Income_Count", "Pension_And_Annuity_Income_Amount", "Unemployment_Income_Count", "Unemployment_Income_Amount",
                       "Social_Security_Count", "Social_Security_Amount", "Itemized_Deductions_Count", "Itemized_Deductions_Amount", "Charitable_Contributions_Count", 
                       "Charitable_Contributions_Amount", "Mortgage_Interest_Count", "Mortgage_Interest_Count", "Property_Tax_Count", "Property_Tax_Amount", "State_And_Local_Income_Tax_Count", "State_And_Local_Income_Tax_Amount", 
                       "State_And_Local_Sales_Tax_Count", "State_And_Local_Sales_Tax_Amount", "Taxable_Income_Amount", "Total_Tax_Credits_Count", "Total_Tax_Credits_Amount", "Earned_Income_Credit_Count", "Earned_Income_Amount", 
                       "Excess_Earned_Income_Credit_Count", "Excess_Earned_Income_Credit_Amount", "Tax_Liability_Count", "Tax_Liability_Amount", 
                       "Balance_Due_Count", "Balance_Due_Amount", "Refund_Count", "Refund_Amount")  
#########---------------------------------#########