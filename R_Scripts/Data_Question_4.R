##---Install Packages---##
install.packages("tidyverse")
##----------------------##

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

##--Read in 2015 IRS data set 
  IRS_2015 <- read.csv("15zpallagi.csv")

###--Examine the structure of the IRS_2015 df to locate areas for cleaning and data type casting---
  str(IRS_2015)
  
  