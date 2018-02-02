library(readxl)
library(tidyverse)
library(maps)
library(mapdata)
library(plotly)
library(ggplot2)
library(ggmap)
library(viridis)


############READ/CLEAN ZIP################
zips_df <- read_excel('data/zip_code_database.xlsx') %>%
  select(zip_code = zip, state, county, latitude, longitude) %>%
  lapply(function(x) if(is.character(x)) tolower(x) else x) %>%
  data.frame() %>%
  filter(state == 'tn') %>%
  select(-state) %>%
  replace_na(replace = list(county = 'obion county')) %>%
  group_by(county) %>%
  mutate(county_lat = (mean(latitude))) %>%
  mutate(county_lon = (mean(longitude)))

###########READ DISTRICT/COUNTY CROSSWALK#################
crosswalk_df <- read_excel('data/data_district_to_county_crosswalk.xls') %>%
  select(county = 'County Name', system = 'District Number') %>%
  lapply(function(x) if(is.character(x)) tolower(x) else x) %>%
  data.frame()

###########READ/CLEAN IRS##################
# Create vectors of better column names, leaving blanks for those we don't want to keep (why, yes -- there probably IS a better way...).
column_names_2011 <- c('zip_code','AGI_range','return_count','joint_return_count','paid_preparer_return_count','exemption_count','dependent_count','AGI_amount','salary_and_wages_count','salary_and_wages_amount','taxable_interest_count','taxable_interest_amount','ordinary_dividends_count','ordinary_dividends_amount','','','business_income_count','business_income_amount','farm_income_count','net_capital_gain_count','net_capital_gain_amount','taxable_IRA_distributions_count','taxable_IRA_distributions_amount','pension_and_annuity_income_count','pension_and_annuity_income_amount','unemployment_income_count','unemployment_income_amount','social_security_count','social_security_amount','','','itemized_deductions_count','itemized_deductions_amount','state_and_local_income_tax_count','state_and_local_income_tax_amount','state_and_local_sales_tax_count','state_and_local_sales_tax_amount','','','taxes_paid_count','taxes_paid_amount','mortgage_interest_count','mortgage_interest_amount','charitable_contributions_count','charitable_contributions_amount','taxable_income_count','taxable_income_amount','total_tax_credits_count','total_tax_credits_amount','','','','','','','','','earned_income_credit_count','earned_income_credit_amount','excess_earned_income_credit_count','excess_earned_income_credit_amount','','','','','tax_liability_count','tax_liability_amount','balance_due_count','balance_due_amount','refund_count','refund_amount')
column_names_2012 <- c('zip_code','AGI_range','return_count','head_house_return_count','joint_return_count','','paid_preparer_return_count','exemption_count','dependent_count','AGI_amount','salary_and_wages_count','salary_and_wages_amount','taxable_interest_count','taxable_interest_amount','ordinary_dividends_count','ordinary_dividends_amount','','','business_income_count','business_income_amount','farm_income_count','net_capital_gain_count','net_capital_gain_amount','taxable_IRA_distributions_count','taxable_IRA_distributions_amount','pension_and_annuity_income_count','pension_and_annuity_income_amount','unemployment_income_count','unemployment_income_amount','social_security_count','social_security_amount','','','itemized_deductions_count','','itemized_deductions_amount','state_and_local_income_tax_count','state_and_local_income_tax_amount','state_and_local_sales_tax_count','state_and_local_sales_tax_amount','','','taxes_paid_count','taxes_paid_amount','mortgage_interest_count','mortgage_interest_amount','charitable_contributions_count','charitable_contributions_amount','taxable_income_count','taxable_income_amount','','','total_tax_credits_count','total_tax_credits_amount','','','','','','','earned_income_credit_count','earned_income_credit_amount','excess_earned_income_credit_count','excess_earned_income_credit_amount','','','','','tax_liability_count','tax_liability_amount','balance_due_count','balance_due_amount','refund_count','refund_amount')
column_names_2013 <- c('zip_code','AGI_range','return_count','head_house_return_count','joint_return_count','','paid_preparer_return_count','exemption_count','dependent_count','AGI_amount','','','salary_and_wages_count','salary_and_wages_amount','taxable_interest_count','taxable_interest_amount','ordinary_dividends_count','ordinary_dividends_amount','','','','','business_income_count','business_income_amount','net_capital_gain_count','net_capital_gain_amount','taxable_IRA_distributions_count','taxable_IRA_distributions_amount','pension_and_annuity_income_count','pension_and_annuity_income_amount','farm_income_count','unemployment_income_count','unemployment_income_amount','social_security_count','social_security_amount','','','','','','','','','','','','','','','','','','','itemized_deductions_count','itemized_deductions_amount','','state_and_local_income_tax_count','state_and_local_income_tax_amount','state_and_local_sales_tax_count','state_and_local_sales_tax_amount','','','taxes_paid_count','taxes_paid_amount','mortgage_interest_count','mortgage_interest_amount','charitable_contributions_count','charitable_contributions_amount','taxable_income_count','taxable_income_amount','','','','','total_tax_credits_count','total_tax_credits_amount','','','','','','','','','','','','','','','','','earned_income_credit_count','earned_income_credit_amount','excess_earned_income_credit_count','excess_earned_income_credit_amount','','','','','','','tax_liability_count','tax_liability_amount','','','','','balance_due_count','balance_due_amount','refund_count','refund_amount')
column_names_2014 <- c('zip_code','AGI_range','return_count','head_house_return_count','joint_return_count','','paid_preparer_return_count','exemption_count','dependent_count','','','','AGI_amount','','','salary_and_wages_count','salary_and_wages_amount','taxable_interest_count','taxable_interest_amount','ordinary_dividends_count','ordinary_dividends_amount','','','','','business_income_count','business_income_amount','net_capital_gain_count','net_capital_gain_amount','taxable_IRA_distributions_count','taxable_IRA_distributions_amount','pension_and_annuity_income_count','pension_and_annuity_income_amount','farm_income_count','unemployment_income_count','unemployment_income_amount','social_security_count','social_security_amount','','','','','','','','','','','','','','','','','','','itemized_deductions_count','itemized_deductions_amount','','state_and_local_income_tax_count','state_and_local_income_tax_amount','state_and_local_sales_tax_count','state_and_local_sales_tax_amount','','','taxes_paid_count','taxes_paid_amount','mortgage_interest_count','mortgage_interest_amount','charitable_contributions_count','charitable_contributions_amount','taxable_income_count','taxable_income_amount','','','','','','','total_tax_credits_count','total_tax_credits_amount','','','','','','','','','','','','','','','','','','','','','','','earned_income_credit_count','earned_income_credit_amount','excess_earned_income_credit_count','excess_earned_income_credit_amount','','','','','','','','','tax_liability_count','tax_liability_amount','','','','','balance_due_count','balance_due_amount','refund_count','refund_amount')
column_names_2015 <- c('zip_code','AGI_range','return_count','head_house_return_count','joint_return_count','','paid_preparer_return_count','exemption_count','dependent_count','','','','','','','','AGI_amount','','','salary_and_wages_count','salary_and_wages_amount','taxable_interest_count','taxable_interest_amount','ordinary_dividends_count','ordinary_dividends_amount','','','','','business_income_count','business_income_amount','net_capital_gain_count','net_capital_gain_amount','taxable_IRA_distributions_count','taxable_IRA_distributions_amount','pension_and_annuity_income_count','pension_and_annuity_income_amount','farm_income_count','unemployment_income_count','unemployment_income_amount','social_security_count','social_security_amount','','','','','','','','','','','','','','','','','','','itemized_deductions_count','itemized_deductions_amount','','state_and_local_income_tax_count','state_and_local_income_tax_amount','state_and_local_sales_tax_count','state_and_local_sales_tax_amount','','','taxes_paid_count','taxes_paid_amount','mortgage_interest_count','mortgage_interest_amount','charitable_contributions_count','charitable_contributions_amount','taxable_income_count','taxable_income_amount','','','','','','','total_tax_credits_count','total_tax_credits_amount','','','','','','','','','','','','','','','','','','','','','','','earned_income_credit_count','earned_income_credit_amount','excess_earned_income_credit_count','excess_earned_income_credit_amount','','','','','','','','','tax_liability_count','tax_liability_amount','','','','','balance_due_count','balance_due_amount','refund_count','refund_amount')

# process_irs_data_xl is a function that takes the filepath of an Excel file containing IRS data, a vector containing column names, and the list of already-processed IRS dataframes, and converts the Excel file to a dataframe.
process_irs_data_xl <- function (filepath, column_names, year) {
  
  # Convert to dataframe, then strip out the columns we don't want, then filter out the blank rows, then fill in the blanks of AGI Range with the word "Total".
  new_irs_data_df <- read_excel(filepath, range = cell_rows(7:4725), col_names = column_names) %>%
    select(-contains('X__')) %>%
    filter(zip_code != '') %>%
    replace_na(list(AGI_range = 'Total')) %>%
    mutate(year = year) %>%
    mutate(head_house_return_count = ifelse(year == 2011, NA, head_house_return_count)) %>%
    merge(zips_df) %>%
    select(county, everything()) %>%
    lapply(function(x) if(is.character(x)) tolower(x) else x) %>%
    data.frame()
  
  #View(new_irs_data_df)
  return(new_irs_data_df)
}

# Process the IRS data Excel sheets using process_irs_data_xl:
IRS_2011_df <- process_irs_data_xl('data/IRS_data/11zp43tn.xls', column_names_2011, 2011)
IRS_2012_df <- process_irs_data_xl('data/IRS_data/12zp43tn.xls', column_names_2012, 2012)
IRS_2013_df <- process_irs_data_xl('data/IRS_data/13zp43tn.xls', column_names_2013, 2013)
IRS_2014_df <- process_irs_data_xl('data/IRS_data/14zp43tn.xls', column_names_2014, 2014)
IRS_2015_df <- process_irs_data_xl('data/IRS_data/15zp43tn.xls', column_names_2015, 2015)

IRS_data_by_year_df <- IRS_2011_df

for (df in list(IRS_2012_df, IRS_2013_df, IRS_2014_df, IRS_2015_df)) {
 IRS_data_by_year_df <- rbind(df, IRS_data_by_year_df)
}

###########READ/CLEAN EDUCATION##################
education_all_df <- read_excel('data/data_2015_district_base.xlsx') %>%
  filter(subgroup == 'All Students', grade == 'All Grades') %>%
  replace(. == "*"| . == "**", NA) %>%
  select(-year, -school, -school_name) %>%
  mutate_at(.vars = vars(valid_tests:pct_prof_adv), .funs = funs(as.numeric)) %>%
  lapply(function(x) if(is.character(x)) tolower(x) else x) %>%
  data.frame() %>%
  merge(crosswalk_df) %>%
  mutate(n_bsc_and_below = round(((pct_bsc_and_below) / 100) * valid_tests), 
         n_prof_and_above = round(((pct_prof_adv) / 100) * valid_tests)) %>%
  select(county, everything(), -system, -contains('pct')) %>%
  mutate(pct_bsc_and_below = n_bsc_and_below / valid_tests * 100, 
         pct_prof_and_above = n_prof_and_above / valid_tests * 100)

###########READ/CLEAN EDUCATION DEMOGRAPHICS############
edu_demo_df <- read_csv('data/achievement_profile_data_with_CORE.csv') %>%
  select(system, system_name, Per_Pupil_Expenditures, Enrollment:Pct_BHN, Pct_Chronically_Absent:Pct_Expelled) %>%
  filter(!is.na(Enrollment)) %>%
  rename(Num_Black = Pct_Black, Num_Hispanic = Pct_Hispanic, Num_Native_American = Pct_Native_American, Num_EL = Pct_EL, Num_SWD = Pct_SWD, Num_ED = Pct_ED, Num_BHN = Pct_BHN, Num_Chronically_Absent = Pct_Chronically_Absent, Num_Suspended = Pct_Suspended, Num_Expelled = Pct_Expelled) %>%
  merge(crosswalk_df) %>%
  mutate_at(vars(Num_Black:Num_Expelled), funs(round(. / 100 * Enrollment))) %>%
  mutate(total_expend = Per_Pupil_Expenditures * Enrollment) %>% 
  mutate(per_pupil_expend = total_expend / Enrollment) %>%
  mutate_at(vars(Num_Black:Num_Expelled), funs(. / Enrollment * 100))

edu_demo_by_county_df <- read_csv('data/achievement_profile_data_with_CORE.csv') %>%
  select(system, system_name, Per_Pupil_Expenditures, Enrollment:Pct_BHN, Pct_Chronically_Absent:Pct_Expelled) %>%
  filter(!is.na(Enrollment)) %>%
  #rename(Num_Black = Pct_Black, Num_Hispanic = Pct_Hispanic, Num_Native_American = Pct_Native_American, Num_EL = Pct_EL, Num_SWD = Pct_SWD, Num_ED = Pct_ED, Num_BHN = Pct_BHN, Num_Chronically_Absent = Pct_Chronically_Absent, Num_Suspended = Pct_Suspended, Num_Expelled = Pct_Expelled) %>%
  merge(crosswalk_df) %>%
  mutate_at(vars(Pct_Black:Pct_Expelled), funs(round(. / 100 * Enrollment))) %>%
  group_by(county) %>%
  mutate(total_expend = Per_Pupil_Expenditures * Enrollment) %>% 
  select(county, Enrollment:total_expend) %>%
  summarize_all(sum) %>%
  mutate(per_pupil_expend = total_expend / Enrollment) %>%
  mutate_at(vars(Pct_Black:Pct_Expelled), funs(. / Enrollment * 100))

##############READ/CLEAN GRADUATION###################
graduation_xl <- read_excel('data/data_graduation_cohort_2015-16.xlsx', sheet = 'Graduation Cohort Data')

graduation_df <- data.frame(graduation_xl) %>%
  lapply(function(x) if(is.character(x)) tolower(x) else x) %>%
  data.frame() %>%
  select(system = District.ID, district_name = District.Name, school_name = School.Name, graduate_count = X2015.Graduate.Count, cohort_count = X2015.Cohort.Count, graduation_rate = X2015.Graduation.Rate) %>%
  merge(crosswalk_df)

###########ROLLUP IRS 2015 TO COUNTY LEVEL/CREATE NEW FIELDS##############
IRS_2015_by_county_df <- IRS_data_by_year_df %>% 
  filter(AGI_range == 'total', year == 2015) %>% 
  group_by(county) %>% 
  summarize_at(vars(return_count:refund_amount), sum) %>% 
  merge(zips_df %>% group_by(county) %>% select(county, county_lat, county_lon) %>% distinct()) %>%
  mutate(joint_return_pct = joint_return_count / return_count * 100, 
         head_house_return_pct = head_house_return_count / return_count * 100,
         avg_AGI_per_return = AGI_amount / return_count,
         pct_earners = salary_and_wages_count / return_count * 100,
         farms_pct = farm_income_count / return_count * 100,
         unemployment_pct = unemployment_income_count / return_count * 100,
         taxable_income_pct = taxable_income_count / return_count * 100,
         char_contrib_pct = charitable_contributions_count / return_count * 100)

#############ROLLUP GRADUATION TO COUNTY LEVEL##############
grad_by_county_df <- graduation_df %>%
  filter(school_name == "all schools") %>%
  group_by(county) %>%
  summarize_at(vars(graduate_count, cohort_count), sum) %>%
  mutate(county_grad_rate = graduate_count/cohort_count*100)

#############ROLLUP EDUCATION 2015 TO COUNTY LEVEL################
education_by_county_and_subj_2015_df <- 
  education_all_df %>%
  filter(subgroup == 'all students', grade == 'all grades') %>%
  group_by(county, subject) %>%
  summarize_at(vars(valid_tests:n_prof_and_above), sum)

education_by_county_2015_df <- 
  education_by_county_and_subj_2015_df %>%
  select(-contains('pct')) %>%
  group_by(county) %>%
  summarize_at(vars(valid_tests:n_prof_and_above), sum) %>%
  mutate(pct_bsc_and_below = n_bsc_and_below / valid_tests * 100, 
         pct_prof_and_above = n_prof_and_above / valid_tests * 100) %>%
  merge(grad_by_county_df) %>%
  merge(edu_demo_by_county_df) 


#############MERGE ROLLED UP EDUCATION WITH IRS##################
edu_IRS_data_2015_df <- education_by_county_2015_df %>%
  merge(IRS_2015_by_county_df) %>% 
  mutate_at(vars(county), funs(gsub(' county', '', .)))
  
##############MAP STUFF#########################
TN_counties <- map_data("county", "tennessee")
colnames(TN_counties)[6] <- "county"
TN_counties$county <- gsub("de kalb", "dekalb", TN_counties$county, fixed = TRUE)
