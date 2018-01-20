library(readxl)
library(tidyverse)

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
    mutate(year = year)

  #View(new_irs_data_df)
  return(new_irs_data_df)
}

# Process the IRS data Excel sheets using process_irs_data_xl:
IRS_2011_df <- process_irs_data_xl('data/IRS_data/11zp43tn.xls', column_names_2011, 2011)
IRS_2012_df <- process_irs_data_xl('data/IRS_data/12zp43tn.xls', column_names_2012, 2012)
IRS_2013_df <- process_irs_data_xl('data/IRS_data/13zp43tn.xls', column_names_2013, 2013)
IRS_2014_df <- process_irs_data_xl('data/IRS_data/14zp43tn.xls', column_names_2014, 2014)
IRS_2015_df <- process_irs_data_xl('data/IRS_data/15zp43tn.xls', column_names_2015, 2015)

IRS_data_df <- IRS_2011_df

for (df in list(IRS_2012_df, IRS_2013_df, IRS_2014_df, IRS_2015_df)) {
  IRS_data_df <- rbind(df, IRS_data_df)
}

############READ/CLEAN ZIP################
zips_xl <- read_excel('data/zip_code_database.xlsx')

zips_df <- data.frame(zips_xl) %>%
  select(c(zip, primary_city, state, county, latitude, longitude, irs_estimated_population_2014)) %>%
  filter(state == 'TN')

###########READ/CLEAN EDUCATION##################
education_csv <- read_csv('data/achievement_profile_data_with_CORE.csv')
                          
education_df <- data.frame(education_csv)

###########READ DISTRICT/COUNTY CROSSWALK#################
crosswalk_xl <- read_excel('data/data_district_to_county_crosswalk.xls')

crosswalk_df <- data.frame(crosswalk_xl) %>%
  select(county_num = County.Number, county = County.Name, system = District.Number)


##########MERGE DATA################
