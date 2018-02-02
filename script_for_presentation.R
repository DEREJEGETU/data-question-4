library(readxl)
library(tidyverse)

### importing achievement data set 

achievement<-read_csv("data/achievement_profile_data_with_CORE.csv")

tolower(achievement$system_name)


##### Top 10 per pupil expenditure per school"########
complete_ach_NA<- achievement[!is.na(achievement$Per_Pupil_Expenditures),] %>% 
  filter(system!=0)


ord_complete<-complete_ach_NA %>% 
  arrange(desc(Per_Pupil_Expenditures)) 

Top_10<-head(ord_complete,10)

Lowest_10<-tail(ord_complete,10)

per_pupil_spendings=rbind(x,y)
  
ggplot(z,aes(x=Per_Pupil_Expenditures, y=Graduation))+geom_point()

glm(Graduation ~ Per_Pupil_Expenditures,data=achievement )

########Importing the cross walk data set#

cross_walk<-data.frame(read_excel("data/data_district_to_county_crosswalk (1).xls"))
names(cross_walk)
cross_walk<-rename(cross_walk, county= "County.Name", system="District.Number" )
###

achivement_county<-achievement %>% 
  mutate(total_spending=Enrollment*Per_Pupil_Expenditures, total_graduates=(Enrollment*Graduation/100),
         Pct_White=(100-(Pct_Black + Pct_Hispanic + Pct_Native_American)), total_dropout=(Enrollment*Dropout/100),total_absent=(Enrollment*Pct_Chronically_Absent/100))%>%
  mutate(total_white= (Enrollment*Pct_White/100)) %>% 
merge(cross_walk)

achivement_county_group<-achivement_county %>% 
  group_by(county) %>% 
  summarise_at(vars(Enrollment,total_spending, total_graduates, total_dropout, total_absent,total_white), sum)

achivement_county_group$county<-tolower(achivement_county_group$county)


############# the top ten's by county
achievement %>% 
  arrange(total_spending) %>% 
  Top_10<-head(achievement,10)
  
achievement %>% 
  arrange(total_spending) %>% 
  Lowest_10<-tail(achievement, 10)
##### Read and clean Zip code#####
zip_code<-data.frame(read_excel("data/zip_code_database.xlsx"))
#View(zip_code)

TN_zip<-zip_code %>% 
  select(state,zip,county,latitude,longitude,irs_estimated_population_2014) %>% 
  filter(state=="TN") %>%
  select(-state) 


# 38227 zip code's county name is missed and hence we replace it by "Obion" 

TN_zip$county[is.na(TN_zip$county)] <- "Obion County" 

# To ensure all counties have the proper letter cases 

TN_zip$county<-tolower(TN_zip$county)

#
TN_zip_df<-TN_zip %>% 
  group_by(county) %>%
  mutate(county_lat = (mean(latitude))) %>%
  mutate(county_lon = (mean(longitude)))

TN_zip_df<-rename(TN_zip_df,zip_code=zip)
################## importing the 2015 IRS tax return"
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
    merge(TN_zip_df) %>%
    select(county, everything())
  
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

###########ROLLUP IRS 2015 TO COUNTY LEVEL##############
IRS_2015_by_county_df <- IRS_data_by_year_df %>% 
  filter(AGI_range == 'Total', year == 2015) %>% 
  group_by(county) %>% 
  summarize_at(vars(return_count:refund_amount), sum) %>% 
  merge(TN_zip_df %>% group_by(county) %>% select(county, county_lat, county_lon) %>% distinct())



IRS_Education<-IRS_2015_by_county_df %>% 
  inner_join(achivement_county_group, by="county")

##Data Visualisation 
#In this section, we want to see if counies investment on education has an impact ih helping 
#students to graduate*.

# we have used spending per enrollment  to represent per pupil expenditure 
# does per pupil spenidng correlated with  coubty's wealth, we have used adjusted gross income per enrollment" as 
# a proxy for county wealth to capture/ minimize the biase in size of the population". We assume , irrespective 
#of their income status , parents might send their children to school. 
##Generating the New variables##
IRS_Education<-IRS_Education %>% 
  mutate(county_per_unit_expenditure=total_spending/Enrollment, pct_graduates=(total_graduates/Enrollment)*100, 
         AGI_per_enrollment=(AGI_amount/Enrollment)*1000, pct_absentism=(total_absent/Enrollment)*100,
pct_white=(total_white/Enrollment)*100,pct_dropout=(total_dropout/Enrollment)*100)


## Does counties wealth matter 
ggplot(IRS_Education, aes(x=AGI_per_enrollment, y=pct_graduates))+
  geom_point()

# the above graph help us to understand graduation is not correlated with the wealth of the county. 

#> achievement_county<-achivement_county %>% group_by(county) %>% replace_na(Graduation=mean(Graduation))

ggplot(IRS_Education, aes(x=county_per_unit_expenditure, y=pct_graduates))+
         geom_point()
### We can observe that per unit spending does not contribute to wards , graduation this pushed us to investigate 
### what other factors are responsible for graduation 
# we want to see if absentism and ethnicity matter 

ggplot(IRS_Education, aes(x=pct_white, y=pct_graduates))+
  geom_point()

## So, Ethnicity matters, atttention  has to be given to minority shcools 

ggplot(IRS_Education, aes(x=pct_absentism, y=pct_graduates))+
  geom_point()

###
ggplot(IRS_Education, aes(x=AGI_per_enrollment, y=pct_absentism))+
  geom_point()

# The above graph has shown us county's wealth do matter for absentism. Low income families has a highr rate of absentism than the highincomes)
ggplot(IRS_Education, aes(x=AGI_per_enrollment, y=pct_dropout))+
  geom_point()+
  geom_smooth(method = "lm", se = FALSE, color = "red")

# similar to absentism wealth matter to reduce dropout 

ggplot(data = IRS_Education, mapping = aes(x =AGI_per_enrollment , y =pct_graduates )) + 
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red")

ggplot(data = IRS_Education, mapping = aes(x =AGI_per_enrollment , y =county_per_unit_expenditure, size=pct_graduates)) + 
  geom_point(aes(color=factor(county))) +
  geom_smooth(method = "lm", se = FALSE, color = "red")

IRS_Education %>% 
  lm(formula=county_per_unit_expenditure ~ AGI_per_enrollment)


