#Installing  packages 

library(tidyverse)

library(readxl)

library(stringr)

library(lubridate)

library(ggmap)

library(tidyr)
# checking if each packages are installed and returs a logical value.
any(grepl("purrr",  installed.packages()))

any(grepl("readxl",  installed.packages()))



# Reading the data sets


#read_excel(path, sheet = NULL, range = NULL, col_names = TRUE,
    #       col_types = NULL, na = "", trim_ws = TRUE, skip = 0, n_max = Inf,
    #       guess_max = min(1000, n_max))

# Importing,filtering by state(TN)
rm(list=ls())

########################Read Zip code data files ######################################
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

#TN_zip$county[is.na(TN_zip$county)] <- "Obion County"
#which(is.na(TN_zip$county))
#sum(complete.cases(TN_zip)
##########################READ DISTRICT/COUNTY CROSSWALK##########################################

county_crosswalk<-data.frame(read_excel("data/data_district_to_county_crosswalk (1).xls")) %>% 
  select(county = County.Name, system = District.Number) 

county_crosswalk$county<-tolower(county_crosswalk$county)


########################################################################################
# we have also observed "0" in irs_estimated_pop and we tried to cross check them 
#we persume that they might be zip codes for companies. 

##ggmap test-the package is installed above
#TN_map<-get_map(location=c(lon=-86.660156, lat=35.86011935), zoom=5)
#ggmap(TN_map) + geom_point(aes(TN_zip$longitude,TN_zip$latitude), data=TN_zip)
###########################Read Achievement profile data files###########################

achievement<-read.csv("data/achievement_profile_data_with_CORE.csv") %>% 
  select(system, system_name,Enrollment,Per_Pupil_Expenditures,Graduation,everything() )

achievement$system_name<-tolower(achievement$system_name)

#boxplot(achievement$Science)
#hist(achievement$Science)
##################################################################################################
education_xl <- read_excel('data/data_2015_district_base.xlsx')

education_df <- data.frame(education_xl) %>%
  # filter(subgroup == 'All Students', grade == 'All Grades') %>%
  select(-year, -school, -school_name) %>%
  merge(crosswalk) %>%
  mutate(n_bsc_and_below = round(as.numeric(pct_bsc_and_below)/100 * valid_tests), n_prof_and_above = round(as.numeric(pct_prof_adv)/100 * valid_tests)) %>%
  select(county, everything(), -system) %>%
  replace(. == "*"| . == "**", NA) %>%
  map_at(.at = c(6:18), as.numeric) %>%
  as.data.frame()

##################################################################################################

names_2011<-c("zip_code","AGI_range", "return_count", "joint_return_count","paid_preparer_return_count",
              "exemption_count","dependent_count","AGI_amount","salary_and_wages_count", "salary_and_wages_amount",
              "taxable_interest_count","taxable_interest_amount","ordinary_dividends_count",
              "ordinary_dividends_amount","skip12","skip13","business_income_count","business_income_amount",
              "farm_income_count","net_capital_gain_count", "net_capital_gain_amount","taxable_IRA_distributions_count",
              "taxable_IRA_distributions_amount","pension_and_annuity_income_count", "pension_and_annuity_income_amount",
              "unemployment_income_count","unemployment_income_amount","social_security_count",
              "social_security_amount","skip16","skip17","itemized_deductions_count","itemized_deductions_amount",
              "state_and_local_income_tax_count","state_and_local_income_tax_amount","state_and_local_sales_tax_count",
              "state_and_local_sales_tax_amount","skip34","skip35","taxes_paid_count","taxes_paid_amount",
              "mortgage_interest_count","mortgage_interest_amount","charitable_contributions_count",
              "charitable_contributions_amount","taxable_income_count","taxable_income_amount","total_tax_credits_count",
              "total_tax_credits_amount","skip42","skip43","skip44","skip45","skip46","skip47",
              "skip48","skip49","earned_income_credit_count", "earned_income_credit_amount", 
              "excess_earned_income_credit_count","excess_earned_income_credit_amount",
              "skip64", "skip65", "skip66","skip67","tax_liability_count","tax_liability_amount", 
              "balance_due_count","balance_due_amount","refund_count","refund_amount")

length(names_2011)

irs11<-data.frame(read_excel("data/IRS_data/11zp43tn.xls", range=cell_rows(7:4725),col_names = names_2011, na="NA"))
#View(irs11)
irs_11<-irs11 %>%
  select(-contains("skip"))
irs_11<-irs_11[rowSums(is.na(irs_11)) != ncol(irs_11),]

irs_11$year<-2011

#View(irs_11)

names(irs_11)
glimpse(irs_11)
dim(irs_11)

names_2012<-c("zip_code","AGI_range", "return_count","skip1","joint_return_count", "skip2", "paid_preparer_return_count",
              "exemption_count","dependent_count","AGI_amount","salary_and_wages_count", "salary_and_wages_amount",
              "taxable_interest_count","taxable_interest_amount","ordinary_dividends_count",
              "ordinary_dividends_amount","skip12","skip13","business_income_count","business_income_amount",
              "farm_income_count","net_capital_gain_count", "net_capital_gain_amount","taxable_IRA_distributions_count",
              "taxable_IRA_distributions_amount","pension_and_annuity_income_count", "pension_and_annuity_income_amount",
              "unemployment_income_count","unemployment_income_amount","social_security_count",
              "social_security_amount","skip16","skip17","itemized_deductions_count","skip7","itemized_deductions_amount",
              "state_and_local_income_tax_count","state_and_local_income_tax_amount","state_and_local_sales_tax_count",
              "state_and_local_sales_tax_amount","skip34","skip35","taxes_paid_count","taxes_paid_amount",
              "mortgage_interest_count","mortgage_interest_amount","charitable_contributions_count",
              "charitable_contributions_amount","taxable_income_count","taxable_income_amount","skip42","skip43","total_tax_credits_count",
              "total_tax_credits_amount","skip44","skip45","skip46","skip47",
              "skip48","skip49","earned_income_credit_count", "earned_income_credit_amount", 
              "excess_earned_income_credit_count","excess_earned_income_credit_amount",
              "skip64", "skip65", "skip66","skip67","tax_liability_count","tax_liability_amount", 
              "balance_due_count","balance_due_amount","refund_count","refund_amount")

length(names_2012)


irs12<-data.frame(read_excel("data/IRS_data/12zp43tn.xls", range=cell_rows(7:4725),col_names = names_2012,na="NA"))
#View(irs12)
irs_12<-irs12 %>%
  select(-contains("skip"))
irs_12<-irs_12[rowSums(is.na(irs_12)) != ncol(irs_12),]

irs_12$year<-2012

#View(irs_12)

names(irs_12)
glimpse(irs_12)
dim(irs_12)

names_2013<-c("zip_code","AGI_range","return_count","skip1","joint_return_count","skip2","paid_preparer_return_count",
  "exemption_count","dependent_count","AGI_amount","skip3","skip4","salary_and_wages_count",
  "salary_and_wages_amount","taxable_interest_count","taxable_interest_amount","ordinary_dividends_count",
  "ordinary_dividends_amount","skip5","skip6","skip7","skip8","business_income_count","business_income_amount",
  "net_capital_gain_count","net_capital_gain_amount","taxable_IRA_distributions_count",
  "taxable_IRA_distributions_amount","pension_and_annuity_income_count","pension_and_annuity_income_amount","farm_income_count",
  "unemployment_income_count","unemployment_income_amount","social_security_count","social_security_amount",
  "skip16","skip17","skip18","skip19","skip20","skip21","skip22","skip23","skip24","skip25",
  "skip26","skip27","skip28","skip29","skip30","skip31","skip32","skip33",
  "itemized_deductions_count","itemized_deductions_amount","skipk","state_and_local_income_tax_count",
  "state_and_local_income_tax_amount","state_and_local_sales_tax_count","state_and_local_sales_tax_amount",
  "skip34","skip35","taxes_paid_count","taxes_paid_amount","mortgage_interest_count","mortgage_interest_amount",
  "charitable_contributions_count","charitable_contributions_amount","taxable_income_count","taxable_income_amount",
  "skip36","skip37","skip38","skip39","total_tax_credits_count","total_tax_credits_amount",
  "skip42","skip43","skip44","skip45","skip46","skip47","skip48","skip49","skip50","skip51",
  "skip52","skip53","skip54","skip55","skip56","skip57",
  "earned_income_credit_count","earned_income_credit_amount","excess_earned_income_credit_count",
  "excess_earned_income_credit_amount","skip58","skip59","skip60","skip61","skip62","skip63",
  "tax_liability_count","tax_liability_amount","skip64","skip65","skip66","skip67","balance_due_count",
  "balance_due_amount","refund_count","refund_amount")

length(names_2013)

irs13<-data.frame(read_excel("data/IRS_data/13zp43tn.xls", range=cell_rows(7:4725),col_names = names_2013,na="NA"))
#View(irs13)
irs_13<-irs13 %>% 
  select(-contains("skip"))
irs_13<-irs_13[rowSums(is.na(irs_13)) != ncol(irs_13),]

irs_13$year<-2013
#View(irs_13)

names(irs_13)
glimpse(irs_13)
dim(irs_13)

names_2014<-c("zip_code","AGI_range", "return_count","skip1", "joint_return_count","skip2","paid_preparer_return_count",
              "exemption_count","dependent_count","skip3","skip4","skip5", "AGI_amount","skip10","skip11",
              "salary_and_wages_count", "salary_and_wages_amount","taxable_interest_count","taxable_interest_amount","ordinary_dividends_count",
              "ordinary_dividends_amount","skip12","skip13","skip14","skip15","business_income_count","business_income_amount","net_capital_gain_count",
              "net_capital_gain_amount","taxable_IRA_distributions_count","taxable_IRA_distributions_amount","pension_and_annuity_income_count",
              "pension_and_annuity_income_amount","farm_income_count","unemployment_income_count","unemployment_income_amount","social_security_count",
              "social_security_amount","skip16","skip17","skip18","skip19","skip20","skip21","skip22","skip23","skip24","skip25",
              "skip26","skip27","skip28","skip29","skip30","skip31","skip32","skip33","itemized_deductions_count","itemized_deductions_amount","skipk",
              "state_and_local_income_tax_count","state_and_local_income_tax_amount","state_and_local_sales_tax_count","state_and_local_sales_tax_amount","skip34",
              "skip35","taxes_paid_count","taxes_paid_amount","mortgage_interest_count","mortgage_interest_amount","charitable_contributions_count",
              "charitable_contributions_amount","taxable_income_count","taxable_income_amount","skip36","skip37","skip38","skip39",
              "skip40","skip41","total_tax_credits_count","total_tax_credits_amount","skip42","skip43","skip44","skip45","skip46","skip47",
              "skip48","skip49","skip50","skip51","skip52","skip53","skip54","skip55","skip56","skip57","skip58","skip59","skip60", "skip61", "skip62","skip63",
              "earned_income_credit_count", "earned_income_credit_amount", "excess_earned_income_credit_count","excess_earned_income_credit_amount",
              "skip64", "skip65", "skip66","skip67","skip68", "skip69", "skip70","skip71","tax_liability_count","tax_liability_amount",
              "skip72","skip73","skip74","skip75","balance_due_count","balance_due_amount","refund_count","refund_amount")


irs14<-data.frame(read_excel("data/IRS_data/14zp43tn.xls", range=cell_rows(7:4725), col_names=names_2014, na = "NA" ))

#View(irs14)

irs_14<-irs14 %>%
  select(-contains("skip"))
irs_14<-irs_14[rowSums(is.na(irs_14)) != ncol(irs_14),]

irs_14$year<-2014
#View(irs_14)

names(irs_14)
glimpse(irs_14)
dim(irs_14)

names_2015<-c("zip_code","AGI_range", "return_count","skip1", "joint_return_count","skip2","paid_preparer_return_count",
              "exemption_count","dependent_count","skip3","skip4","skip5", "skip6","skip7","skip8","skip9","AGI_amount","skip10","skip11",
              "salary_and_wages_count", "salary_and_wages_amount","taxable_interest_count","taxable_interest_amount","ordinary_dividends_count",
              "ordinary_dividends_amount","skip12","skip13","skip14","skip15","business_income_count","business_income_amount","net_capital_gain_count",
              "net_capital_gain_amount","taxable_IRA_distributions_count","taxable_IRA_distributions_amount","pension_and_annuity_income_count",
              "pension_and_annuity_income_amount","farm_income_count","unemployment_income_count","unemployment_income_amount","social_security_count",
              "social_security_amount","skip16","skip17","skip18","skip19","skip20","skip21","skip22","skip23","skip24","skip25",
              "skip26","skip27","skip28","skip29","skip30","skip31","skip32","skip33","itemized_deductions_count","itemized_deductions_amount",
              "skipk",
              "state_and_local_income_tax_count","state_and_local_income_tax_amount","state_and_local_sales_tax_count","state_and_local_sales_tax_amount","skip34",
              "skip35","taxes_paid_count","taxes_paid_amount","mortgage_interest_count","mortgage_interest_amount","charitable_contributions_count",
              "charitable_contributions_amount","taxable_income_count","taxable_income_amount","skip36","skip37","skip38","skip39",
              "skip40","skip41","total_tax_credits_count","total_tax_credits_amount","skip42","skip43","skip44","skip45","skip46","skip47",
              "skip48","skip49","skip50","skip51","skip52","skip53","skip54","skip55","skip56","skip57","skip58","skip59","skip60", "skip61", "skip62","skip63",
              "earned_income_credit_count", "earned_income_credit_amount", "excess_earned_income_credit_count","excess_earned_income_credit_amount",
              "skip64", "skip65", "skip66","skip67","skip68", "skip69", "skip70","skip71","tax_liability_count","tax_liability_amount",
              "skip72","skip73","skip74","skip75","balance_due_count","balance_due_amount","refund_count","refund_amount")


irs15<-data.frame(read_excel("data/IRS_data/15zp43tn.xls", range=cell_rows(7:4725), col_names=names_2015, na="NA"))


#View(irs15)

irs_15<-irs15 %>%
  select(-contains("skip"))

irs_15<-irs_15[rowSums(is.na(irs_15)) != ncol(irs_15),]

irs_15$year<-2015
#View(irs_15)

#data[rowSums(is.na(data)) != ncol(data),]

names(irs_15)
glimpse(irs_15)
dim(irs_15)

### Combining  the five year IRS data sets
IRS_11_15_df<-rbind(irs_11,irs_12,irs_13,irs_14,irs_15) 

IRS_11_15_df<-replace_na(IRS_11_15_df, list(AGI_range='Total'))
# droping Na by using column AGI_range# neads tidyr library 
#IRS_11_15<-IRS_11_15 %>% drop_na(AGI_range)

View(IRS_11_15_df)

#glimpse(IRS_11_15)

##################################################################################################

# writing function to drop rows with NA in AGI_range column
#completeFun <- function(data, desiredCols) {
 # completeVec <- complete.cases(data[, desiredCols])
  #return(data[completeVec, ])
#}

#completeFun(DF, "y")
#district_base<-data.frame(read_excel("data/data_2015_district_base.xlsx"))
###################################################################################################

###################################################################################################
graduation_count<-data.frame(read_excel("data/data_graduation_cohort_2015-16.xlsx", col_names = TRUE, sheet = "Graduation Cohort Data"))
graduation_count<-graduation_count %>% 
  select(District.ID,  X2015.Graduate.Count, X2015.Cohort.Count)

graduation_count<-rename(graduation_count,
                        system= "District.ID",
                        graduation_count_2015= "X2015.Graduate.Count",
                        Cohort_count_2015="X2015.Cohort.Count")


graduation_count<-group_by(graduation_count, system) %>%
  summarise_all(funs(sum),na.rm = FALSE)

View(graduation_count)



#################################################################################################
#Merging data sets 
#################################################################################################

#merging achievement data with graduation count and cross walk

Achievement_county_count<-achievement %>%  
  inner_join(county_crosswalk) %>% 
  inner_join(graduation_count)

View(Achievement_county_count)

# aggregating by county

Achievement_county_count_df <- Achievement_county_count %>% 
  group_by(county) %>% 
  summarize(AlgI=mean(AlgI), AlgII=mean(AlgII),                 
                BioI=mean(BioI), Chemistry=mean(Chemistry),
                ELA=mean(ELA),AlgII=mean(AlgII),                 
                EngI=mean(EngI), EngII=mean(EngII), EngIII=mean(EngIII),
                Math=mean(Math),                 
                Science=mean(Science), Enrollment=sum(Enrollment), Pct_Black=mean(Pct_Black),
                Pct_Hispanic=mean(Pct_Hispanic),                 
                Pct_Native_American=mean(Pct_Native_American), Pct_EL=mean(Pct_EL),
                Pct_SWD=mean(Pct_SWD),Pct_ED=mean(Pct_ED),                 
                Per_Pupil_Expenditures=mean(Per_Pupil_Expenditures), Pct_BHN=mean(Pct_BHN),
                ACT_Composite=mean(ACT_Composite),Pct_Chronically_Absent=mean(Pct_Chronically_Absent),                 
                Pct_Suspended=mean(Pct_Suspended), Pct_Expelled=mean(Pct_Expelled), 
                Graduation=mean(Graduation),Dropout=mean(Dropout),                 
                graduation_count_2015=sum(graduation_count_2015), Cohort_count_2015=sum(Cohort_count_2015) ) 
                

IRS_11_15_df<-IRS_11_15_df %>% 
  rename(zip=zip_code)
  
  
Final_IRS_11_15<-IRS_11_15_df %>%
  inner_join(TN_zip)
              
Final_irs_2015<-Final_IRS_11_15 %>% 
  filter(year==2015) 

Final_irs_2015<-Final_irs_2015 %>% 
  filter(AGI_range=="Total")
  
  #merge(zips_df %>% group_by(county) %>% select(county, county_lat, county_lon) %>% distinct())
  
IRS_Education<-Final_irs_2015 %>% 
  inner_join(Achievement_county_count_df, by=zip)

View(IRS_Education)


rename(data_district_to_county_crosswalk_1_,system="District Number")
achievement_profile_data_with_CORE<-achievement_profile_data_with_CORE %>% 
  mutate( total_spending=(Enrollment*Per_Pupil_Expenditures)) %>% 
  inner_join(data_district_to_county_crosswalk_1_)%>%
  View(achievement_profile_data_with_CORE)


       