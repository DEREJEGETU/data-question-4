source('taxes_education_dataframes.r')
##############CORRELATIONS######################


##############ANALYSIS OF INCOME VS PCT OF PASSING TEST SCORES#########################
edu_IRS_data_2015_df %>%
  ggplot(aes(x = joint_return_pct, y = pct_prof_and_above)) +
  geom_point(na.rm = TRUE) +
  stat_smooth(method="lm")

edu_IRS_data_2015_df %>%
  ggplot(aes(x = farms_pct, y = pct_prof_and_above)) +
  geom_point(na.rm = TRUE) +
  stat_smooth(method="lm")

edu_IRS_data_2015_df %>%
  ggplot(aes(x = unemployment_pct, y = pct_prof_and_above)) +
  geom_point(na.rm = TRUE) +
  stat_smooth(method="lm")

edu_IRS_data_2015_df %>%
  ggplot(aes(x = taxable_income_pct, y = pct_prof_and_above)) +
  geom_point(na.rm = TRUE) +
  stat_smooth(method="lm")

edu_IRS_data_2015_df %>%
  ggplot(aes(x = char_contrib_pct, y = pct_prof_and_above)) +
  geom_point(na.rm = TRUE) +
  stat_smooth(method="lm")

edu_IRS_data_2015_df %>%
  ggplot(aes(x = avg_AGI_per_return, y = pct_prof_and_above)) +
  geom_point(na.rm = TRUE) +
  stat_smooth(method="lm")


##############ANALYSIS OF INCOME VS GRADUATION RATE#########################
edu_IRS_data_2015_df %>%
  ggplot(aes(x = joint_return_pct, y = county_grad_rate)) +
  geom_point(na.rm = TRUE) +
  stat_smooth(method="lm")

edu_IRS_data_2015_df %>%
  ggplot(aes(x = farms_pct, y = county_grad_rate)) +
  geom_point(na.rm = TRUE) +
  stat_smooth(method="lm")

edu_IRS_data_2015_df %>%
  ggplot(aes(x = unemployment_pct, y = county_grad_rate)) +
  geom_point(na.rm = TRUE) +
  stat_smooth(method="lm")

edu_IRS_data_2015_df %>%
  ggplot(aes(x = taxable_income_pct, y = county_grad_rate)) +
  geom_point(na.rm = TRUE) +
  stat_smooth(method="lm")

edu_IRS_data_2015_df %>%
  ggplot(aes(x = char_contrib_pct, y = county_grad_rate)) +
  geom_point(na.rm = TRUE) +
  stat_smooth(method="lm")

edu_IRS_data_2015_df %>%
  ggplot(aes(x = avg_AGI_per_return, y = county_grad_rate)) +
  geom_point(na.rm = TRUE) +
  stat_smooth(method="lm")


##############ANALYSIS OF PER PUPIL EXPENDITURES VS PCT OF PASSING TEST SCORES#########################
edu_IRS_data_2015_df %>%
  ggplot(aes(x = per_pupil_expend, y = pct_prof_and_above)) +
  geom_point(na.rm = TRUE) +
  stat_smooth(method="lm")


##############MAPS######################
mapping_edu_IRS_df <- edu_IRS_data_2015_df %>%
  right_join(TN_counties) 

options(scipen = 999)
mapping_edu_IRS_df%>% 
  ggplot(aes(x = long, y = lat, group = county, fill = Enrollment)) +
  geom_polygon() +
  coord_equal() +
  scale_fill_viridis(direction = -1, na.value = "gray") +
  theme(plot.title = element_text(hjust = 0.5),axis.title.x=element_blank(),axis.title.y=element_blank(),
        axis.text.x=element_blank(),axis.text.y=element_blank(),
        axis.ticks.x=element_blank(),axis.ticks.y=element_blank())+
  ggtitle("County Enrollment")

mapping_edu_IRS_df %>% 
  ggplot(aes(x = long, y = lat, group = county, fill = pct_prof_and_above)) +
  geom_polygon() +
  coord_equal() +
  scale_fill_viridis(direction = -1, na.value = "gray") +
  theme(plot.title = element_text(hjust = 0.5),axis.title.x=element_blank(),axis.title.y=element_blank(),
        axis.text.x=element_blank(),axis.text.y=element_blank(),
        axis.ticks.x=element_blank(),axis.ticks.y=element_blank())+
  ggtitle("Percentage of Assessments Passed")
