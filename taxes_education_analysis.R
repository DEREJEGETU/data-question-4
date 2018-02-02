source('taxes_education_dataframes.r')

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
