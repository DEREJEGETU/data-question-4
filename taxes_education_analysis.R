source('taxes_education_dataframes.r')

#install.packages("devtools")
#library(devtools)
#install_github('arilamstein/choroplethrZip@v1.5.0')
library(choroplethr)
#library(choroplethrZip)

mapping_edu_IRS_df <- edu_IRS_data_2015_df %>%
  right_join(TN_counties) 

options(scipen = 999)

##############MAPS FOR INCOME######################
mapping_edu_IRS_df%>% 
  ggplot(aes(x = long, y = lat, group = county, fill = avg_AGI_per_return)) +
  geom_polygon() +
  coord_equal() +
  scale_fill_viridis(direction = -1, na.value = "gray") +
  theme(plot.title = element_text(hjust = 0.5),axis.title.x=element_blank(),axis.title.y=element_blank(),
        axis.text.x=element_blank(),axis.text.y=element_blank(),
        axis.ticks.x=element_blank(),axis.ticks.y=element_blank())+
  ggtitle("Average AGI Per Return")

mapping_edu_IRS_df %>% 
  ggplot(aes(x = long, y = lat, group = county, fill = farms_pct)) +
  geom_polygon() +
  coord_equal() +
  scale_fill_viridis(direction = -1, na.value = "gray") +
  theme(plot.title = element_text(hjust = 0.5),axis.title.x=element_blank(),axis.title.y=element_blank(),
        axis.text.x=element_blank(),axis.text.y=element_blank(),
        axis.ticks.x=element_blank(),axis.ticks.y=element_blank())+
  ggtitle("Percentages of Returns With Farm Income")

mapping_edu_IRS_df %>% 
  ggplot(aes(x = long, y = lat, group = county, fill = unemployment_pct)) +
  geom_polygon() +
  coord_equal() +
  scale_fill_viridis(direction = -1, na.value = "gray") +
  theme(plot.title = element_text(hjust = 0.5),axis.title.x=element_blank(),axis.title.y=element_blank(),
        axis.text.x=element_blank(),axis.text.y=element_blank(),
        axis.ticks.x=element_blank(),axis.ticks.y=element_blank())+
  ggtitle("Percentages of Returns With Unemployment Income")

##############MAPS FOR EDUCATION######################
mapping_edu_IRS_df %>% 
  ggplot(aes(x = long, y = lat, group = county, fill = pct_prof_and_above)) +
  geom_polygon() +
  coord_equal() +
  scale_fill_viridis(direction = -1, na.value = "gray") +
  theme(plot.title = element_text(hjust = 0.5),axis.title.x=element_blank(),axis.title.y=element_blank(),
        axis.text.x=element_blank(),axis.text.y=element_blank(),
        axis.ticks.x=element_blank(),axis.ticks.y=element_blank())+
  ggtitle("Percent of State Assessments Passed (All Subjects)")

mapping_edu_IRS_df %>% 
  ggplot(aes(x = long, y = lat, group = county, fill = county_grad_rate)) +
  geom_polygon() +
  coord_equal() +
  scale_fill_viridis(direction = -1, na.value = "gray") +
  theme(plot.title = element_text(hjust = 0.5),axis.title.x=element_blank(),axis.title.y=element_blank(),
        axis.text.x=element_blank(),axis.text.y=element_blank(),
        axis.ticks.x=element_blank(),axis.ticks.y=element_blank())+
  ggtitle("Graduation Rates")

mapping_edu_IRS_df %>% 
  ggplot(aes(x = long, y = lat, group = county, fill = Pct_ED)) +
  geom_polygon() +
  coord_equal() +
  scale_fill_viridis(direction = -1, na.value = "gray") +
  theme(plot.title = element_text(hjust = 0.5),axis.title.x=element_blank(),axis.title.y=element_blank(),
        axis.text.x=element_blank(),axis.text.y=element_blank(),
        axis.ticks.x=element_blank(),axis.ticks.y=element_blank())+
  ggtitle("Percent of Economically Disadvantaged Students")

mapping_edu_IRS_df%>% 
  ggplot(aes(x = long, y = lat, group = county, fill = Pct_BHN)) +
  geom_polygon() +
  coord_equal() +
  scale_fill_viridis(direction = -1, na.value = "gray") +
  theme(plot.title = element_text(hjust = 0.5),axis.title.x=element_blank(),axis.title.y=element_blank(),
        axis.text.x=element_blank(),axis.text.y=element_blank(),
        axis.ticks.x=element_blank(),axis.ticks.y=element_blank())+
  ggtitle("Percentage of Minority Students (Black/Hispanic/Native American)")

mapping_edu_IRS_df %>% 
  ggplot(aes(x = long, y = lat, group = county, fill = per_pupil_expend)) +
  geom_polygon() +
  coord_equal() +
  scale_fill_viridis(direction = -1, na.value = "gray") +
  theme(plot.title = element_text(hjust = 0.5),axis.title.x=element_blank(),axis.title.y=element_blank(),
        axis.text.x=element_blank(),axis.text.y=element_blank(),
        axis.ticks.x=element_blank(),axis.ticks.y=element_blank())+
  ggtitle("Per Pupil Expenditures")

##############CORRELATIONS######################
# edu_IRS_data_2015_df %>% 
#   select(contains('pct'), county_grad_rate, per_pupil_expend) %>% 
#   ggcorr(type = "lower", 
#          #lab = TRUE, 
#          #lab_size = 3, 
#          #method="circle", 
#          colors = c("tomato2", "white", "springgreen3"), 
#          title="Correlogram of Education/Income Data", 
#          ggtheme=theme_bw)

#-----Based on this chart, it looks like I should analyze:
# * pct_bsc_and_below ~ Pct_ED
# * pct_prof_and_above ~ taxable_income_pct
# * Pct_Black(BHN) ~ Pct_Suspended
# * Pct_Black(BHN) ~ joint_return_pct
# * Pct_Suspended(Expelled) ~ head_house_return_pct (joint_return_pct)

edu_IRS_data_2015_df %>%
  ggplot(aes(x = Pct_ED, y = pct_bsc_and_below)) +
  geom_point(na.rm = TRUE) +
  stat_smooth(method="lm")

edu_IRS_data_2015_df %>%
  ggplot(aes(x = taxable_income_pct, y = pct_prof_and_above)) +
  geom_point(na.rm = TRUE) +
  stat_smooth(method="lm")

edu_IRS_data_2015_df %>%
  ggplot(aes(x = Pct_BHN, y = Pct_Suspended)) +
  geom_point(na.rm = TRUE) +
  stat_smooth(method="lm")

edu_IRS_data_2015_df %>%
  ggplot(aes(x = Pct_BHN, y = joint_return_pct)) +
  geom_point(na.rm = TRUE) +
  stat_smooth(method="lm")

edu_IRS_data_2015_df %>%
  ggplot(aes(x = joint_return_pct, y = Pct_Suspended)) +
  geom_point(na.rm = TRUE) +
  stat_smooth(method="lm")

##############ANALYSIS OF PER PUPIL EXPENDITURES VS PCT OF PASSING TEST SCORES#########################
edu_IRS_data_2015_df %>%
  ggplot(aes(x = per_pupil_expend, y = pct_prof_and_above)) +
  geom_point(na.rm = TRUE)

##############ANALYSIS OF PCT ENROLLMENT ED VS PASSING TESTS#########################
edu_IRS_data_2015_df %>%
  ggplot(aes(x = Pct_ED, y = pct_prof_and_above)) +
  geom_point(na.rm = TRUE) +
  stat_smooth(method="lm")

##############ANALYSIS OF PCT PASSING TEST SCORES VS PCT ENROLLMENT ED#########################
edu_IRS_data_2015_df %>%
  ggplot(aes(x = pct_prof_and_above, y = Pct_ED)) +
  geom_point(na.rm = TRUE) +
  stat_smooth(method="lm")

#-------------------------------------------------------------------

#############Expenditures Per Pupil Compared to Average#######################
edu_IRS_data_2015_df$per_pupil_expend_z <- round((edu_IRS_data_2015_df$per_pupil_expend - mean(edu_IRS_data_2015_df$per_pupil_expend))/sd(edu_IRS_data_2015_df$per_pupil_expend), 2)  # compute normalized mpg
edu_IRS_data_2015_df$per_pupil_expend_type <- ifelse(edu_IRS_data_2015_df$per_pupil_expend_z < 0, "below", "above")  # above / below avg flag
edu_IRS_data_2015_df <- edu_IRS_data_2015_df[order(edu_IRS_data_2015_df$per_pupil_expend_z), ]  # sort
edu_IRS_data_2015_df$`county` <- factor(edu_IRS_data_2015_df$`county`, levels = edu_IRS_data_2015_df$`county`)  # convert to factor to retain sorted order in plot.

# Diverging Barcharts
ggplot(edu_IRS_data_2015_df, aes(x=`county`, y=per_pupil_expend_z, label=per_pupil_expend_z)) + 
  geom_bar(stat='identity', aes(fill=per_pupil_expend_type), width=.5)  +
  scale_fill_manual(name="Per Pupil Expenditures", 
                    labels = c("Above Average", "Below Average"), 
                    values = c("above"="#00ba38", "below"="#f8766d")) + 
  labs(subtitle="Normalized Per Pupil Expenditures") + 
  coord_flip()
