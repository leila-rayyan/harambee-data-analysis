### Project Submission 1 ###
#remember to load all packages

                                 ############## QUESTION ###############
# Predict who is likely to be in work (in survey 1) so that they can intervene at ‘baseline’.
                                 
                                 
library(ggplot2)
library(tidyverse)
library(caret)
library(lubridate)
library(dplyr)
library(vcdExtra)
library(ISLR)
library(rpart)
library(party)
library(partykit)
library(rattle)
library(exegetic)                               
library(e1071)
library(mlbench)
  
df <- read.csv("data/raw/teaching_training_data.csv")
datacom = read.csv("data/raw/teaching_training_data_com.csv")
datanum = read.csv("data/raw/teaching_training_data_num.csv")
datagrit = read.csv("data/raw/teaching_training_data_grit.csv")
dataopt = read.csv("data/raw/teaching_training_data_opt.csv")
datacft = read.csv("data/raw/teaching_training_data_cft.csv")
view(dataopt)
view(datagrit)

### Merge Data ###
surv1 = filter(df, survey_num==1)
df2 = merge(surv1, datacom, by="unid")
df2 = df2 %>% distinct()
df3 = merge(df2, datanum, by="unid")
df3 = df3[,-2] %>% distinct()
datagrit = datagrit[,-1]
dataopt = dataopt[,-1]
datacft = datacft[,-1]
df4 = merge(df3, datagrit, by="unid")
view(df4)
df4 = df4[,-c(23,25)]  %>% distinct() 
df5 = merge(df4, dataopt, by="unid")
df5 = unique(df5)
view(df5)
data = merge(df5, datacft, by="unid")
data = unique(data)
### Clean Data ###
#Add Age data
data <- data %>% 
  mutate(age_at_survey = interval(dob, survey_date_month)/years(1)) %>% 
  mutate(age = floor(age_at_survey))
view(data)
#Drop unnecessary columns
data = select(data, -monthly_pay, -company_size, -job_start_date, -job_leave_date)
#Drop: company size column
#monthly pay
#job leave and start date
data2 = select(data, -unid, -dob, -survey_date_month, -peoplelive_15plus)
view(data2)
data3 = select(data2, -province)
view(data3)

#Take out Na's
d = na.omit(data3)
view(d)


#Build Trainig Model
# y variable = working
# x variables = everything else (for now)
fit <- glm(working~ ., data2, family = binomial)
summary(fit)
#should the variables be factors or variables?
### model output ###
# sig factors:
# survey_num (drop later)
# province Gauteng (make sure provinces is a character)
# province Mpumalanga
# provinceNoth West
# provinceNorther Cape
# provinceWestern Cape
# volunteerYes
# leadershiproleYes
# numchildren2
# anygrantTrue
# financial situtation_now3
# financial_situation_nrow6
# givemoney_yesTrue


# Test Model