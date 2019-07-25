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
data3 = select(data2, -province)
#Take out survey_num column because it is 1 for all the data set. 
d = select(data3, -survey_num)
#Take out Na's
d = na.omit(d)

view(d)


### Visualizing data ###

#historgram of the age of working people
workingpeople  = filter(d, working==T)
ggplot(workingpeople, aes(x = age)) + geom_histogram()
mean(workingpeople$age)
#histogram of the age of nonworking people
nonworking = filter(d, working==F)
ggplot(nonworking, aes(x = age,)) + geom_histogram()
mean(nonworking$age)
#shows that mean age for working and nonworking people is around the same. Therefore, age would probably be a bad predictor of working status. 



### Build Trainig Model ###
# y variable = working
# x variables = everything else (for now)
set.seed(17)
train_index = sample(c(T, F), nrow(cars), prob = c(0.8, 0.2), replace = TRUE)
d_train <- d[train_index,]
d_test <- d[-train_index,]

fit <- glm(working~ ., data2, family = binomial)
summary(fit)

fit2 <- (titanic.rpart <- train(working ~ ., data = d, method = "rpart", trControl = TRAINCONTROL,
                                metric = "Accuracy", na.action = na.omit, tuneGrid = expand.grid(cp = seq(0.000, 0.02, 0.0025))))

fancyRpartPlot(fit2$finalModel, sub="Decision Tree Titanic")


trControl <- trainControl(method = "cv", number = 10, verboseIter = TRUE)
model_rpart <- train(working ~ ., data=d_train, method='rpart', trControl = trControl)
model_rpart <- train(working ~ ., data=d_train, method='rpart', trControl = trControl, 
                     tuneGrid = expand.grid(cp = seq(0.000, 0.02, 0.0025)))
new_model <- train(working ~ ., data = d_train, method="ctree", trControl= trControl)
#Compare the two models using resamples
model_comp <- resamples(list(my_new_model = new_model, Rpart = model_rpart))
summary(model_comp)
new_model$results
model_rpart$results




# Test Model