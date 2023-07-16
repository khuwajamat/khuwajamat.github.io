---
layout: post
title: "Machine Learning Newborns Data Modelling"
---

{% highlight ruby %}
###########################################################
#                    Machine Learning 1             
#                 NEWBORNS_DATASET   
#                  Regression Models
#                 academic year 2022/2023                 
#                 Duae Mariam (455855)                        #
########################################################### 



setwd("D:/data science course/2 Sem/ML/_assessment/data")
library(dplyr)
library(readr)
library(ggplot2)

#loading the datasets
newborns <- read_csv("newborn_train.csv")


test_data <- read_csv("newborn_test.csv")


#######################
#Data cleaning
#######################
#step 1
#encoding the categorical value with integers for training sample
#converting nominal variables to numeric variables
newborns$newborn_gender<- factor(newborns$newborn_gender, levels = c("M", "F"), 
                                 labels = c(0,1), ordered = TRUE)
#table(newborns$newborn_gender)
newborns$previous_cesarean<- factor(newborns$previous_cesarean, levels = c("Y", "N"), 
                                    labels = c(0,1), ordered = TRUE)

##################
#Step 2
#encoding the categorical value with integers for test sample

test_data$newborn_gender<- factor(test_data$newborn_gender, levels = c("M", "F"), 
                                  labels = c(0,1), ordered = TRUE)
test_data$previous_cesarean<- factor(test_data$previous_cesarean, levels = c("Y", "N"), 
                                     labels = c(0,1), ordered = TRUE)



##################
#step 3

####replacing NA with the mean and mode of data of the variable for training data
library(DescTools)
hist(newborns$number_prenatal_visits)
newborns$mother_body_mass_index[is.na(newborns$mother_body_mass_index)] <-median(newborns$mother_body_mass_index, na.rm = TRUE)
newborns$mother_marital_status[is.na(newborns$mother_marital_status)] <- Mode(newborns$mother_marital_status, na.rm = TRUE)
newborns$mother_delivery_weight[is.na(newborns$mother_delivery_weight)] <- median(newborns$mother_delivery_weight, na.rm = TRUE)
newborns$mother_race[is.na(newborns$mother_race)] <- Mode(newborns$mother_race, na.rm = TRUE)
newborns$mother_height[is.na(newborns$mother_height)] <- mean(newborns$mother_height, na.rm = TRUE)
newborns$mother_weight_gain[is.na(newborns$mother_weight_gain)] <- mean(newborns$mother_weight_gain, na.rm = TRUE)
newborns$father_age[is.na(newborns$father_age)] <-mean(newborns$father_age, na.rm = TRUE)
newborns$father_education[is.na(newborns$father_education)] <- mean(newborns$father_education, na.rm = TRUE)
newborns$cigarettes_before_pregnancy[is.na(newborns$cigarettes_before_pregnancy)] <-Mode(newborns$cigarettes_before_pregnancy, na.rm = TRUE)
newborns$prenatal_care_month[is.na(newborns$prenatal_care_month)] <- mean(newborns$prenatal_care_month, na.rm = TRUE)
newborns$number_prenatal_visits[is.na(newborns$number_prenatal_visits)] <- Mode(newborns$number_prenatal_visits, na.rm = TRUE)
newborns$previous_cesarean[is.na(newborns$previous_cesarean)] <- 1
newborns$newborn_gender[is.na(newborns$newborn_gender)] <-1

#Rplacing the missing values with the mean of the variable

test_data$mother_body_mass_index[is.na(test_data$mother_body_mass_index)] <-median(test_data$mother_body_mass_index, na.rm = TRUE)
test_data$mother_marital_status[is.na(test_data$mother_marital_status)] <-Mode(test_data$mother_marital_status, na.rm = TRUE)
test_data$mother_delivery_weight[is.na(test_data$mother_delivery_weight)] <- median(test_data$mother_delivery_weight, na.rm = TRUE)
test_data$mother_race[is.na(test_data$mother_race)] <- Mode(test_data$mother_race, na.rm = TRUE)
test_data$mother_height[is.na(test_data$mother_height)] <- mean(test_data$mother_height, na.rm = TRUE)
test_data$mother_weight_gain[is.na(test_data$mother_weight_gain)] <- mean(test_data$mother_weight_gain, na.rm = TRUE)
test_data$father_age[is.na(test_data$father_age)] <- mean(test_data$father_age, na.rm = TRUE)
test_data$father_education[is.na(test_data$father_education)] <- mean(test_data$father_education, na.rm = TRUE)
test_data$cigarettes_before_pregnancy[is.na(test_data$cigarettes_before_pregnancy)] <- Mode(test_data$cigarettes_before_pregnancy, na.rm = TRUE)
test_data$prenatal_care_month[is.na(test_data$prenatal_care_month)] <- mean(test_data$prenatal_care_month, na.rm = TRUE)
test_data$number_prenatal_visits[is.na(test_data$number_prenatal_visits)] <- Mode(test_data$number_prenatal_visits, na.rm = TRUE)
test_data$previous_cesarean[is.na(test_data$previous_cesarean)] <- 1
test_data$newborn_gender[is.na(test_data$newborn_gender)] <-1


##################
#step 4

#converting data to numeric for training sample 

for (col in 1:(ncol(newborns))) {
  if (!is.numeric(newborns[,col])) 
    newborns[,col] <-lapply(newborns[,col], as.numeric)
  
}
#converting data to numeric for test_data 
for (col in 1:(ncol(test_data))) {
  if (!is.numeric(test_data[,col])) 
    test_data[,col] <-lapply(test_data[,col], as.numeric)
  
}


##################
#step 5
#checking correlation

library(corrplot)

cor_matrix <- cor(newborns)

corrplot(cor_matrix, method = "circle")

# we take correlations with the newborn_weight and sort the dataset according to correlation
newborns_numeric_vars_order <- 
  cor_matrix[,"newborn_weight"] %>% 
  sort(decreasing = TRUE) %>%
  names()

corrplot(cor_matrix[newborns_numeric_vars_order,newborns_numeric_vars_order]
         , method = "circle")


#so here we see in the graph there is strong correlation between the
# mothers body_mass index and mothers delivery weight 
#This could be eliminated, however, i find that this attribute is still important to reduce MAPE


#However, we do eliminate the attributes, which have almost 0 correlation with the dependent variable.
#this must be done in order to reduce AIC and BIC, as they have additional weight on model, but no significant results

#columns to be removed
#"previous_cesarean","father_education","father_age" "prenatal_care_month"
columns<- c(7,8,10,12)
newborns <- newborns[,-columns]

#making the test data similar to training
test_data <- test_data[,-columns]

##################


#step 6
#checking outliers
# after analysis of all the variables with plotting and graphs , we can see the outliersrs exist after data
#data points for newborn_weight greater than 6800, so we eleminate all the rows where newborn_weight is greater
#than 6800, hence we remove outliers.

plot(newborns$newborn_weight, newborns$mother_weight_gain)

( newborn_weights_which_outliers <- which(newborns$newborn_weight > 6800) )

newborns  <- newborns[-newborn_weights_which_outliers,]

#I also found that the Pre_natal_care column has invalid value as 99.   But we know that column is not important for my analysis.


##################
#step7
#Scaling
# Load required libraries
library(dplyr)



# View the scaled dataset
names(newborns)
scale(newborns$mother_body_mass_index)
scale(newborns$mother_marital_status)
scale(newborns$mother_delivery_weight)
scale(newborns$mother_race)
scale(newborns$mother_height)
scale(newborns$mother_weight_gain)
scale(newborns$number_prenatal_visits)
scale(newborns$newborn_gender)
scale(newborns$cigarettes_before_pregnancy)



scale(test_data$mother_body_mass_index)
scale(test_data$mother_marital_status)
scale(test_data$mother_delivery_weight)
scale(test_data$mother_race)
scale(test_data$mother_height)
scale(test_data$mother_weight_gain)
scale(test_data$number_prenatal_visits)
scale(test_data$newborn_gender)
scale(test_data$cigarettes_before_pregnancy)

#Step 8
#########################################

# I have tried to run multiple models with different variations of the input parameters
#the following models are the ones which resulted in least value for MAPE

# Linear regression model
# MAPE=0.1796298 
# 
# Ridge Regression model
# MAPE=0.1794363
# 
# LASSO Regression model
# MAPE=0.1795213
# 
# Xgboost Regression Model 
# MAPE=0.1830804    


#but in the end, i chose to go with Lg boost model, for which the full code is appended.
############

# Function regressionMetrics is defines for easily assessing the model's prediction results
regressionMetrics <- function(real, newborns_predictions) {
  # Mean Square Error
  MSE <- mean((real - newborns_predictions)^2)
  # Root Mean Square Error
  RMSE <- sqrt(MSE)
  # Mean Absolute Error
  MAE <- mean(abs(real - newborns_predictions))
  # Mean Absolute Percentage Error
  MAPE <- mean(abs(real - newborns_predictions)/real)
  # R2
  R2 <- cor(newborns_predictions, real)^2
  
  result <- data.frame(MSE, RMSE, MAE, MAPE, R2)
  return(result)
}

###################################

#Model 5 Lgboostmodel
# Install and load the necessary packages
install.packages("lightgbm")
library(lightgbm)

# Split the training data into features (X) and labels (Y)
X_train <- newborns[, -10]  # Exclude the target variable
Y_train <- newborns$newborn_weight

# Create the LGBM dataset
train_dataset <- lgb.Dataset(data = as.matrix(X_train), label = Y_train)

# Set the LGBM parameters
params <- list(
  objective = "huber",
  num_iterations = 50
)


lgb_model <- lgb.train(params, train_dataset)
names(test_data)
# Make predictions on the test data using the trained model
newborns_predictions5 <- predict(lgb_model, as.matrix(test_data[]))

# Print the error using function defined above

print(regressionMetrics(real = real,newborns_predictions = newborns_predictions5))



#########################################################

#saving the results from lgbmodel in csv

file_path <- "results.csv"

# Save the data frame as a CSV file
write.csv(newborns_predictions5, file = file_path, row.names = FALSE)
{% endhighlight %}
