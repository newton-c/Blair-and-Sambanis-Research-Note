
set.seed(92382)
# Clear environment
    
rm(list = ls())
    
# Set working directory
setwd("~/Desktop/blair&sambanis2020_rn/replication-3")

library(haven) # import .dta files
library(pROC) # ROC curves
library(randomForest) # it's kinda obvious, isn't it?

library(gbm) # gradient boosting
library(e1071) # naive bayes

# Define 1-month models -------------------------------------------------------

# Load data
data <- read_dta("data/1mo_data.dta")

# Define training and testing sets for base specification
train_period = mean(data$period[which(data$month == 12 & data$year == 2007)])
end_period = mean(data$period[which(data$month == 12 & data$year == 2015)])
train <- data[data$period <= train_period,]
test <- data[data$period > train_period & data$period <= end_period,]

train_DV_civil_ns <- train$incidence_civil_ns_plus1
test_DV_civil_ns <- test$incidence_civil_ns_plus1

rm(list = c("data", "train_period", "end_period"))

source("code_new/1mo_define_base_models.R")

# Original ROC plot -----------------------------------------------------------
source("code_new/original_roc_plot.R")

# Comparing gradient boosted to random forest models --------------------------
source("code_new/gb_vs_rf_roc_plot.R")

# Comparing naive bayes to random forest models -------------------------------
source("code_new/naive_bayes_roc_plot.R")
