# Analysis of experimental results of
# Korean stop contrast, perception, young (pilot)
# created by Sarang Jeong on June 6, 2021

##########
# set-up #
##########

# set working directory
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

# load libraries
library(tidyverse)
source("preprocessing.R")
source("models.R")

# Preprocess the data
processed_data <- basic_data_preprocessing(
  workerids_csv_path = "../data/korean_stops_perception_3_poa_all_ages-workerids.csv",
  trials_csv_path = "../data/korean_stops_perception_3_poa_all_ages-trials.csv",
  subject_information_csv_path = "../data/korean_stops_perception_3_poa_all_ages-subject_information.csv"
)

minimum_age = 20
maximum_age = 72

####################################
####################################
# Youngest
####################################
####################################

youngest.data <- processed_data %>%
  filter(age == minimum_age)

# Multinomial Logistic Regression
youngest.data$response <- relevel(youngest.data$response, ref = "lenis")
youngest.mlr_model <- multinomial_logistic_regression(data = youngest.data, model_path = "../model/youngest/multinomial_logistic_regression.rds")
summary(youngest.mlr_model)

####################################
# Multiple Binary Logistic Regressions
####################################

# Preprocess for each model
youngest.lenis_asp_data <- youngest.data %>%
  filter(lenis==1 | asp==1) %>%
  mutate(response = as.character(response)) %>%
  mutate(response = as.factor(response))

youngest.lenis_tense_data <- youngest.data %>%
  filter(lenis==1 | tense==1) %>%
  mutate(response = as.character(response)) %>%
  mutate(response = as.factor(response))

youngest.asp_tense_data <- youngest.data %>%
  filter(asp==1 | tense==1) %>%
  mutate(response = as.character(response)) %>%
  mutate(response = as.factor(response))

# Fit model
youngest.blr_model <- binary_logistic_regression(
  lenis_asp_data = youngest.lenis_asp_data,
  lenis_tense_data = youngest.lenis_tense_data,
  asp_tense_data = youngest.asp_tense_data,
  model_dir_path = "../model/youngest/binary_logistic_regression")

summary(youngest.blr_model$lenis_asp_model)
summary(youngest.blr_model$lenis_tense_model)
summary(youngest.blr_model$asp_tense_model)

####################################
####################################
# Oldest
####################################
####################################

# Oldest
oldest.data <- processed_data %>%
  filter(age == maximum_age)

# Multinomial Logistic Regression
oldest.data$response <- relevel(oldest.data$response, ref = "lenis")
oldest.mlr_model <- multinomial_logistic_regression(data = oldest.data, model_path = "../model/oldest/multinomial_logistic_regression.rds")
summary(oldest.mlr_model)

####################################
# Multiple Binary Logistic Regressions
####################################

# Preprocess for each model
oldest.lenis_asp_data <- oldest.data %>%
  filter(lenis==1 | asp==1) %>%
  mutate(response = as.character(response)) %>%
  mutate(response = as.factor(response))

oldest.lenis_tense_data <- oldest.data %>%
  filter(lenis==1 | tense==1) %>%
  mutate(response = as.character(response)) %>%
  mutate(response = as.factor(response))

oldest.asp_tense_data <- oldest.data %>%
  filter(asp==1 | tense==1) %>%
  mutate(response = as.character(response)) %>%
  mutate(response = as.factor(response))

# Fit model
oldest.blr_model <- binary_logistic_regression(
  lenis_asp_data = oldest.lenis_asp_data,
  lenis_tense_data = oldest.lenis_tense_data,
  asp_tense_data = oldest.asp_tense_data,
  model_dir_path = "../model/youngest/binary_logistic_regression")

summary(youngest.blr_model$lenis_asp_model)
summary(youngest.blr_model$lenis_tense_model)
summary(youngest.blr_model$asp_tense_model)
