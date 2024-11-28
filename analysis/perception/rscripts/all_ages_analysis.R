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

####################################
# Multinomial Logistic Regression
####################################

# Set the reference level for the response variable
processed_data$response <- relevel(processed_data$response, ref = "lenis")

# Path to save or load the model
model <- multinomial_logistic_regression(data = processed_data, model_path = "../model/brms_model.rds")
summary(model)

####################################
# Multiple Binary Logistic Regressions
####################################

# Preprocess for each model
lenis_asp_data <- processed_data %>%
  filter(lenis==1 | asp==1) %>%
  mutate(response = as.character(response)) %>%
  mutate(response = as.factor(response))

lenis_tense_data <- processed_data %>%
  filter(lenis==1 | tense==1) %>%
  mutate(response = as.character(response)) %>%
  mutate(response = as.factor(response))

asp_tense_data <- processed_data %>%
  filter(asp==1 | tense==1) %>%
  mutate(response = as.character(response)) %>%
  mutate(response = as.factor(response))

# Fit model
model <- binary_logistic_regression(
  lenis_asp_data = lenis_asp_data,
  lenis_tense_data = lenis_tense_data,
  asp_tense_data = asp_tense_data,
  model_dir_path = "../model/all_ages_binary_logistic_regression")

summary(model$lenis_asp_model)
summary(model$lenis_tense_model)
summary(model$asp_tense_model)
