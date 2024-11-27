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
library(brms)
source("preprocessing.R")
source("plot.R")

processed_data = basic_data_preprocessing(
  workerids_csv_path = "../data/korean_stops_perception_3_poa_all_ages-workerids.csv",
  trials_csv_path = "../data/korean_stops_perception_3_poa_all_ages-trials.csv",
  subject_information_csv_path = "../data/korean_stops_perception_3_poa_all_ages-subject_information.csv"
)

processed_data$response <- relevel(processed_data$response, ref = "lenis")

brm_model <- brm(
  formula = response ~ svot * sage + sf0 * sage + (1 + svot + sf0 | subject),
  data = processed_data,
  family = categorical(link = "logit"),  # Multinomial logistic regression
  cores = 4,                             # Use multiple cores for faster computation
  iter = 2000,                           # Number of iterations (adjust as needed)
  control = list(adapt_delta = 0.95)     # Helps convergence for complex models
)
summary(brm_model)
