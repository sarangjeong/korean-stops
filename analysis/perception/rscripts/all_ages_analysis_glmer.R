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
library(nnet)
library(broom)

source("preprocessing.R")
source("models.R")
source("plot.R")

# Preprocess the data
processed_data <- basic_data_preprocessing(
  workerids_csv_path = "../data/korean_stops_perception_3_poa_all_ages-workerids.csv",
  trials_csv_path = "../data/korean_stops_perception_3_poa_all_ages-trials.csv",
  subject_information_csv_path = "../data/korean_stops_perception_3_poa_all_ages-subject_information.csv"
)

processed_data$response <- relevel(processed_data$response, ref = "lenis")

fit_multinom_function <- function(data) {
  multinom(
    response ~ sf0 * svot * sage,
    data = data,
    maxit = 500
  )
}

model <- save_or_load_model(
  data = processed_data,
  model_path = "../model/all_ages/nnet_all_interactions.rds",
  model_function = fit_multinom_function,
  force=TRUE
)

summary(model)

tidy(model)
