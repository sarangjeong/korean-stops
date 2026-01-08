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
library(broom.mixed)
library(patchwork)
source("preprocessing.R")
source("plot.R")
source("models.R")
library(bayestestR)
library(posterior)
library(loo)

model1 <- save_or_load_model(
  model_path = "../models/all_ages_multinomial_logistic_regression.rds",
  data = NULL,
  model_function = NULL
)

model2 <- save_or_load_model(
  model_path = "../models/all_ages_multinomial_logistic_regression_poa_gender.rds",
  data = NULL,
  model_function = NULL
)

model3 <- save_or_load_model(
  model_path = "../models/all_ages_multinomial_logistic_regression_poa_gender_random_poa.rds",
  data = NULL,
  model_function = NULL
)

model4 <- save_or_load_model(
  model_path = "../model/all_ages_multinomial_logistic_regression_vot_f0_age_interaction.rds",
  data = NULL,
  model_function = NULL
)

model5 <- save_or_load_model(
  model_path = "../model/all_ages_multinomial_logistic_regression_poa_gender_interaction_of_vot_f0_age_gender.rds",
  data = NULL,
  model_function = NULL
)

summary(model3)
loo1 <- loo(model1)
loo2 <- loo(model2)
loo3 <- loo(model3)
loo4 <- loo(model4)
loo5 <- loo(model5)

loo_compare(loo1, loo2) # model 2 is much better
# elpd_diff se_diff
# model2     0.0       0.0
# model1 -1107.5      46.6

loo_compare(loo2, loo3) 
loo_compare(loo1, loo4)
loo_compare(loo3, loo4)
loo_compare(loo3, loo5) # model5 is better by 23.2 (se = 9.6)
