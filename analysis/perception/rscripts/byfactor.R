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

####################################
# Multinomial Logistic Regression
####################################

# Path to save or load the model
model <- save_or_load_model(
  model_path = "../model/all_ages_multinomial_logistic_regression_poa.rds",
  data = NULL,
  model_function = NULL)

model_without_interaction <- save_or_load_model(
  model_path = "../model/all_ages_multinomial_logistic_regression_poa_without_interaction.rds",
  data = NULL,
  model_function = NULL)

# Comparison

bf <- bayes_factor(model, model_without_interaction)
bf