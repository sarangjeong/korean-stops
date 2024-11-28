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

save_or_load_model <- function(model_path, fit_model_function) {
  # Ensure the directory exists
  model_dir <- dirname(model_path)
  if (!dir.exists(model_dir)) {
    dir.create(model_dir, recursive = TRUE)
  }
  
  if (!file.exists(model_path)) {
    cat("Model file not found. Fitting the model...\n")
    model <- fit_model_function()  # Call the function to fit the model
    saveRDS(model, model_path)     # Save the model to the specified path
  } else {
    cat("Model file found. Loading the saved model...\n")
    model <- readRDS(model_path)  # Load the saved model
  }
  return(model)
}

binary_logistic_regression <- function(
    lenis_asp_data,
    lenis_tense_data,
    asp_tense_data,
    model_dir_path = "../model/glmer_model"
) {
  control <- glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5))
  
  # Define model-specific fit functions
  fit_lenis_asp <- function() {
    glmer(
      asp ~ sf0 * sage + svot * sage + (1 + sf0 + svot | subject),
      data = lenis_asp_data,
      family = "binomial",
      control = control
    )
  }
  
  fit_lenis_tense <- function() {
    glmer(
      tense ~ sf0 * sage + svot * sage + (1 + sf0 + svot | subject),
      data = lenis_tense_data,
      family = "binomial",
      control = control
    )
  }
  
  fit_asp_tense <- function() {
    glmer(
      asp ~ sf0 * sage + svot * sage + (1 + sf0 + svot | subject),
      data = asp_tense_data,
      family = "binomial",
      control = control
    )
  }
  
  # Paths for each model
  lenis_asp_path <- file.path(model_dir_path, "lenis_asp_model.rds")
  lenis_tense_path <- file.path(model_dir_path, "lenis_tense_model.rds")
  asp_tense_path <- file.path(model_dir_path, "asp_tense_model.rds")
  
  # Save or load each model
  cat("Lenis vs Asp...\n")
  lenis_asp_model <- save_or_load_model(lenis_asp_path, fit_lenis_asp)
  cat("Lenis vs Tense...\n")
  lenis_tense_model <- save_or_load_model(lenis_tense_path, fit_lenis_tense)
  cat("Asp vs Tense...\n")
  asp_tense_model <- save_or_load_model(asp_tense_path, fit_asp_tense)
  
  # Return all models as a list
  return(list(
    lenis_asp_model = lenis_asp_model,
    lenis_tense_model = lenis_tense_model,
    asp_tense_model = asp_tense_model
  ))
}

multinomial_logistic_regression <- function(
    data,
    model_path = "../model/brms_model.rds"
) {
  fit_model <- function() {
    brm(
      formula = response ~ svot * sage + sf0 * sage + (1 + svot + sf0 | subject),
      data = data,
      family = categorical(link = "logit"),  # Multinomial logistic regression
      cores = 4,                             # Use multiple cores for faster computation
      threads = threading(4),
      iter = 2000,                           # Number of iterations (adjust as needed)
      control = list(adapt_delta = 0.95)     # Helps convergence for complex models
    )
  }
  
  brm_model <- save_or_load_model(model_path, fit_model)
  return(brm_model)
}