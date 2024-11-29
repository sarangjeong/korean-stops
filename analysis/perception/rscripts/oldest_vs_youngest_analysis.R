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
source("plot.R")

# Preprocess the data
processed_data <- basic_data_preprocessing(
  workerids_csv_path = "../data/korean_stops_perception_3_poa_all_ages-workerids.csv",
  trials_csv_path = "../data/korean_stops_perception_3_poa_all_ages-trials.csv",
  subject_information_csv_path = "../data/korean_stops_perception_3_poa_all_ages-subject_information.csv"
)

minimum_age = 20
maximum_age = 72

fit_glm_lenis_asp_function <- function(data) {
  glm(
    asp ~ sf0 + svot,
    data = data,
    family = "binomial",
  )
}

fit_glm_lenis_tense_function <- function(data) {
  glm(
    tense ~ sf0 + svot,
    data = data,
    family = "binomial",
  )
}

fit_glm_asp_tense_function <- function(data) {
  glm(
    asp ~ sf0 + svot,
    data = data,
    family = "binomial",
  )
}

####################################
####################################
# Youngest
####################################
####################################

youngest.data <- processed_data %>%
  filter(age == minimum_age)

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
youngest.lenis_asp_model <- save_or_load_model(
  data = youngest.lenis_asp_data,
  model_path = "../model/youngest/lenis_asp.rds",
  model_function = fit_glm_lenis_asp_function,
)
summary(youngest.lenis_asp_model)

youngest.lenis_tense_model <- save_or_load_model(
  data = youngest.lenis_tense_data,
  model_path = "../model/youngest/lenis_tense.rds",
  model_function = fit_glm_lenis_tense_function
)
summary(youngest.lenis_tense_model)

youngest.asp_tense_model <- save_or_load_model(
  data = youngest.asp_tense_data,
  model_path = "../model/youngest/asp_tense.rds",
  model_function = fit_glm_asp_tense_function
)
summary(youngest.asp_tense_model)


####################################
####################################
# Oldest
####################################
####################################

# Oldest
oldest.data <- processed_data %>%
  filter(age == maximum_age)

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
oldest.lenis_asp_model <- save_or_load_model(
  data = oldest.lenis_asp_data,
  model_path = "../model/oldest/lenis_asp.rds",
  model_function = fit_glm_lenis_asp_function,
)
summary(oldest.lenis_asp_model)

oldest.lenis_tense_model <- save_or_load_model(
  data = oldest.lenis_tense_data,
  model_path = "../model/oldest/lenis_tense.rds",
  model_function = fit_glm_lenis_tense_function
)
summary(oldest.lenis_tense_model)

oldest.asp_tense_model <- save_or_load_model(
  data = oldest.asp_tense_data,
  model_path = "../model/oldest/asp_tense.rds",
  model_function = fit_glm_asp_tense_function
)
summary(oldest.asp_tense_model)

####################################
####################################
# Rainbow Plots
####################################
####################################

youngest.plot_data = plot_data_preprocessing(youngest.data)
youngest.rainbow_plot = f0_vot_rainbow_plot(
  data = youngest.plot_data,
  title = "Youngest participant's mean responses across F0 and VOT continua",
  path = "../graphs/youngest_three_test.png"
)
print(youngest.rainbow_plot)


oldest.plot_data = plot_data_preprocessing(oldest.data)
oldest.rainbow_plot = f0_vot_rainbow_plot(
  data = oldest.plot_data,
  title = "Oldest participant's mean responses across F0 and VOT continua",
  path = "../graphs/oldest_three_test.png"
)
print(oldest.rainbow_plot)
