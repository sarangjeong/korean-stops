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

maximum_age_in_young_group = 39
minimum_age_in_old_group = 60

control <- glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5))

fit_glmer_lenis_asp_function <- function(data) {
  glmer(
    asp ~ sf0 + svot + (1 + sf0 + svot | subject),
    data = data,
    family = "binomial",
    control = control
  )
}

fit_glmer_lenis_tense_function <- function(data) {
  glmer(
    tense ~ sf0 + svot + (1 + sf0 + svot | subject),
    data = data,
    family = "binomial",
    control = control
  )
}

fit_glmer_asp_tense_function <- function(data) {
  glmer(
    asp ~ sf0 + svot + (1 + sf0 + svot | subject),
    data = data,
    family = "binomial",
    control = control
  )
}

####################################
####################################
# Young Group
####################################
####################################

young_group.data <- processed_data %>%
  filter(age <= maximum_age_in_young_group)

# Preprocess for each model
young_group.lenis_asp_data <- young_group.data %>%
  filter(lenis==1 | asp==1) %>%
  mutate(response = as.character(response)) %>%
  mutate(response = as.factor(response))

young_group.lenis_tense_data <- young_group.data %>%
  filter(lenis==1 | tense==1) %>%
  mutate(response = as.character(response)) %>%
  mutate(response = as.factor(response))

young_group.asp_tense_data <- young_group.data %>%
  filter(asp==1 | tense==1) %>%
  mutate(response = as.character(response)) %>%
  mutate(response = as.factor(response))

# Fit model
young_group.lenis_asp_model <- save_or_load_model(
  data = young_group.lenis_asp_data,
  model_path = "../model/young_group/lenis_asp.rds",
  model_function = fit_glmer_lenis_asp_function,
  force=TRUE
)
summary(young_group.lenis_asp_model)

young_group.lenis_tense_model <- save_or_load_model(
  data = young_group.lenis_tense_data,
  model_path = "../model/young_group/lenis_tense.rds",
  model_function = fit_glmer_lenis_tense_function,
  force=TRUE
)
summary(young_group.lenis_tense_model)

young_group.asp_tense_model <- save_or_load_model(
  data = young_group.asp_tense_data,
  model_path = "../model/young_group/asp_tense.rds",
  model_function = fit_glmer_asp_tense_function,
  force=TRUE
)
summary(young_group.asp_tense_model)


####################################
####################################
# Oldest
####################################
####################################

# Oldest
old_group.data <- processed_data %>%
  filter(age >= minimum_age_in_old_group)

# Preprocess for each model
old_group.lenis_asp_data <- old_group.data %>%
  filter(lenis==1 | asp==1) %>%
  mutate(response = as.character(response)) %>%
  mutate(response = as.factor(response))

old_group.lenis_tense_data <- old_group.data %>%
  filter(lenis==1 | tense==1) %>%
  mutate(response = as.character(response)) %>%
  mutate(response = as.factor(response))

old_group.asp_tense_data <- old_group.data %>%
  filter(asp==1 | tense==1) %>%
  mutate(response = as.character(response)) %>%
  mutate(response = as.factor(response))

# Fit model
old_group.lenis_asp_model <- save_or_load_model(
  data = old_group.lenis_asp_data,
  model_path = "../model/old_group/lenis_asp.rds",
  model_function = fit_glmer_lenis_asp_function,
  force=TRUE
)
summary(old_group.lenis_asp_model)

old_group.lenis_tense_model <- save_or_load_model(
  data = old_group.lenis_tense_data,
  model_path = "../model/old_group/lenis_tense.rds",
  model_function = fit_glmer_lenis_tense_function,
  force=TRUE
)
summary(old_group.lenis_tense_model)

old_group.asp_tense_model <- save_or_load_model(
  data = old_group.asp_tense_data,
  model_path = "../model/old_group/asp_tense.rds",
  model_function = fit_glmer_asp_tense_function,
  force=TRUE
)
summary(old_group.asp_tense_model)

####################################
####################################
# Rainbow Plots
####################################
####################################

young_group.plot_data = plot_data_preprocessing(young_group.data)
young_group.rainbow_plot = f0_vot_rainbow_plot(
  data = young_group.plot_data,
  title = "Younger group's mean responses across F0 and VOT continua",
  path = "../graphs/young_group_three_test.png"
)
print(young_group.rainbow_plot)


old_group.plot_data = plot_data_preprocessing(old_group.data)
old_group.rainbow_plot = f0_vot_rainbow_plot(
  data = old_group.plot_data,
  title = "Older group's mean responses across F0 and VOT continua",
  path = "../graphs/old_group_three_test.png"
)
print(old_group.rainbow_plot)


####################################
####################################
# Lenis Plots
####################################
####################################
young_group.lenis_plot = f0_vot_lenis_plot(
  data = young_group.plot_data,
  title = "Younger group's lenis responses across F0 and VOT continua",
  path = "../graphs/young_group_lenis.png"
)
print(young_group.lenis_plot)

old_group.lenis_plot = f0_vot_lenis_plot(
  data = old_group.plot_data,
  title = "Older group's lenis responses across F0 and VOT continua",
  path = "../graphs/old_group_lenis.png"
)
print(old_group.lenis_plot)

####################################
####################################
# Tense Plots
####################################
####################################

young_group.tense_plot = f0_vot_tense_plot(
  data = young_group.plot_data,
  title = "Younger group's tense responses across F0 and VOT continua",
  path = "../graphs/young_group_tense.png"
)
print(young_group.tense_plot)

old_group.tense_plot = f0_vot_tense_plot(
  data = old_group.plot_data,
  title = "Older group's tense responses across F0 and VOT continua",
  path = "../graphs/old_group_tense.png"
)
print(old_group.tense_plot)

####################################
####################################
# Asp Plots
####################################
####################################

young_group.asp_plot = f0_vot_asp_plot(
  data = young_group.plot_data,
  title = "Younger group's aspirated responses across F0 and VOT continua",
  path = "../graphs/young_group_asp.png"
)
print(young_group.asp_plot)

old_group.asp_plot = f0_vot_asp_plot(
  data = old_group.plot_data,
  title = "Older group's aspirated responses across F0 and VOT continua",
  path = "../graphs/old_group_asp.png"
)
print(old_group.asp_plot)

####################################
####################################
# Bayesian Multinomial Logistic Regression with Mixed Effects
####################################
####################################
# Categorize Age into two age groups (old, young)
# old = participants who are older than or equal to `minimum_age_in_old_group`
# young = participants who are younger than or equal to `maximum_age_in_young_group`
# rows of participants in other age groups will be discarded.
age_groups.data <- processed_data %>%
  mutate(age_group = case_when(
    age >= minimum_age_in_old_group ~ "old",
    age <= maximum_age_in_young_group ~ "young",
    TRUE ~ NA_character_  # Assign NA to rows outside the desired age groups
  )) %>%
  filter(!is.na(age_group)) %>%  # Remove rows with NA age_group
  mutate(age_group = factor(age_group))

# Set `lenis` as reference level
age_groups.data$response <- relevel(age_groups.data$response, ref = "lenis")

fit_brms_model <- function(data) {
  brm(
    formula = response ~ svot * age_group + sf0 * age_group + (1 + svot + sf0 | subject),
    data = data,
    family = categorical(link = "logit"),  # Multinomial logistic regression
    cores = 4,                             # Use multiple cores for faster computation
    threads = threading(6),
    iter = 2000,                           # Number of iterations (adjust as needed)
    control = list(adapt_delta = 0.95)     # Helps convergence for complex models
  )
}

# Path to save or load the model
model <- save_or_load_model(
  model_path = "../model/old_group_vs_young_group_multinomial_logistic_regression.rds",
  data = age_groups.data,
  model_function = fit_brms_model)
summary(model)
