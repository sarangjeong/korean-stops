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

# Preprocess the data
processed_data <- basic_data_preprocessing(
  workerids_csv_path = "../data/korean_stops_perception_3_poa_all_ages-workerids.csv",
  trials_csv_path = "../data/korean_stops_perception_3_poa_all_ages-trials.csv",
  subject_information_csv_path = "../data/korean_stops_perception_3_poa_all_ages-subject_information.csv"
)

# 20-39: young, 40-59: middle, 60+: old
# Create `age_cat` column from `age` column
MAXIMUM_YOUNG_AGE = 39
MAXIMUM_MIDDLE_AGE = 59

young_group.data <- processed_data %>%
  filter(age <= MAXIMUM_YOUNG_AGE)

middle_group.data <- processed_data %>%
  filter(age <= MAXIMUM_MIDDLE_AGE) %>%
  filter(age > MAXIMUM_YOUNG_AGE)

old_group.data <- processed_data %>%
  filter(age > MAXIMUM_MIDDLE_AGE)


####################################
####################################
# Rainbow Plots
####################################
####################################

young_group.plot_data = plot_data_preprocessing(young_group.data)

young_group.rainbow_plot = f0_vot_rainbow_plot(
  data = young_group.plot_data,
  title = "Younger group's mean responses across F0 and VOT continua",
  path = "../graphs/young_group_three.png"
)
print(young_group.rainbow_plot)

middle_group.plot_data = plot_data_preprocessing(middle_group.data)

middle_group.rainbow_plot = f0_vot_rainbow_plot(
  data = middle_group.plot_data,
  title = "Intermediate group's mean responses across F0 and VOT continua",
  path = "../graphs/middle_group_three.png"
)
print(middle_group.rainbow_plot)

old_group.plot_data = plot_data_preprocessing(old_group.data)
old_group.rainbow_plot = f0_vot_rainbow_plot(
  data = old_group.plot_data,
  title = "Older group's mean responses across F0 and VOT continua",
  path = "../graphs/old_group_three.png"
)
print(old_group.rainbow_plot)


