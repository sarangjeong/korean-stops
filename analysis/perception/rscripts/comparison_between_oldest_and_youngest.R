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
source("plot.R")

processed_data = data_preprocessing(
  workerids_csv_path = "../data/korean_stops_perception_3_poa_all_ages-workerids.csv",
  trials_csv_path = "../data/korean_stops_perception_3_poa_all_ages-trials.csv",
  subject_information_csv_path = "../data/korean_stops_perception_3_poa_all_ages-subject_information.csv"
)

minimum_age = 20
maximum_age = 72

# Youngest
youngest_data <- processed_data %>%
  filter(age == minimum_age)

# Oldest
oldest_data <- processed_data %>%
  filter(age == maximum_age)

#########
# PLOTS #
#########

youngest_groupped_data = data_preprocessing2(youngest_data)
oldest_groupped_data = data_preprocessing2(oldest_data)

f0_vot_rainbow_plot(
  data = youngest_groupped_data,
  title = "[Youngest] Mean responses across F0 and VOT continua",
  path = "../graphs/youngest_three_test.png"
)

f0_vot_rainbow_plot(
  data = oldest_groupped_data,
  title = "[Oldest] Mean responses across F0 and VOT continua",
  path = "../graphs/oldest_three_test.png"
)
