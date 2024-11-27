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

maximum_age_in_young_group = 39
minimum_age_in_old_group = 60

# Youngest
young_group_data <- processed_data %>%
  filter(age <= maximum_age_in_young_group)

# Oldest
old_group_data <- processed_data %>%
  filter(age >= minimum_age_in_old_group)

#########
# PLOTS #
#########

temp1 = data_preprocessing2(young_group_data)
temp2 = data_preprocessing2(old_group_data)

f0_vot_rainbow_plot(
  data = temp1,
  title = "[Young Group] Mean responses across F0 and VOT continua",
  path = "../graphs/young_group_three_test.png"
)

f0_vot_rainbow_plot(
  data = temp2,
  title = "[Old Group] Mean responses across F0 and VOT continua",
  path = "../graphs/old_group_three_test.png"
)
