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

# Preprocess the data
processed_data <- basic_data_preprocessing(
  workerids_csv_path = "../data/korean_stops_perception_3_poa_all_ages-workerids.csv",
  trials_csv_path = "../data/korean_stops_perception_3_poa_all_ages-trials.csv",
  subject_information_csv_path = "../data/korean_stops_perception_3_poa_all_ages-subject_information.csv"
)

# Count unique IDs for each gender
age_distrubtion <- processed_data %>%
  distinct(subject, age) %>%
  summarise(
    min_age = min(age),         # Minimum age
    max_age = max(age),         # Maximum age
    mean_age = mean(age),       # Average (mean) age
    median_age = median(age),   # Median age
    sd_age = sd(age),           # Standard deviation of age
    count = n()                 # Number of unique participants
  )
age_distrubtion

# Count unique IDs for each gender
gender_distribution <- processed_data %>%
  distinct(subject, gender) %>%  # Keep only distinct id and gender combinations
  group_by(gender) %>%     # Group by gender
  summarise(count = n())   # Count unique IDs for each gender
gender_distribution