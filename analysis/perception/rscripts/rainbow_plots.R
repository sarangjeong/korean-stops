# Rainbow plots for different groupings
# created on November 28, 2025

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

# Preprocess the data
processed_data <- basic_data_preprocessing(
  workerids_csv_path = "../data/korean_stops_perception_3_poa_all_ages-workerids.csv",
  trials_csv_path = "../data/korean_stops_perception_3_poa_all_ages-trials.csv",
  subject_information_csv_path = "../data/korean_stops_perception_3_poa_all_ages-subject_information.csv"
)

####################################
# 1. Rainbow plots by subject
####################################
# Create subdirectory for subject plots
dir.create("../graphs/rainbow_plots/subjects", recursive = TRUE, showWarnings = FALSE)

subjects <- unique(processed_data$subject)

for (subj in subjects) {
  subj_data <- processed_data %>%
    filter(subject == subj)
  
  # Get subject's age
  subj_age <- unique(subj_data$age)[1]
  
  subj_plot_data <- plot_data_preprocessing(subj_data)
  
  rainbow_plot <- f0_vot_rainbow_plot(
    data = subj_plot_data,
    title = paste("Mean responses across F0 and VOT continua - Subject", subj, "(Age:", subj_age, ")"),
    path = paste0("../graphs/rainbow_plots/subjects/rainbow_plot_", subj_age, "_", subj, ".png")
  )
  print(paste("Created plot for subject:", subj, "Age:", subj_age))
}

####################################
# 2. Rainbow plots by POA
####################################
# Create subdirectory for rainbow plots
dir.create("../graphs/rainbow_plots", recursive = TRUE, showWarnings = FALSE)

poa_levels <- unique(processed_data$poa)

for (poa_level in poa_levels) {
  poa_data <- processed_data %>%
    filter(poa == poa_level)
  
  poa_plot_data <- plot_data_preprocessing(poa_data)
  
  rainbow_plot <- f0_vot_rainbow_plot(
    data = poa_plot_data,
    title = paste("Mean responses across F0 and VOT continua - POA:", poa_level),
    path = paste0("../graphs/rainbow_plots/rainbow_plot_poa_", poa_level, ".png")
  )
  print(paste("Created plot for POA:", poa_level))
}

####################################
# 3. Rainbow plots by gender x age
####################################
# 20-39: young, 40-59: middle, 60+: old
# Create `age_cat` column from `age` column
MAXIMUM_YOUNG_AGE = 39
MAXIMUM_MIDDLE_AGE = 59

processed_data <- processed_data %>%
  mutate(age_group = case_when(
    age <= MAXIMUM_YOUNG_AGE ~ "young",
    age > MAXIMUM_YOUNG_AGE & age <= MAXIMUM_MIDDLE_AGE ~ "middle",
    age > MAXIMUM_MIDDLE_AGE ~ "old"
  ))

gender_levels <- unique(processed_data$gender)
age_group_levels <- c("young", "middle", "old")

for (gen in gender_levels) {
  for (age_grp in age_group_levels) {
    gender_age_data <- processed_data %>%
      filter(gender == gen, age_group == age_grp)
    
    if (nrow(gender_age_data) > 0) {
      gender_age_plot_data <- plot_data_preprocessing(gender_age_data)
      
      rainbow_plot <- f0_vot_rainbow_plot(
        data = gender_age_plot_data,
        title = paste("Mean responses - Gender:", gen, "/ Age:", age_grp),
        path = paste0("../graphs/rainbow_plots/rainbow_plot_gender_", gen, "_age_", age_grp, ".png")
      )
      print(paste("Created plot for Gender:", gen, "/ Age:", age_grp))
    }
  }
}

####################################
# 4. Rainbow plots by POA x gender x age
####################################
for (poa_level in poa_levels) {
  for (gen in gender_levels) {
    for (age_grp in age_group_levels) {
      poa_gender_age_data <- processed_data %>%
        filter(poa == poa_level, gender == gen, age_group == age_grp)
      
      if (nrow(poa_gender_age_data) > 0) {
        poa_gender_age_plot_data <- plot_data_preprocessing(poa_gender_age_data)
        
        rainbow_plot <- f0_vot_rainbow_plot(
          data = poa_gender_age_plot_data,
          title = paste("Mean responses - POA:", poa_level, "/ Gender:", gen, "/ Age:", age_grp),
          path = paste0("../graphs/rainbow_plots/rainbow_plot_poa_", poa_level, "_gender_", gen, "_age_", age_grp, ".png")
        )
        print(paste("Created plot for POA:", poa_level, "/ Gender:", gen, "/ Age:", age_grp))
      }
    }
  }
}

print("All rainbow plots have been generated!")

