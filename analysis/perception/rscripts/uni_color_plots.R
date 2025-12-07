# Uni-color plots for age-based groupings
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

########################################
# 1. Prepare age-group-specific datasets
########################################
MAXIMUM_YOUNG_AGE <- 39
MAXIMUM_MIDDLE_AGE <- 59

processed_data <- processed_data %>%
  mutate(age_group = case_when(
    age <= MAXIMUM_YOUNG_AGE ~ "young",
    age > MAXIMUM_YOUNG_AGE & age <= MAXIMUM_MIDDLE_AGE ~ "middle",
    age > MAXIMUM_MIDDLE_AGE ~ "old"
  ))

age_group_levels <- c("young", "middle", "old")
age_group_labels <- c(young = "Young", middle = "Middle", old = "Old")

########################################
# 2. Generate uni-color plots by age group
########################################
# Create subdirectories for each response type
base_output_dir <- "../graphs/uni_color_plots"
dir.create(base_output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(base_output_dir, "asp"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(base_output_dir, "fortis"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(base_output_dir, "lenis"), recursive = TRUE, showWarnings = FALSE)

for (group_key in age_group_levels) {
  group_data <- processed_data %>%
    filter(age_group == group_key)

  if (nrow(group_data) == 0) {
    next
  }

  group_plot_data <- plot_data_preprocessing(group_data)
  readable_age_group <- age_group_labels[[group_key]]

  # Aspirated responses
  f0_vot_asp_plot(
    data = group_plot_data,
    title = paste("Aspirated responses - Age group:", readable_age_group),
    path = file.path(base_output_dir, "asp", paste0("asp_plot_", group_key, ".png"))
  )
  print(paste("Created aspirated plot for age group:", readable_age_group))

  # Fortis responses
  f0_vot_tense_plot(
    data = group_plot_data,
    title = paste("Fortis responses - Age group:", readable_age_group),
    path = file.path(base_output_dir, "fortis", paste0("fortis_plot_", group_key, ".png"))
  )
  print(paste("Created fortis plot for age group:", readable_age_group))

  # Lenis responses
  f0_vot_lenis_plot(
    data = group_plot_data,
    title = paste("Lenis responses - Age group:", readable_age_group),
    path = file.path(base_output_dir, "lenis", paste0("lenis_plot_", group_key, ".png"))
  )
  print(paste("Created lenis plot for age group:", readable_age_group))
}

print("All uni-color plots have been generated!")

