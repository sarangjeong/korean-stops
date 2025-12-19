# Rainbow plots by session (1-9)
# created on December 9, 2025

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
# Assign session numbers to each trial
####################################

# Each worker has:
# - Slides 1-16: practice/check (excluded)
# - Slides 17-80: Session 1 (64 slides)
# - Slides 81-144: Session 2 (64 slides)
# - ... and so on for 9 sessions total

processed_data <- processed_data %>%
  group_by(subject) %>%
  arrange(subject, slide_number_in_experiment) %>%
  mutate(
    # Calculate session number based on slide number
    # Slides 1-16 are excluded (practice/check)
    # Slides 17-80 = Session 1, 81-144 = Session 2, etc.
    session = case_when(
      slide_number_in_experiment <= 16 ~ NA_integer_,  # Exclude practice/check
      slide_number_in_experiment >= 17 & slide_number_in_experiment <= 80 ~ 1L,
      slide_number_in_experiment >= 81 & slide_number_in_experiment <= 144 ~ 2L,
      slide_number_in_experiment >= 145 & slide_number_in_experiment <= 208 ~ 3L,
      slide_number_in_experiment >= 209 & slide_number_in_experiment <= 272 ~ 4L,
      slide_number_in_experiment >= 273 & slide_number_in_experiment <= 336 ~ 5L,
      slide_number_in_experiment >= 337 & slide_number_in_experiment <= 400 ~ 6L,
      slide_number_in_experiment >= 401 & slide_number_in_experiment <= 464 ~ 7L,
      slide_number_in_experiment >= 465 & slide_number_in_experiment <= 528 ~ 8L,
      slide_number_in_experiment >= 529 & slide_number_in_experiment <= 592 ~ 9L,
      TRUE ~ NA_integer_
    )
  ) %>%
  ungroup() %>%
  filter(!is.na(session))  # Remove practice/check slides

# Display session distribution
cat("Session distribution:\n")
print(table(processed_data$session))
cat("\n")

####################################
# Create rainbow plots for each session
####################################

# Create subdirectory for session plots
dir.create("../graphs/rainbow_plots/sessions", recursive = TRUE, showWarnings = FALSE)

for (sess in 1:9) {
  session_data <- processed_data %>%
    filter(session == sess)
  
  if (nrow(session_data) > 0) {
    session_plot_data <- plot_data_preprocessing(session_data)
    
    rainbow_plot <- f0_vot_rainbow_plot(
      data = session_plot_data,
      title = paste("Mean responses across F0 and VOT continua - Session", sess),
      path = paste0("../graphs/rainbow_plots/sessions/rainbow_plot_session_", sess, ".png")
    )
    
    cat(sprintf("Created plot for Session %d (n = %d trials)\n", sess, nrow(session_data)))
  } else {
    cat(sprintf("Warning: No data for Session %d\n", sess))
  }
}

print("All session rainbow plots have been generated!")

