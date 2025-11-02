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

fit_brms_model <- function(data) {
  brm(
    formula = response ~ svot + sf0,
    data = data,
    family = categorical(link = "logit"),  # Multinomial logistic regression
    cores = 4,                             # Use multiple cores for faster computation
    threads = threading(6),
    iter = 2000,                           # Number of iterations (adjust as needed)
    control = list(adapt_delta = 0.95),     # Helps convergence for complex models
  )
}

####################################
# Multinomial Logistic Regression
####################################

# Set the reference level for the response variable
processed_data$response <- relevel(processed_data$response, ref = "lenis")

subject_list <- unique(processed_data$subject)
for (subj in subject_list) {
  data_name <- paste0("data_", subj)
  assign(data_name, processed_data[processed_data$subject == subj , ])
}


# Function to extract coefficients for individual participants
extract_coefficients_for_subject <- function(subject_id, subject_data) {
  cat("Processing subject:", subject_id, "\n")
  
  # Fit model for this subject
  model <- save_or_load_model(
    model_path = sprintf("../model/individual/%s.rds", subject_id),
    data = subject_data,
    model_function = fit_brms_model)
  
  # Extract coefficients
  tidy_model <- broom.mixed::tidy(model, effects = "fixed", conf.int = TRUE)
  
  # Get age for this subject
  subject_age <- subject_data$age[1]  # Take the first value since all should be identical
  
  # Extract coefficients for aspirate and tense responses
  coeffs <- tidy_model %>%
    filter(!str_detect(term, "Intercept")) %>%
    mutate(
      category = case_when(
        str_detect(term, "mutense") ~ "tense",
        str_detect(term, "muasp") ~ "asp",
        TRUE ~ "other"
      ),
      predictor = case_when(
        str_detect(term, "svot") ~ "vot",
        str_detect(term, "sf0") ~ "f0",
        TRUE ~ "other"
      )
    ) %>%
    filter(category %in% c("asp", "tense"), predictor %in% c("vot", "f0")) %>%
    select(category, predictor, estimate) %>%
    pivot_wider(names_from = c(category, predictor), values_from = estimate, names_sep = "_")
  
  # Return subject data with coefficients
  result <- data.frame(
    subject = subject_id,
    age = subject_age,
    asp_vot = ifelse("asp_vot" %in% names(coeffs), coeffs$asp_vot, NA),
    asp_f0 = ifelse("asp_f0" %in% names(coeffs), coeffs$asp_f0, NA),
    tense_vot = ifelse("tense_vot" %in% names(coeffs), coeffs$tense_vot, NA),
    tense_f0 = ifelse("tense_f0" %in% names(coeffs), coeffs$tense_f0, NA)
  )
  
  return(result)
}

# Run models for all subjects and extract coefficients
coefficients_data <- data.frame()

for (subj in subject_list) {
  subject_data <- processed_data[processed_data$subject == subj, ]
  
  tryCatch({
    subj_result <- extract_coefficients_for_subject(subj, subject_data)
    coefficients_data <- rbind(coefficients_data, subj_result)
  }, error = function(e) {
    cat("Error processing subject", subj, ":", e$message, "\n")
  })
}

# Calculate F0 reliance for aspirate contrast
coefficients_data <- coefficients_data %>%
  mutate(
    asp_f0_reliance = ifelse(
      !is.na(asp_f0) & !is.na(asp_vot),
      abs(asp_f0) / (abs(asp_f0) + abs(asp_vot)),
      NA
    )
  )

# Calculate F0 reliance for aspirate contrast
coefficients_data <- coefficients_data %>%
  mutate(
    fortis_f0_reliance = ifelse(
      !is.na(tense_f0) & !is.na(tense_vot),
      abs(tense_f0) / (abs(tense_f0) + abs(tense_vot)),
      NA
    )
  )
  
# Display summary
print(summary(coefficients_data))

output_path <- "../graphs/coefficients_data_no_random_effect.csv"
write.csv(coefficients_data, output_path, row.names = FALSE)

####################################
# Visualization: Age vs F0 Reliance
####################################

# Create scatter plot for aspirate F0 reliance
asp_f0_plot <- coefficients_data %>%
  filter(!is.na(asp_f0_reliance)) %>%
  ggplot(aes(x = age, y = asp_f0_reliance)) +
  geom_point(size = 3, alpha = 0.7, color = "#d62728") +
  geom_smooth(method = "lm", se = TRUE, color = "#1f77b4", fill = "#1f77b4", alpha = 0.2) +
  labs(
    x = "Age",
    y = "F0 Reliance for Aspirated Contrast",
    title = "Relationship between Age and F0 Reliance",
    subtitle = "F0 Reliance = |F0 coefficient| / (|F0 coefficient| + |VOT coefficient|)",
    caption = "Each point represents one participant"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 11),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  ) +
  scale_x_reverse() +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2))

# Print plot
print(asp_f0_plot)

# Save plot
ggsave(
  "../graphs/individual_without_random_effect.png",
  plot = asp_f0_plot,
  width = 8,
  height = 6,
  dpi = "retina"
)

# Create scatter plot for fortis F0 reliance
fortis_f0_plot <- coefficients_data %>%
  filter(!is.na(fortis_f0_reliance)) %>%
  ggplot(aes(x = age, y = fortis_f0_reliance)) +
  geom_point(size = 3, alpha = 0.7, color = "#d62728") +
  geom_smooth(method = "lm", se = TRUE, color = "#1f77b4", fill = "#1f77b4", alpha = 0.2) +
  labs(
    x = "Age",
    y = "F0 Reliance for Fortis Contrast",
    title = "Relationship between Age and F0 Reliance",
    subtitle = "F0 Reliance = |F0 coefficient| / (|F0 coefficient| + |VOT coefficient|)",
    caption = "Each point represents one participant"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 11),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  ) +
  scale_x_reverse() +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2))

# Print plot
print(fortis_f0_plot)

# Save plot
ggsave(
  "../graphs/individual_fortis_without_random_effect.png",
  plot = fortis_f0_plot,
  width = 8,
  height = 6,
  dpi = "retina"
)

sprintf("Minimum, Mean, and Maximum of Asperated f0 reliance: (%f / %f / %f)",
       min(coefficients_data$asp_f0_reliance), mean(coefficients_data$asp_f0_reliance
       ), max(coefficients_data$asp_f0_reliance
                                                   ))

sprintf("Minimum, Mean and Maximum of Fortis f0 reliance: (%f / %f / %f)",
        min(coefficients_data$fortis_f0_reliance), mean(coefficients_data$fortis_f0_reliance
        ), max(coefficients_data$fortis_f0_reliance
        ))
