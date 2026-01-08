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

sprintf("The number of `asp` rows: %d", nrow(processed_data[processed_data$response == "asp", ]))
sprintf("The number of `tense` rows: %d", nrow(processed_data[processed_data$response == "tense", ]))
sprintf("The number of `lenis` rows: %d", nrow(processed_data[processed_data$response == "lenis", ]))
sprintf("The number of rows: %d", nrow(processed_data))

fit_brms_model <- function(data) {
  brm(
    formula = response ~ svot * sf0 * sage + (1 + svot + sf0 | subject),
    data = data,
    family = categorical(link = "logit"),  # Multinomial logistic regression
    cores = 4,                             # Use multiple cores for faster computation
    threads = threading(6),
    iter = 2000,                           # Number of iterations (adjust as needed)
    control = list(adapt_delta = 0.95)     # Helps convergence for complex models
  )
}

####################################
# Multinomial Logistic Regression
####################################

# Set the reference level for the response variable
processed_data$response <- relevel(processed_data$response, ref = "lenis")

# Path to save or load the model
model <- save_or_load_model(
  model_path = "../model/all_ages_multinomial_logistic_regression_vot_f0_age_interaction.rds",
  data = processed_data,
  model_function = fit_brms_model)
summary(model)
# Extract model summaries into a tidy data frame
tidy_model <- broom.mixed::tidy(model, effects = "fixed", conf.int = TRUE)

# Define a color palette
row_colors <- c(
  "Age:VOT" = "#1f77b4",
  "VOT" = "#ff7f0e",
  "f0" = "#2ca02c",
  "Age:f0" = "#d62728",
  "Age" = "#9467bd",
  "Insignificant" = "#808080"
)

# Filter out intercepts and prepare data
tidy_model_filtered <- tidy_model %>%
  filter(!str_detect(term, "Intercept")) %>%  # Exclude intercepts
  mutate(
    category = case_when(
      str_detect(term, "mutense") ~ "Tense",
      str_detect(term, "muasp") ~ "Aspirate",
      TRUE ~ "Other"
    ),
    # Simplify term names by removing "mu" and replacing underscores
    term = str_replace_all(term, "_", "") %>%
      str_remove_all("mutense") %>%
      str_remove_all("muasp") %>%
      str_replace_all("svot", "VOT") %>%  # VOT in uppercase
      str_replace_all("sf0", "f0") %>%      # f0 in lowercase
      str_replace_all("sage", "Age"),      # Age capitalized
    # Reorder interaction terms to have Age first
    term = case_when(
      str_detect(term, "VOT:Age") ~ str_replace(term, "VOT:Age", "Age:VOT"),
      str_detect(term, "f0:Age") ~ str_replace(term, "f0:Age", "Age:f0"),
      TRUE ~ term
    ),
    # Mark insignificant coefficients
    is_significant = (conf.low > 0 & conf.high > 0) | (conf.low < 0 & conf.high < 0),
    term_color = ifelse(is_significant, term, "Insignificant")
  )

# Determine y-axis range for both facets
y_limits <- range(
  c(tidy_model_filtered$conf.low - 0.5, tidy_model_filtered$conf.high + 0.5),
  na.rm = TRUE
)
y_limits <- c(min(y_limits, -abs(y_limits)), max(y_limits, abs(y_limits)))  # Symmetric range

# Combined forest plot with distinct row colors and fixed y-axis range
forest_plot <- tidy_model_filtered %>%
  filter(category %in% c("Tense", "Aspirate")) %>%
  ggplot(aes(x = term, y = estimate, color = term_color)) +
  geom_hline(yintercept = 0, linewidth = 0.8, color = "grey80") +  # Thicker zero line in light grey
  geom_point(size = 2) +  # Smaller points
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  scale_color_manual(values = row_colors) +
  scale_x_discrete(limits = c("VOT", "f0", "Age", "Age:VOT", "Age:f0")) +  # Fixed order
  scale_y_continuous(limits = y_limits, oob = scales::squish) +
  theme_minimal() +
  labs(
    x = "Fixed effect",
    y = "Coefficient",
    title = "Forest Plot: Fortis vs. Aspirated Coefficients",
    subtitle = "Note: 'VOT' and 'f0' are scaled variables.",
    caption = "Error bars represent 95% confidence intervals"
  ) +
  facet_wrap(
    ~category,
    labeller = as_labeller(c("Tense" = "Fortis", "Aspirate" = "Aspirated"))
  ) +
  theme(
    legend.position = "none",
    text = element_text(size = 14),  # Larger font size
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    plot.title = element_text(size = 16),
    plot.subtitle = element_text(size = 12),
    strip.text = element_text(size = 14)
  )

# Print the forest plot
print(forest_plot)
ggsave(
  "../graphs/all_ages_bayesian_analysis.png",
  plot = forest_plot,
  scale = 1,
  width = 7,
  height = 6,  # Increased height
  dpi = "retina",
)
browseURL("../graphs/all_ages_bayesian_analysis.png")

####################################
# Plot the random effects

# You can see the random effect per each participant
# tidy_random_effects <- broom.mixed::tidy(model, effects = "ran_vals", conf.int = TRUE)
model.random_effect.result <- broom.mixed::tidy(model, effects = "ran_pars", conf.int = TRUE)

# Filter out intercepts and prepare data
model.random_effect.filtered_result <- model.random_effect.result %>%
  filter(!str_detect(term, "cor")) %>%  # Exclude correlation
  mutate(
    category = case_when(
      str_detect(term, "mutense") ~ "Tense",
      str_detect(term, "muasp") ~ "Aspirated",
      TRUE ~ "Other"
    ),
    # Simplify term names by removing unnecessary parts
    term = str_replace_all(term, "_", "") %>%
      str_remove_all("sd") %>%
      str_remove_all("mutense") %>%
      str_remove_all("muasp") %>%
      str_replace_all("svot", "VOT") %>%  # Remove "s" from "svot"
      str_replace_all("sf0", "f0") %>%
      str_remove_all("[()]"),  # Remove parentheses
    term = str_to_title(term),  # Capitalize first letter of each word
    term = str_replace_all(term, "Vot", "VOT")
  )

model.random_effect.color_mape <- c(
  "VOT" = "#1f77b4",
  "Intercept" = "#00000e",
  "F0" = "#2ca02c"
)

# Determine y-axis range for both facets
y_limits <- range(
  c(model.random_effect.filtered_result$conf.low, model.random_effect.filtered_result$conf.high),
  na.rm = TRUE
)
model.random_effect.y_limits <- c(0, ceiling(max(abs(y_limits))))  # Round up to next integer

# Change the order of the x-axis labels
model.random_effect.plot <- model.random_effect.filtered_result %>%
  filter(category %in% c("Tense", "Aspirated")) %>%
  ggplot(aes(x = term, y = estimate, color = term)) +  # Switched x and y
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +  # Adjusted for switched axes
  scale_color_manual(values = model.random_effect.color_mape) +
  scale_x_discrete(limits = c("F0", "VOT", "Intercept")) +  # Set x-axis order
  scale_y_continuous(
    limits = model.random_effect.y_limits, 
    breaks = seq(0, model.random_effect.y_limits[2], by = 1),  # Integer breaks
    oob = scales::squish
  ) +  # Set consistent y-axis range with integer ticks
  theme_minimal() +
  labs(
    x = "Random effect",
    y = "SD",
    title = "Bayesian Analysis Result - Random Effects",
    subtitle = "Note: 'Vot' and 'F0' are scaled variables.",
    caption = "Error bars represent 95% confidence intervals"
  ) +
  facet_wrap(~category) +  # Facet for separation
  theme(legend.position = "none")  # Remove legend

# Print the forest plot
print(model.random_effect.plot)

ggsave(
  "../graphs/all_ages_bayesian_analysis_random_effect.png",
  plot = model.random_effect.plot,
  scale = 1,
  width = 5,
  height = 3,
  dpi = "retina",
)

####################################
# Rainbow Plot 
####################################
data_for_rainbow_plot = plot_data_preprocessing(processed_data)
view(data_for_rainbow_plot)
rainbow_plot = f0_vot_rainbow_plot(
  data = data_for_rainbow_plot,
  title = "Mean responses across F0 and VOT continua",
  path = "../graphs/all_ages_three.png"
)
browseURL("../graphs/all_ages_three.png")


library(bayesplot)

# Caterpillar plot for all random effects
# Get all parameter names that start with "r_subject"
all_random_effects <- names(model$fit)[grepl("^r_subject__", names(model$fit))]

# Separate by effect type and category
intercept_pars <- all_random_effects[grepl("Intercept", all_random_effects)]
vot_pars <- all_random_effects[grepl("svot", all_random_effects)]
f0_pars <- all_random_effects[grepl("sf0", all_random_effects)]

# Function to sort parameters by median (ascending order)
sort_by_median <- function(pars) {
  post <- as.matrix(model)
  medians <- apply(post[, pars], 2, median)
  pars[order(medians, decreasing = FALSE)]
}

# Function to sort parameters by age
sort_by_age <- function(pars) {
  # Extract subject IDs from parameter names
  subject_ids <- as.numeric(gsub(".*\\[(\\d+),.*", "\\1", pars))
  
  # Get age information for each subject
  age_data <- processed_data %>%
    select(subject, age) %>%
    distinct() %>%
    mutate(subject = as.numeric(as.character(subject)))  # Convert factor to numeric
  
  # Create a data frame with parameter names and ages
  par_age <- data.frame(
    par = pars,
    subject = subject_ids
  ) %>%
    left_join(age_data, by = "subject") %>%
    arrange(age)
  
  return(par_age$par)
}

# Sort parameters by median
intercept_pars_sorted <- sort_by_median(intercept_pars)
vot_pars_sorted <- sort_by_median(vot_pars)
f0_pars_sorted <- sort_by_median(f0_pars)

# Sort parameters by age
intercept_pars_age_sorted <- sort_by_age(intercept_pars)
vot_pars_age_sorted <- sort_by_age(vot_pars)
f0_pars_age_sorted <- sort_by_age(f0_pars)

# Create caterpillar plots with sorted parameters
p1 <- mcmc_intervals(model, pars = intercept_pars_sorted,
                     prob = 0.5,
                     prob_outer = 0.95,
                     point_size = 0.3) +  # Very small points
  coord_flip() +
  labs(title = "Random Effects: Intercept",
       x = "Parameter estimate",
       y = "Participant") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

p2 <- mcmc_intervals(model, pars = vot_pars_sorted,
                     prob = 0.5,
                     prob_outer = 0.95,
                     point_size = 0.3) +  # Very small points
  coord_flip() +
  labs(title = "Random Effects: VOT",
       x = "Parameter estimate",
       y = "Participant") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

p3 <- mcmc_intervals(model, pars = f0_pars_sorted,
                     prob = 0.5,
                     prob_outer = 0.95,
                     point_size = 0.3) +  # Very small points
  coord_flip() +
  labs(title = "Random Effects: F0",
       x = "Parameter estimate",
       y = "Participant") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

# Combine plots horizontally
combined_random_effects <- p1 | p2 | p3

# Print and save
print(combined_random_effects)

ggsave(
  "../graphs/all_ages_random_effects_caterpillar.png",
  plot = combined_random_effects,
  scale = 1,
  width = 15,
  height = 6,
  dpi = "retina"
)

browseURL("../graphs/all_ages_random_effects_caterpillar.png")

# Create caterpillar plots sorted by age
p1_age <- mcmc_intervals(model, pars = intercept_pars_age_sorted,
                         prob = 0.5,
                         prob_outer = 0.95,
                         point_size = 0.3) +
  coord_flip() +
  labs(title = "Random Effects: Intercept (by Age)",
       x = "Parameter estimate",
       y = "Participant") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

p2_age <- mcmc_intervals(model, pars = vot_pars_age_sorted,
                         prob = 0.5,
                         prob_outer = 0.95,
                         point_size = 0.3) +
  coord_flip() +
  labs(title = "Random Effects: VOT (by Age)",
       x = "Parameter estimate",
       y = "Participant") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

p3_age <- mcmc_intervals(model, pars = f0_pars_age_sorted,
                         prob = 0.5,
                         prob_outer = 0.95,
                         point_size = 0.3) +
  coord_flip() +
  labs(title = "Random Effects: F0 (by Age)",
       x = "Parameter estimate",
       y = "Participant") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

# Combine age-sorted plots
combined_random_effects_age <- p1_age | p2_age | p3_age

# Print and save
print(combined_random_effects_age)

ggsave(
  "../graphs/all_ages_random_effects_caterpillar_by_age.png",
  plot = combined_random_effects_age,
  scale = 1,
  width = 15,
  height = 6,
  dpi = "retina"
)

browseURL("../graphs/all_ages_random_effects_caterpillar_by_age.png")



