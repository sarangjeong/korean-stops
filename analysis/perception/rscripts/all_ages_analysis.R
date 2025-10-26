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
    formula = response ~ svot * sage + sf0 * sage + (1 + svot + sf0 | subject),
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
  model_path = "../model/all_ages_multinomial_logistic_regression.rds",
  data = processed_data,
  model_function = fit_brms_model)
summary(model)
# Extract model summaries into a tidy data frame
tidy_model <- broom.mixed::tidy(model, effects = "fixed", conf.int = TRUE)

# Define a color palette
row_colors <- c(
  "Vot:Age" = "#1f77b4",
  "Vot" = "#ff7f0e",
  "F0" = "#2ca02c",
  "Age:F0" = "#d62728",
  "Age" = "#9467bd"
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
      str_replace_all("svot", "vot") %>%  # Remove "s" from "svot"
      str_replace_all("sf0", "f0") %>%      # Remove "s" from "sf0"
      str_replace_all("sage", "age"),      # Remove "s" from "sage"
    term = str_to_title(term)  # Capitalize first letter of each word
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
  ggplot(aes(x = term, y = estimate, color = term)) +  # Switched x and y
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +  # Adjusted for switched axes
  scale_color_manual(values = row_colors) +
  scale_y_continuous(limits = y_limits, oob = scales::squish) +  # Set consistent y-axis range
  theme_minimal() +
  labs(
    x = "Fixed effect",
    y = "Coefficient",
    title = "Forest Plot: Tense vs. Aspirate Coefficients",
    subtitle = "Note: 'Vot' and 'F0' are scaled variables.",
    caption = "Error bars represent 95% confidence intervals"
  ) +
  facet_wrap(~category) +  # Facet for separation
  theme(legend.position = "none")  # Remove legend

# Print the forest plot
print(forest_plot)
ggsave(
  "../graphs/all_ages_bayesian_analysis.png",
  plot = forest_plot,
  scale = 1,
  width = 7,
  height = 4,
  dpi = "retina",
)

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
rainbow_plot = f0_vot_rainbow_plot(
  data = data_for_rainbow_plot,
  title = "Mean responses across F0 and VOT continua",
  path = "../graphs/all_ages_three.png"
)
print(rainbow_plot)



