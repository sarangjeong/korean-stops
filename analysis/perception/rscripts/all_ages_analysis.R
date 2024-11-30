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
  c(tidy_model_filtered$conf.low, tidy_model_filtered$conf.high),
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
