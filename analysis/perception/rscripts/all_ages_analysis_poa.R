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
library(bayestestR)
library(posterior)

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
    formula = response ~ svot + sf0 + poa + svot:sage + sf0:sage + (1 + svot + sf0 | subject),
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
  model_path = "../models/all_ages_multinomial_logistic_regression_poa.rds",
  data = processed_data,
  model_function = fit_brms_model)
summary(model)

p_direction(model)

# Extract model summaries into a tidy data frame
tidy_model <- broom.mixed::tidy(model, effects = "fixed", conf.int = TRUE)
summary(tidy_model)

# Define a color palette
row_colors <- c(
  "Age:VOT" = "#1f77b4",
  "VOT" = "#ff7f0e",
  "f0" = "#2ca02c",
  "Age:f0" = "#d62728",
  "Labial" = "#9467bd",
  "Dorsal" = "pink",
  "Insignificant" = "#808080"
)

# Filter out intercepts and prepare data
tidy_model_filtered <- tidy_model %>%
  filter(!str_detect(term, "Intercept")) %>%  # Exclude intercepts
  mutate(
    raw_term = term,
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
      str_replace_all("sage", "Age") %>%
      str_replace_all("poalab", "Labial") %>%    # Labial PoA
      str_replace_all("poador", "Dorsal"),      # Dorsal PoA
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

# Ordered list of fixed effects that appear in the model
term_levels <- c("VOT", "f0", "Labial", "Dorsal", "Age:VOT", "Age:f0")
term_levels <- term_levels[term_levels %in% tidy_model_filtered$term]
if (length(term_levels) == 0) {
  term_levels <- unique(tidy_model_filtered$term)
}

# Determine y-axis range for both facets
y_limits <- range(
  c(tidy_model_filtered$conf.low - 0.5, tidy_model_filtered$conf.high + 0.5),
  na.rm = TRUE
)
y_limits <- c(min(y_limits, -abs(y_limits)), max(y_limits, abs(y_limits)))  # Symmetric range

# Build individual forest plots for Fortis and Aspirated contrasts
build_forest_plot <- function(data, panel_title) {
  data %>%
    ggplot(aes(x = term, y = estimate, color = term_color)) +
    geom_hline(yintercept = 0, linewidth = 0.8, color = "grey80") +
    geom_point(size = 2) +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
    scale_color_manual(values = row_colors) +
    scale_x_discrete(limits = term_levels) +
    scale_y_continuous(limits = y_limits, oob = scales::squish) +
    theme_minimal() +
    labs(
      x = "Fixed effect",
      y = "Coefficient",
      title = panel_title,
      subtitle = "Note: 'VOT' and 'f0' are scaled variables.",
      caption = "Error bars represent 95% confidence intervals"
    ) +
    theme(
      legend.position = "none",
      text = element_text(size = 14),
      axis.title = element_text(size = 14),
      axis.text = element_text(size = 12),
      plot.title = element_text(size = 16),
      plot.subtitle = element_text(size = 12)
    )
}

forest_plot_fortis <- tidy_model_filtered %>%
  filter(category == "Tense") %>%
  build_forest_plot(panel_title = "Forest Plot: Fortis Coefficients")

forest_plot_aspirated <- tidy_model_filtered %>%
  filter(category == "Aspirate") %>%
  build_forest_plot(panel_title = "Forest Plot: Aspirated Coefficients")

print(forest_plot_fortis)
print(forest_plot_aspirated)

ggsave(
  "../graphs/all_ages_bayesian_analysis_forest_fortis.png",
  plot = forest_plot_fortis,
  scale = 1,
  width = 7,
  height = 6,
  dpi = "retina"
)

ggsave(
  "../graphs/all_ages_bayesian_analysis_forest_aspirated.png",
  plot = forest_plot_aspirated,
  scale = 1,
  width = 7,
  height = 6,
  dpi = "retina"
)

browseURL("../graphs/all_ages_bayesian_analysis_forest_fortis.png")
browseURL("../graphs/all_ages_bayesian_analysis_forest_aspirated.png")

# Posterior draws for coefficient-level box plots
posterior_draws <- as_draws_df(model)

tidy_lookup <- tidy_model_filtered %>%
  filter(category %in% c("Tense", "Aspirate")) %>%
  select(raw_term, term, category) %>%
  distinct()

posterior_term_names <- colnames(posterior_draws)

tidy_lookup <- tidy_lookup %>%
  mutate(
    raw_term = if_else(
      raw_term %in% posterior_term_names,
      raw_term,
      if_else(
        paste0("b_", raw_term) %in% posterior_term_names,
        paste0("b_", raw_term),
        raw_term
      )
    )
  ) %>%
  filter(raw_term %in% posterior_term_names)

extract_draws <- function(lookup_df, label) {
  posterior_draws %>%
    select(all_of(lookup_df$raw_term)) %>%
    pivot_longer(cols = everything(), names_to = "raw_term", values_to = "draw") %>%
    left_join(lookup_df, by = "raw_term") %>%
    mutate(
      term = factor(term, levels = term_levels),
      comparison = label
    ) %>%
    select(term, draw, comparison)
}

aspirated_lookup <- tidy_lookup %>%
  filter(category == "Aspirate") %>%
  select(raw_term, term)

fortis_lookup <- tidy_lookup %>%
  filter(category == "Tense") %>%
  select(raw_term, term)

aspirated_draws <- extract_draws(aspirated_lookup, "Aspirated/Lenis")
fortis_draws <- extract_draws(fortis_lookup, "Fortis/Lenis")

difference_lookup <- tidy_lookup %>%
  select(term, category, raw_term) %>%
  pivot_wider(names_from = category, values_from = raw_term) %>%
  drop_na()

difference_draws <- difference_lookup %>%
  mutate(draw = purrr::map2(Aspirate, Tense, ~ posterior_draws[[.x]] - posterior_draws[[.y]])) %>%
  select(term, draw) %>%
  unnest(cols = c(draw)) %>%
  mutate(
    term = factor(term, levels = term_levels),
    comparison = "Aspirated/Fortis"
  ) %>%
  select(term, draw, comparison)

posterior_boxplot_data <- bind_rows(aspirated_draws, fortis_draws, difference_draws) %>%
  filter(!is.na(term)) %>%
  mutate(
    comparison = factor(comparison, levels = c("Aspirated/Lenis", "Fortis/Lenis", "Aspirated/Fortis"))
  )

box_abs_limit <- max(abs(posterior_boxplot_data$draw), na.rm = TRUE)
if (!is.finite(box_abs_limit) || box_abs_limit == 0) {
  box_abs_limit <- 0.5
}
box_y_limits <- c(-box_abs_limit, box_abs_limit)

comparison_labels <- c(
  "Aspirated/Lenis" = "Aspirated/Lenis (K)",
  "Fortis/Lenis" = "Fortis/Lenis (K)",
  "Aspirated/Fortis" = "Aspirated/Fortis (K)"
)

term_display <- c(
  "VOT" = "VOT",
  "f0" = "f0",
  "Labial" = "Labial",
  "Dorsal" = "Dorsal",
  "Age:VOT" = "Age:VOT",
  "Age:f0" = "Age:f0"
)

box_plot <- posterior_boxplot_data %>%
  mutate(term = forcats::fct_recode(term, !!!term_display)) %>%
  ggplot(aes(x = term, y = draw)) +
  geom_hline(yintercept = 0, color = "grey40", linewidth = 0.6, linetype = "dashed") +
  geom_boxplot(width = 0.6, fill = "grey80", color = "black", outlier.shape = 21, outlier.size = 1.8, outlier.fill = "white") +
  scale_y_continuous(limits = box_y_limits, breaks = scales::pretty_breaks(n = 5), oob = scales::squish) +
  facet_wrap(~comparison, nrow = 1, scales = "free_y", labeller = labeller(comparison = comparison_labels)) +
  theme_classic() +
  labs(
    x = "Fixed effect",
    y = "Posterior coefficient",
    title = "Posterior Contrasts by Stop Category",
    subtitle = "Grey boxes summarise draws from the fitted brms model.",
    caption = "Dashed line marks zero effect."
  ) +
  theme(
    legend.position = "none",
    text = element_text(size = 14),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 14, face = "bold"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    panel.grid = element_blank()
  )

print(box_plot)

ggsave(
  "../graphs/all_ages_bayesian_analysis_boxplot.png",
  plot = box_plot,
  scale = 1,
  width = 9,
  height = 3.5,
  dpi = "retina"
)

browseURL("../graphs/all_ages_bayesian_analysis_boxplot.png")

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
browseURL("../graphs/all_ages_three.png")

