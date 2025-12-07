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
    formula = response ~ poa + svot*sage*gender + sf0*sage*gender + (1 + svot + sf0 | subject),
    data = data,
    family = categorical(link = "logit"),
    cores = 4,
    threads = threading(6),
    iter = 2000,
    control = list(adapt_delta = 0.95),
    save_pars = save_pars(all = TRUE)
  )
}

####################################
# Multinomial Logistic Regression
####################################

# Set the reference level for the response variable
processed_data$response <- relevel(processed_data$response, ref = "lenis")

# Path to save or load the model
model <- save_or_load_model(
  model_path = "../models/all_ages_multinomial_logistic_regression_poa_gender.rds",
  data = processed_data,
  model_function = fit_brms_model
)
summary(model)

p_direction(model)

prior_summary(model)

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
  filter(!str_detect(term, "Intercept")) %>%
  mutate(
    raw_term = term,
    category = case_when(
      str_detect(term, "mutense") ~ "Tense",
      str_detect(term, "muasp") ~ "Aspirate",
      TRUE ~ "Other"
    ),
    term = str_replace_all(term, "_", "") %>%
      str_remove_all("mutense") %>%
      str_remove_all("muasp") %>%
      str_replace_all("svot", "VOT") %>%
      str_replace_all("sf0", "f0") %>%
      str_replace_all("sage", "Age") %>%
      str_replace_all("genderMale", "Gender") %>%
      str_replace_all("poalab", "Labial") %>%
      str_replace_all("poador", "Dorsal"),
    term = case_when(
      str_detect(term, "VOT:Age:Gender") ~ str_replace(term, "VOT:Age:Gender", "Age:VOT:Gender"),
      str_detect(term, "f0:Age:Gender") ~ str_replace(term, "f0:Age:Gender", "Age:f0:Gender"),
      str_detect(term, "VOT:Age") ~ str_replace(term, "VOT:Age", "Age:VOT"),
      str_detect(term, "f0:Age") ~ str_replace(term, "f0:Age", "Age:f0"),
      str_detect(term, "VOT:Gender") ~ str_replace(term, "VOT:Gender", "Gender:VOT"),
      str_detect(term, "f0:Gender") ~ str_replace(term, "f0:Gender", "Gender:f0"),
      TRUE ~ term
    ),
    is_significant = (conf.low > 0 & conf.high > 0) | (conf.low < 0 & conf.high < 0),
    term_color = if_else(is_significant, term, "Insignificant")
  )

term_levels <- c("VOT", "f0", "Labial", "Dorsal", "Gender", "Age:VOT", "Age:f0", "Gender:VOT", "Gender:f0", "Age:VOT:Gender", "Age:f0:Gender")
term_levels <- term_levels[term_levels %in% tidy_model_filtered$term]
if (length(term_levels) == 0) {
  term_levels <- unique(tidy_model_filtered$term)
}

y_limits <- range(
  c(tidy_model_filtered$conf.low - 0.5, tidy_model_filtered$conf.high + 0.5),
  na.rm = TRUE
)
y_limits <- c(min(y_limits, -abs(y_limits)), max(y_limits, abs(y_limits)))

forest_plot <- tidy_model_filtered %>%
  filter(category %in% c("Tense", "Aspirate")) %>%
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
    text = element_text(size = 14),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    plot.title = element_text(size = 16),
    plot.subtitle = element_text(size = 12),
    strip.text = element_text(size = 14)
  )

print(forest_plot)

ggsave(
  "../graphs/all_ages_bayesian_analysis.png",
  plot = forest_plot,
  scale = 1,
  width = 7,
  height = 6,
  dpi = "retina"
)

browseURL("../graphs/all_ages_bayesian_analysis.png")

####################################
# Posterior draws box plot
####################################
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

posterior_draws_long <- bind_rows(aspirated_draws, fortis_draws, difference_draws) %>%
  filter(!is.na(term)) %>%
  mutate(
    comparison = factor(comparison, levels = c("Aspirated/Lenis", "Fortis/Lenis", "Aspirated/Fortis"))
  )

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
  "Male" = "Gender",
  "Age:VOT" = "Age:VOT",
  "Age:f0" = "Age:f0",
  "Male:VOT" = "Gender:VOT",
  "Male:f0" = "Gender:f0",
  "Age:VOT:Male" = "Age:VOT:Gender",
  "Age:f0:Male" = "Age:f0:Gender"
)

posterior_box_summary <- posterior_draws_long %>%
  group_by(term, comparison) %>%
  summarise(
    draw_min = min(draw, na.rm = TRUE),
    ci_lower = quantile(draw, probs = 0.025, na.rm = TRUE),
    median = median(draw, na.rm = TRUE),
    ci_upper = quantile(draw, probs = 0.975, na.rm = TRUE),
    draw_max = max(draw, na.rm = TRUE),
    .groups = "drop"
  )

box_plot <- posterior_box_summary %>%
  mutate(
    term = forcats::fct_recode(factor(term, levels = term_levels), !!!term_display),
    comparison_label = comparison_labels[as.character(comparison)]
  ) %>%
  ggplot(
    aes(
      x = term,
      ymin = draw_min,
      lower = ci_lower,
      middle = median,
      upper = ci_upper,
      ymax = draw_max
    )
  ) +
  geom_hline(yintercept = 0, color = "grey40", linewidth = 0.6, linetype = "dashed") +
  geom_boxplot(
    stat = "identity",
    fill = "grey85",
    color = "black",
    width = 0.55,
    outlier.shape = NA
  ) +
  facet_wrap(~comparison_label, nrow = 1, scales = "free_y", labeller = label_value) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.08))) +
  theme_classic() +
  labs(
    x = "Fixed effect",
    y = "Posterior coefficient",
    title = "Posterior Contrasts by Stop Category",
    subtitle = "Grey boxes show 95% credible intervals from the fitted brms model.",
    caption = "Dashed line marks zero effect."
  ) +
  theme(
    legend.position = "none",
    text = element_text(size = 14),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(size = 14, face = "bold"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    panel.grid = element_blank()
  )

print(box_plot)

ggsave(
  "../graphs/all_ages_bayesian_analysis_box_gender.png",
  plot = box_plot,
  scale = 1,
  width = 9,
  height = 7,
  dpi = "retina"
)

browseURL("../graphs/all_ages_bayesian_analysis_box_gender.png")

####################################
# Posterior draws box plot (filtered by p_direction >= 90%)
####################################

# Calculate p_direction for all parameters
pd_results <- p_direction(model)
pd_df <- as.data.frame(pd_results)

# Filter terms with p_direction >= 90%
significant_terms <- pd_df %>%
  filter(pd >= 0.90) %>%
  pull(Parameter)

# Filter tidy_lookup to only include significant terms
tidy_lookup_filtered <- tidy_lookup %>%
  filter(raw_term %in% significant_terms)

# Extract draws for significant terms only
extract_draws_filtered <- function(lookup_df, label) {
  if (nrow(lookup_df) == 0) {
    return(data.frame(term = character(), draw = numeric(), comparison = character()))
  }
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

aspirated_lookup_filtered <- tidy_lookup_filtered %>%
  filter(category == "Aspirate") %>%
  select(raw_term, term)

fortis_lookup_filtered <- tidy_lookup_filtered %>%
  filter(category == "Tense") %>%
  select(raw_term, term)

aspirated_draws_filtered <- extract_draws_filtered(aspirated_lookup_filtered, "Aspirated/Lenis")
fortis_draws_filtered <- extract_draws_filtered(fortis_lookup_filtered, "Fortis/Lenis")

# For difference, only include if both terms are significant
difference_lookup_filtered <- tidy_lookup_filtered %>%
  select(term, category, raw_term) %>%
  pivot_wider(names_from = category, values_from = raw_term) %>%
  drop_na()

if (nrow(difference_lookup_filtered) > 0) {
  difference_draws_filtered <- difference_lookup_filtered %>%
    mutate(draw = purrr::map2(Aspirate, Tense, ~ posterior_draws[[.x]] - posterior_draws[[.y]])) %>%
    select(term, draw) %>%
    unnest(cols = c(draw)) %>%
    mutate(
      term = factor(term, levels = term_levels),
      comparison = "Aspirated/Fortis"
    ) %>%
    select(term, draw, comparison)
} else {
  difference_draws_filtered <- data.frame(term = character(), draw = numeric(), comparison = character())
}

posterior_draws_filtered_long <- bind_rows(aspirated_draws_filtered, fortis_draws_filtered, difference_draws_filtered) %>%
  filter(!is.na(term)) %>%
  mutate(
    comparison = factor(comparison, levels = c("Aspirated/Lenis", "Fortis/Lenis", "Aspirated/Fortis"))
  )

if (nrow(posterior_draws_filtered_long) > 0) {
  posterior_box_summary_filtered <- posterior_draws_filtered_long %>%
    group_by(term, comparison) %>%
    summarise(
      draw_min = min(draw, na.rm = TRUE),
      ci_lower = quantile(draw, probs = 0.025, na.rm = TRUE),
      median = median(draw, na.rm = TRUE),
      ci_upper = quantile(draw, probs = 0.975, na.rm = TRUE),
      draw_max = max(draw, na.rm = TRUE),
      .groups = "drop"
    )
  
  box_plot_filtered <- posterior_box_summary_filtered %>%
    mutate(
      term = forcats::fct_recode(factor(term, levels = term_levels), !!!term_display),
      comparison_label = comparison_labels[as.character(comparison)]
    ) %>%
    ggplot(
      aes(
        x = term,
        ymin = draw_min,
        lower = ci_lower,
        middle = median,
        upper = ci_upper,
        ymax = draw_max
      )
    ) +
    geom_hline(yintercept = 0, color = "grey40", linewidth = 0.6, linetype = "dashed") +
    geom_boxplot(
      stat = "identity",
      fill = "grey85",
      color = "black",
      width = 0.55,
      outlier.shape = NA
    ) +
    facet_wrap(~comparison_label, nrow = 1, scales = "free", labeller = label_value) +
    scale_y_continuous(expand = expansion(mult = c(0.05, 0.08))) +
    theme_classic() +
    labs(
      x = "Fixed effect",
      y = "Posterior coefficient",
      title = "Posterior Contrasts by Stop Category (p_direction >= 90%)",
      subtitle = "Grey boxes show 95% credible intervals from the fitted brms model. Only coefficients with p_direction >= 90% are shown.",
      caption = "Dashed line marks zero effect."
    ) +
    theme(
      legend.position = "none",
      text = element_text(size = 14),
      axis.title = element_text(size = 14),
      axis.text = element_text(size = 12),
      axis.text.x = element_text(angle = 45, hjust = 1),
      strip.text = element_text(size = 14, face = "bold"),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
      axis.line = element_line(color = "black"),
      axis.ticks = element_line(color = "black"),
      panel.grid = element_blank()
    )
  
  print(box_plot_filtered)
  
  ggsave(
    "../graphs/all_ages_bayesian_analysis_box_gender_filtered.png",
    plot = box_plot_filtered,
    scale = 1,
    width = 9,
    height = 7,
    dpi = "retina"
  )
  
  browseURL("../graphs/all_ages_bayesian_analysis_box_gender_filtered.png")
} else {
  message("No significant terms found with p_direction >= 90%")
}

####################################
# Plot the random effects
####################################
model.random_effect.result <- broom.mixed::tidy(model, effects = "ran_pars", conf.int = TRUE)

model.random_effect.filtered_result <- model.random_effect.result %>%
  filter(!str_detect(term, "cor")) %>%
  mutate(
    category = case_when(
      str_detect(term, "mutense") ~ "Tense",
      str_detect(term, "muasp") ~ "Aspirated",
      TRUE ~ "Other"
    ),
    term = str_replace_all(term, "_", "") %>%
      str_remove_all("sd") %>%
      str_remove_all("mutense") %>%
      str_remove_all("muasp") %>%
      str_replace_all("svot", "VOT") %>%
      str_replace_all("sf0", "f0") %>%
      str_remove_all("[()]") %>%
      str_to_title() %>%
      str_replace_all("Vot", "VOT")
  )

model.random_effect.color_mape <- c(
  "VOT" = "#1f77b4",
  "Intercept" = "#00000e",
  "F0" = "#2ca02c"
)

y_limits <- range(
  c(model.random_effect.filtered_result$conf.low, model.random_effect.filtered_result$conf.high),
  na.rm = TRUE
)
model.random_effect.y_limits <- c(0, ceiling(max(abs(y_limits))))

model.random_effect.plot <- model.random_effect.filtered_result %>%
  filter(category %in% c("Tense", "Aspirated")) %>%
  ggplot(aes(x = term, y = estimate, color = term)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  scale_color_manual(values = model.random_effect.color_mape) +
  scale_x_discrete(limits = c("F0", "VOT", "Intercept")) +
  scale_y_continuous(
    limits = model.random_effect.y_limits,
    breaks = seq(0, model.random_effect.y_limits[2], by = 1),
    oob = scales::squish
  ) +
  theme_minimal() +
  labs(
    x = "Random effect",
    y = "SD",
    title = "Bayesian Analysis Result - Random Effects",
    subtitle = "Note: 'Vot' and 'F0' are scaled variables.",
    caption = "Error bars represent 95% confidence intervals"
  ) +
  facet_wrap(~category) +
  theme(legend.position = "none")

print(model.random_effect.plot)

ggsave(
  "../graphs/all_ages_bayesian_analysis_random_effect.png",
  plot = model.random_effect.plot,
  scale = 1,
  width = 5,
  height = 3,
  dpi = "retina"
)

####################################
# Rainbow Plot 
####################################
data_for_rainbow_plot <- plot_data_preprocessing(processed_data)
rainbow_plot <- f0_vot_rainbow_plot(
  data = data_for_rainbow_plot,
  title = "Mean responses across F0 and VOT continua",
  path = "../graphs/all_ages_three.png"
)

browseURL("../graphs/all_ages_three.png")

####################################
# Observed vs. Predicted Probabilities
####################################
observed_probabilities <- processed_data %>%
  count(f0, vot, response) %>%
  group_by(f0, vot) %>%
  complete(response = levels(processed_data$response), fill = list(n = 0)) %>%
  mutate(observed_probability = n / sum(n)) %>%
  ungroup() %>%
  select(-n)

f0_levels <- sort(unique(processed_data$f0))
vot_levels <- sort(unique(processed_data$vot))
gender_levels <- levels(processed_data$gender)
poa_levels <- levels(processed_data$poa)

poa_weights <- processed_data %>%
  count(poa) %>%
  mutate(poa_weight = n / sum(n)) %>%
  select(poa, poa_weight)

age_grid <- tibble(
  age = c(20:39, 40:59, 60:70),
  age_weight = c(
    rep(1, length(20:39)),
    rep(1, length(40:59)),
    rep(2, length(60:70))
  )
)

scale_with_training <- function(raw, reference_scaled_column) {
  center <- attr(reference_scaled_column, "scaled:center")
  scale <- attr(reference_scaled_column, "scaled:scale")
  (raw - center) / scale
}

prediction_grid <- crossing(
  f0 = f0_levels,
  vot = vot_levels,
  gender = gender_levels,
  poa = poa_levels,
  age = age_grid$age
) %>%
  left_join(age_grid, by = "age") %>%
  left_join(poa_weights, by = "poa") %>%
  mutate(
    poa_weight = coalesce(poa_weight, 1 / length(poa_levels)),
    weight = age_weight * poa_weight,
    gender = factor(gender, levels = gender_levels),
    poa = factor(poa, levels = poa_levels),
    sf0 = scale_with_training(f0, processed_data$sf0),
    svot = scale_with_training(vot, processed_data$svot),
    sage = scale_with_training(age, processed_data$sage)
  ) %>%
  select(-age_weight, -poa_weight)

posterior_predictions <- posterior_epred(
  model,
  newdata = prediction_grid,
  re_formula = NA
)

posterior_mean_probabilities <- apply(posterior_predictions, c(2, 3), mean)

category_names <- dimnames(posterior_predictions)[[3]]
if (is.null(category_names)) {
  category_names <- levels(processed_data$response)
}
colnames(posterior_mean_probabilities) <- category_names

predicted_probabilities <- prediction_grid %>%
  bind_cols(as_tibble(posterior_mean_probabilities))

predicted_probabilities_long <- predicted_probabilities %>%
  pivot_longer(
    cols = all_of(category_names),
    names_to = "response",
    values_to = "predicted_probability"
  ) %>%
  group_by(f0, vot, response) %>%
  summarise(
    predicted_probability = sum(predicted_probability * weight) / sum(weight),
    .groups = "drop"
  )

probability_comparison <- observed_probabilities %>%
  left_join(predicted_probabilities_long, by = c("f0", "vot", "response")) %>%
  mutate(
    response_label = recode(response,
      "asp" = "Aspirated",
      "tense" = "Fortis",
      "lenis" = "Lenis",
      .default = response
    ),
    response_label = factor(response_label, levels = c("Aspirated", "Fortis", "Lenis")),
    f0 = factor(f0, levels = f0_levels)
  )

predicted_plot_data <- probability_comparison %>%
  select(f0, vot, response_label, predicted_probability) %>%
  drop_na(predicted_probability)

observed_plot_data <- probability_comparison %>%
  select(f0, vot, response_label, observed_probability) %>%
  drop_na(observed_probability)

observed_vs_predicted_plot <- ggplot() +
  geom_smooth(
    data = predicted_plot_data,
    aes(
      x = vot,
      y = predicted_probability,
      color = f0,
      linetype = "Predicted"
    ),
    method = "loess",
    formula = y ~ x,
    se = FALSE,
    linewidth = 1
  ) +
  geom_point(
    data = observed_plot_data,
    aes(
      x = vot,
      y = observed_probability,
      color = f0,
      shape = "Observed"
    ),
    size = 2.4
  ) +
  facet_wrap(~response_label, nrow = 1) +
  scale_color_brewer(palette = "Set2", name = "F0 level") +
  scale_linetype_manual(name = "", values = c("Predicted" = "solid")) +
  scale_shape_manual(name = "", values = c("Observed" = 16)) +
  scale_y_continuous(limits = c(0, 1), expand = expansion(mult = c(0, 0.05))) +
  labs(
    x = "VOT (ms)",
    y = "Probability",
    title = "Observed vs. Predicted Class Probabilities",
    subtitle = "Lines show model predictions; points mark empirical response proportions.",
    caption = "Predicted probabilities margined over age, gender, and POA weights."
  ) +
  guides(
    color = guide_legend(order = 1),
    linetype = guide_legend(order = 2),
    shape = guide_legend(order = 3)
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid = element_blank(),
    strip.text = element_text(face = "bold"),
    legend.position = "bottom",
    legend.box = "vertical",
    axis.text = element_text(size = 13)
  )

print(observed_vs_predicted_plot)

ggsave(
  "../graphs/all_ages_observed_vs_predicted_probabilities.png",
  plot = observed_vs_predicted_plot,
  scale = 1,
  width = 9,
  height = 4,
  dpi = "retina"
)

browseURL("../graphs/all_ages_observed_vs_predicted_probabilities.png")

####################################
# Observed vs. Predicted Probabilities (F0 Binned)
####################################

probability_comparison_binned <- probability_comparison %>%
  mutate(
    f0_bin = cut(
      as.numeric(as.character(f0)),
      breaks = c(-Inf, 2, 4, 6, Inf),
      labels = c("F0 1-2", "F0 3-4", "F0 5-6", "F0 7-8"),
      right = TRUE
    )
  )

predicted_plot_data_binned <- probability_comparison_binned %>%
  group_by(f0_bin, vot, response_label) %>%
  summarise(
    predicted_probability = mean(predicted_probability, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  drop_na(predicted_probability)

observed_plot_data_binned <- probability_comparison_binned %>%
  group_by(f0_bin, vot, response_label) %>%
  summarise(
    observed_probability = mean(observed_probability, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  drop_na(observed_probability)

observed_vs_predicted_plot_binned <- ggplot() +
  geom_hline(
    yintercept = c(0.25, 0.75),
    color = "grey80",
    linewidth = 0.4,
    linetype = "dashed"
  ) +
  geom_hline(
    yintercept = 0.5,
    color = "grey60",
    linewidth = 0.5,
    linetype = "dotted"
  ) +
  geom_smooth(
    data = predicted_plot_data_binned,
    aes(
      x = vot,
      y = predicted_probability,
      color = f0_bin,
      linetype = "Predicted"
    ),
    method = "loess",
    formula = y ~ x,
    se = FALSE,
    linewidth = 1
  ) +
  geom_point(
    data = observed_plot_data_binned,
    aes(
      x = vot,
      y = observed_probability,
      color = f0_bin,
      shape = "Observed"
    ),
    size = 2.4
  ) +
  facet_wrap(~response_label, nrow = 1) +
  scale_color_manual(
    name = "F0 bin",
    values = c("F0 1-2" = "#0072B2", "F0 3-4" = "#E69F00", "F0 5-6" = "#009E73", "F0 7-8" = "#56B4E9")
  ) +
  scale_linetype_manual(name = "", values = c("Predicted" = "solid")) +
  scale_shape_manual(name = "", values = c("Observed" = 16)) +
  scale_y_continuous(limits = c(0, 1), expand = expansion(mult = c(0, 0.05))) +
  labs(
    x = "VOT (ms)",
    y = "Probability",
    title = "Observed vs. Predicted Class Probabilities (F0 Bins)",
    subtitle = "Lines show model predictions binned across F0 levels; points mark empirical response proportions.",
    caption = "Predicted probabilities margined over age, gender, and POA weights."
  ) +
  guides(
    color = guide_legend(order = 1),
    linetype = guide_legend(order = 2),
    shape = guide_legend(order = 3)
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid = element_blank(),
    strip.text = element_text(face = "bold"),
    legend.position = "bottom",
    legend.box = "vertical"
  )

print(observed_vs_predicted_plot_binned)

ggsave(
  "../graphs/all_ages_observed_vs_predicted_probabilities_binned.png",
  plot = observed_vs_predicted_plot_binned,
  scale = 1,
  width = 9,
  height = 4,
  dpi = "retina"
)

browseURL("../graphs/all_ages_observed_vs_predicted_probabilities_binned.png")


