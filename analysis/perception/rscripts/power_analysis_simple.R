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
pilot_model <- save_or_load_model(
  model_path = "../model/all_ages_multinomial_logistic_regression.rds",
  data = processed_data,
  model_function = fit_brms_model)

####################################################################################################################

# Step 1: 모든 파라미터에 대해 power 계산
calculate_power_all_params <- function(
    pilot_fit,
    n_subjects_new,
    n_trials_new = 50,
    alpha = 0.05
) {
  
  # Posterior 추출
  post <- as_draws_df(pilot_fit)
  
  # 관심 파라미터 (Intercept 제외한 fixed effects)
  param_names <- colnames(post)
  parameters <- grep("^b_mu(asp|tense)_", param_names, value = TRUE)
  parameters <- parameters[!grepl("Intercept", parameters)]
  
  # Pilot 정보
  pilot_n_subjects <- length(unique(pilot_fit$data$subject))
  pilot_n_trials <- nrow(pilot_fit$data) / pilot_n_subjects
  
  # SE scaling
  se_scaling <- sqrt(
    (pilot_n_subjects * pilot_n_trials) / 
      (n_subjects_new * n_trials_new)
  )
  
  # 각 파라미터의 power 계산
  results <- map_dfr(parameters, function(param) {
    
    param_samples <- post[[param]]
    effect_mean <- mean(param_samples)
    effect_se <- sd(param_samples)
    new_se <- effect_se * se_scaling
    
    # Power
    z_score <- abs(effect_mean) / new_se
    power <- 1 - pnorm(qnorm(1 - alpha/2) - z_score) + 
      pnorm(-qnorm(1 - alpha/2) - z_score)
    
    tibble(
      parameter = param,
      n_subjects = n_subjects_new,
      power = power
    )
  })
  
  return(results)
}


# Step 2: Power curve 데이터 생성
sample_sizes <- seq(10, 100, by = 10)

power_curve_data <- map_dfr(sample_sizes, function(n) {
  calculate_power_all_params(
    pilot_fit = pilot_model,  # <- 여기에 실제 모델 넣기
    n_subjects_new = n,
    n_trials_new = 50
  )
})


# Step 3: 파라미터명 정리
power_curve_data <- power_curve_data %>%
  mutate(
    parameter_clean = parameter %>%
      str_remove("b_mu") %>%
      str_replace(":", " × "),
    category = if_else(str_detect(parameter, "asp"), "asp", "tense")
  )


# Step 4: 시각화
ggplot(power_curve_data, aes(x = n_subjects, y = power, color = parameter_clean)) +
  geom_line(size = 1.2) +
  geom_point(size = 2.5) +
  geom_hline(yintercept = 0.8, linetype = "dashed", color = "red", size = 1) +
  facet_wrap(~category, ncol = 1) +
  labs(
    title = "Power Analysis: All Parameters",
    x = "Number of Subjects",
    y = "Statistical Power",
    color = "Parameter"
  ) +
  scale_x_continuous(
    limits = c(10, 100),
    breaks = seq(10, 100, by = 10)
  ) +
  scale_y_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, by = 0.1)
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  ) +
  guides(color = guide_legend(ncol = 2))


# Step 5: 필요한 N 출력 (80% power 기준)
required_n <- power_curve_data %>%
  filter(power >= 0.8) %>%
  group_by(parameter_clean) %>%
  slice_min(n_subjects, n = 1) %>%
  arrange(desc(n_subjects)) %>%
  select(Parameter = parameter_clean, `Required N` = n_subjects, Power = power)

print(required_n)