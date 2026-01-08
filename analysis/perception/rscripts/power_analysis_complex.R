# Analysis of experimental results of
# Korean stop contrast, perception, young (pilot)
# created by Sarang Jeong on June 6, 2021

##########
# set-up #
##########

# set working directory
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

options(cmdstanr_warn_inits = FALSE)

library(brms)
library(tidyverse)
library(furrr)
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
    formula = response ~ poa + svot*sage*gender + sf0*sage*gender + (1 + svot + sf0 | subject),
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
  model_path = "../model/all_ages_multinomial_logistic_regression_poa_gender.rds",
  data = processed_data,
  model_function = fit_brms_model)

# 실험 디자인 파라미터 (실제 값 범위)
design_params <- list(
  f0_levels = 1:8,      # 실제 f0 값 (Hz 단위 등)
  vot_levels = 1:8,     # 실제 VOT 값 (ms 단위 등)
  poa_levels = c("cor", "dor", "lab"),
  n_repetitions = 1,
  age_min = 20,         # 최소 나이
  age_max = 70          # 최대 나이
)

# 한 participant의 완전한 데이터 생성 함수
create_subject_data <- function(subject_id, participant_age, participant_gender, design_params) {
  
  # 192 trials per participant (8 f0 x 8 vot x 3 poa x 1 repetition)
  data <- expand.grid(
    subject = subject_id,
    f0 = design_params$f0_levels,
    vot = design_params$vot_levels,
    poa = design_params$poa_levels,
    repetition = 1:design_params$n_repetitions
  ) %>%
    mutate(
      age = participant_age,  # 각 participant는 고정된 나이
      gender = participant_gender  # 각 participant는 고정된 성별
    )
  
  return(data)
}

# 여러 participant 데이터 생성 (나이 분포 포함)
create_full_dataset <- function(n_subjects, design_params, age_distribution = "uniform") {
  
  # 각 participant의 나이 생성
  if (age_distribution == "uniform") {
    participant_ages <- runif(
      n_subjects, 
      min = design_params$age_min, 
      max = design_params$age_max
    )
  } else if (age_distribution == "normal") {
    # 정규분포 (평균 45세, SD 15)
    participant_ages <- rnorm(n_subjects, mean = 45, sd = 15)
    participant_ages <- pmin(pmax(participant_ages, design_params$age_min), 
                             design_params$age_max)
  }
  
  # 각 participant의 성별 생성 (50/50 분포)
  participant_genders <- sample(c("F", "M"), n_subjects, replace = TRUE)
  
  # 각 participant의 데이터 생성
  full_data <- pmap_dfr(
    list(1:n_subjects, participant_ages, participant_genders),
    ~create_subject_data(..1, ..2, ..3, design_params)
  )
  
  # Scaling 적용
  full_data <- full_data %>%
    mutate(
      # 전체 데이터셋 기준으로 scaling
      sf0 = scale(f0)[,1],
      svot = scale(vot)[,1],
      sage = scale(age)[,1],
      # gender를 factor로 변환 (reference level: F)
      gender = factor(gender, levels = c("F", "M"))
    )
  
  return(full_data)
}

################################################################### SIMULATION ##################################################################################
#################################################################################################################################################################
# POST-HOC POWER ANALYSIS (Sensitivity Analysis)
# 고정된 샘플 크기에서 다양한 효과 크기를 탐지할 수 있는 검정력 계산
#################################################################################################################################################################
#################################################################################################################################################################

simulate_power_sensitivity <- function(
    fitted_model, 
    design_params,
    target_effects = list(
      asp_svot_sage = c(0.5, 1), 
      asp_sage_sf0 = c(-0.5, -1),      
      tense_svot_sage = c(-0.5, -1),
      tense_sage_sf0 = c(-0.5, -1)     
    ),
    n_sim = 100,
    age_distribution = "uniform",
    use_cmdstanr = TRUE,
    verbose = TRUE
) {
  
  # 실제 데이터의 샘플 크기 사용
  n_subjects <- length(unique(fitted_model$data$subject))
  
  if (verbose) {
    message(sprintf("\n=== Sensitivity Analysis with Fixed Effect Sizes ==="))
    message(sprintf("Sample size: %d subjects", n_subjects))
    total_tests <- sum(sapply(target_effects, length))
    message(sprintf("Total tests: %d (across %d effects)", total_tests, length(target_effects)))
    message("Target effects:")
    for (name in names(target_effects)) {
      message(sprintf("  %s: %s", name, paste(target_effects[[name]], collapse=", ")))
    }
  }
  
  # 실제 모델에서 추정된 계수 추출 (참고용)
  fixed_effects <- fixef(fitted_model)
  
  # 각 효과와 그 효과 크기에 대해 독립적으로 power 계산
  all_results <- map_dfr(names(target_effects), function(effect_name) {
    
    effect_sizes <- target_effects[[effect_name]]
    
    map_dfr(effect_sizes, function(effect_size) {
      
      if (verbose) {
        message(sprintf("\n=== Testing %s = %.2f ===", effect_name, effect_size))
      }
      
      # 순차 실행 (각 시뮬레이션)
      sim_results <- map_dfr(1:n_sim, function(i) {
        
        if (verbose && i %% 10 == 0) {
          message(sprintf("  [%s] Simulation %d/%d (%s = %.2f)...", Sys.time(), i, n_sim, effect_name, effect_size))
        }
        
        # Step 1: 새로운 데이터셋 생성 (고정된 샘플 크기)
        new_data <- create_full_dataset(
          n_subjects, 
          design_params, 
          age_distribution
        )
        
        # Step 2: 고정된 효과 크기로 response 시뮬레이션
        
        # posterior에서 하나의 draw를 가져옴
        posterior_samples <- as_draws_df(fitted_model)
        random_draw_idx <- sample(nrow(posterior_samples), 1)
        one_draw <- posterior_samples[random_draw_idx, ]
        
        # 관심 있는 효과 하나만 고정 효과 크기로 대체, 나머지는 유지
        modified_draw <- one_draw
        
        # 현재 테스트 중인 효과만 설정
        coef_name <- switch(effect_name,
                            asp_svot_sage = "b_muasp_svot:sage",
                            asp_sage_sf0 = "b_muasp_sage:sf0",
                            tense_svot_sage = "b_mutense_svot:sage",
                            tense_sage_sf0 = "b_mutense_sage:sf0")
        
        modified_draw[[coef_name]] <- effect_size
        
        # 조정된 계수로 response 시뮬레이션
        # brms는 직접적인 계수 조정이 어려우므로, linear predictor를 직접 계산
        n_categories <- length(levels(fitted_model$data$response))
        response_levels <- levels(fitted_model$data$response)
        
        # 각 카테고리에 대한 linear predictor 계산
        linear_predictors <- matrix(0, nrow = nrow(new_data), ncol = n_categories)
        
        # Reference category는 첫 번째 (lenis) - linear predictor = 0
        # 나머지 카테고리들에 대해 계산
        for (cat_idx in 2:n_categories) {
          cat_name <- response_levels[cat_idx]
          
          # Fixed effects - 스칼라 값을 벡터로 확장
          intercept_name <- paste0("b_mu", cat_name, "_Intercept")
          if (!intercept_name %in% names(modified_draw)) {
            stop(paste("Cannot find intercept:", intercept_name))
          }
          intercept_val <- as.numeric(modified_draw[[intercept_name]])
          lp <- rep(intercept_val, nrow(new_data))
          
          # svot 주효과
          coef_name <- paste0("b_mu", cat_name, "_svot")
          if (coef_name %in% names(modified_draw)) {
            lp <- lp + as.numeric(modified_draw[[coef_name]]) * new_data$svot
          }
          
          # sf0 주효과
          coef_name <- paste0("b_mu", cat_name, "_sf0")
          if (coef_name %in% names(modified_draw)) {
            lp <- lp + as.numeric(modified_draw[[coef_name]]) * new_data$sf0
          }
          
          # poa: dorsal
          coef_name <- paste0("b_mu", cat_name, "_poador")
          if (coef_name %in% names(modified_draw)) {
            lp <- lp + as.numeric(modified_draw[[coef_name]]) * as.numeric(new_data$poa == "dor")
          }
          
          # poa: labial
          coef_name <- paste0("b_mu", cat_name, "_poalab")
          if (coef_name %in% names(modified_draw)) {
            lp <- lp + as.numeric(modified_draw[[coef_name]]) * as.numeric(new_data$poa == "lab")
          }
          
          # sage 주효과
          coef_name <- paste0("b_mu", cat_name, "_sage")
          if (coef_name %in% names(modified_draw)) {
            lp <- lp + as.numeric(modified_draw[[coef_name]]) * new_data$sage
          }
          
          # svot:sage 교호작용
          coef_name <- paste0("b_mu", cat_name, "_svot:sage")
          if (coef_name %in% names(modified_draw)) {
            lp <- lp + as.numeric(modified_draw[[coef_name]]) * new_data$svot * new_data$sage
          }
          
          # sage:sf0 교호작용 (순서 주의)
          coef_name <- paste0("b_mu", cat_name, "_sage:sf0")
          if (coef_name %in% names(modified_draw)) {
            lp <- lp + as.numeric(modified_draw[[coef_name]]) * new_data$sage * new_data$sf0
          }
          
          # gender 주효과 (genderMale)
          coef_name <- paste0("b_mu", cat_name, "_genderMale")
          if (coef_name %in% names(modified_draw)) {
            lp <- lp + as.numeric(modified_draw[[coef_name]]) * as.numeric(new_data$gender == "M")
          }
          
          # svot:genderMale 교호작용
          coef_name <- paste0("b_mu", cat_name, "_svot:genderMale")
          if (coef_name %in% names(modified_draw)) {
            lp <- lp + as.numeric(modified_draw[[coef_name]]) * new_data$svot * as.numeric(new_data$gender == "M")
          }
          
          # sage:genderMale 교호작용
          coef_name <- paste0("b_mu", cat_name, "_sage:genderMale")
          if (coef_name %in% names(modified_draw)) {
            lp <- lp + as.numeric(modified_draw[[coef_name]]) * new_data$sage * as.numeric(new_data$gender == "M")
          }
          
          # genderMale:sf0 교호작용
          coef_name <- paste0("b_mu", cat_name, "_genderMale:sf0")
          if (coef_name %in% names(modified_draw)) {
            lp <- lp + as.numeric(modified_draw[[coef_name]]) * as.numeric(new_data$gender == "M") * new_data$sf0
          }
          
          # svot:sage:genderMale 3-way 교호작용
          coef_name <- paste0("b_mu", cat_name, "_svot:sage:genderMale")
          if (coef_name %in% names(modified_draw)) {
            lp <- lp + as.numeric(modified_draw[[coef_name]]) * new_data$svot * new_data$sage * as.numeric(new_data$gender == "M")
          }
          
          # sage:genderMale:sf0 3-way 교호작용
          coef_name <- paste0("b_mu", cat_name, "_sage:genderMale:sf0")
          if (coef_name %in% names(modified_draw)) {
            lp <- lp + as.numeric(modified_draw[[coef_name]]) * new_data$sage * as.numeric(new_data$gender == "M") * new_data$sf0
          }
          
          linear_predictors[, cat_idx] <- lp
        }
        
        # Softmax 변환으로 확률 계산
        exp_lp <- exp(linear_predictors)
        probs <- exp_lp / rowSums(exp_lp)
        
        # 확률에 따라 response 샘플링
        simulated_responses <- apply(probs, 1, function(p) sample(1:n_categories, 1, prob = p))
        new_data$response <- factor(simulated_responses, levels = 1:n_categories, labels = response_levels)
        
        # Step 3: 새 데이터로 모델 re-fitting
        sim_fit <- tryCatch({
          
          # 초기값을 fitted model의 추정값으로 설정
          init_values <- lapply(1:2, function(x) {
            list(
              b = as.vector(fixef(fitted_model)[, "Estimate"])
            )
          })
          
          update(
            fitted_model,
            newdata = new_data,
            refresh = 0,
            iter = 500,
            warmup = 250,
            chains = 2,
            cores = 4,                             # Use multiple cores for faster computation
            threads = threading(6),
            backend = if(use_cmdstanr) "cmdstanr" else "rstan",
            init = init_values,
            control = list(adapt_delta = 0.95)
          )
        }, error = function(e) {
          return(NULL)
        })
        
        if (is.null(sim_fit)) {
          return(tibble(
            iteration = i, 
            effect_name = effect_name,
            effect_size = effect_size,
            converged = FALSE
          ))
        }
        
        # 수렴 확인
        rhat_max <- max(rhat(sim_fit), na.rm = TRUE)
        if (rhat_max > 1.1) {
          return(tibble(
            iteration = i, 
            effect_name = effect_name,
            effect_size = effect_size,
            converged = FALSE, 
            rhat_max = rhat_max
          ))
        }
        
        # Step 4: 효과 추출 및 검정
        posterior <- as_draws_df(sim_fit)
        
        results <- tibble(
          iteration = i, 
          effect_name = effect_name,
          effect_size = effect_size,
          converged = TRUE,
          rhat_max = rhat_max
        )
        
        # 현재 테스트 중인 효과만 검정
        var_name <- switch(effect_name,
                           asp_svot_sage = "b_muasp_svot:sage",
                           asp_sage_sf0 = "b_muasp_sage:sf0",
                           tense_svot_sage = "b_mutense_svot:sage",
                           tense_sage_sf0 = "b_mutense_sage:sf0")
        
        if (var_name %in% names(posterior)) {
          ci <- quantile(posterior[[var_name]], c(0.025, 0.975))
          results[["significant"]] <- !between(0, ci[1], ci[2])
          results[["estimate"]] <- median(posterior[[var_name]])
        } else {
          results[["significant"]] <- NA
          results[["estimate"]] <- NA
        }
        
        return(results)
        
      })
      
      # 이 효과 크기에 대한 power 계산
      power_summary <- sim_results %>%
        filter(converged) %>%
        summarise(
          effect_name = effect_name,
          effect_size = effect_size,
          n_converged = n(),
          convergence_rate = n() / n_sim,
          power = mean(significant, na.rm = TRUE),
          mean_estimate = mean(estimate, na.rm = TRUE)
        )
      
      if (verbose) {
        message(sprintf("  Completed %s = %.2f: Power = %.3f", 
                        effect_name, effect_size, power_summary$power))
      }
      
      return(power_summary)
      
    })
  })
  
  return(list(
    results = all_results,
    actual_sample_size = n_subjects
  ))
}

##############################################
# 실행: Sensitivity Analysis
##############################################

# 먼저 실제 모델의 효과 크기 확인
cat("\n=== Observed Effect Sizes ===\n")
observed_effects <- fixef(pilot_model)
effects_of_interest <- c("muasp_svot:sage", "muasp_sage:sf0", "mutense_svot:sage", "mutense_sage:sf0")
for (effect in effects_of_interest) {
  if (effect %in% rownames(observed_effects)) {
    cat(sprintf("%s: %.3f [%.3f, %.3f]\n", 
                effect,
                observed_effects[effect, "Estimate"],
                observed_effects[effect, "Q2.5"],
                observed_effects[effect, "Q97.5"]))
  }
}

# 고정된 효과 크기 설정 (부호 유지)
# Small = 0.2, Medium = 0.5, Large = 0.8 (Cohen's d 기준을 logit scale에 적용)
target_effects <- list(
  asp_svot_sage = c(-0.5, -0.45, -0.4, -0.35, -0.3, 0.3, 0.35, 0.4, 0.45, 0.5),      # 양/음 모두 테스트
  asp_sage_sf0 = c(-0.5, -0.45, -0.4, -0.35, -0.3, 0.2, 0.25, 0.3, 0.35, 0.4),
  tense_svot_sage = c(-0.5, -0.45, -0.4, -0.35, -0.3, 0.3, 0.35, 0.4, 0.45, 0.5),
  tense_sage_sf0 = c(-0.3, -0.25, -0.2, -0.15, -0.1, 0.1, 0.15, 0.2, 0.25, 0.3)
)

cat("\n=== Running Sensitivity Analysis ===\n")
cat("Sample size:", length(unique(pilot_model$data$subject)), "subjects\n\n")

# Sensitivity analysis 실행
n_sim <- 100  # 시뮬레이션 횟수
age_distribution <- "uniform"  # 나이 분포

sensitivity_results <- simulate_power_sensitivity(
  fitted_model = pilot_model,
  design_params = design_params,
  target_effects = target_effects,
  n_sim = n_sim,
  age_distribution = age_distribution,
  verbose = TRUE
)

# 결과 저장
saveRDS(sensitivity_results, "../model/power_sensitivity_analysis_results_sim100.rds")

##############################################
# Sensitivity Analysis 결과 시각화
##############################################
sensitivity_results <- readRDS("../model/power_sensitivity_analysis_results.rds")

power_data <- sensitivity_results$results

# 이미 long format이므로 effect_label만 추가
power_long <- power_data %>%
  mutate(
    effect_label = case_when(
      effect_name == "asp_svot_sage" ~ "aspirated: VOT × age",
      effect_name == "asp_sage_sf0" ~ "aspirated: age × f0",
      effect_name == "tense_svot_sage" ~ "tense: VOT × age",
      effect_name == "tense_sage_sf0" ~ "tense: age × f0",
      TRUE ~ effect_name
    )
  )

# 각 효과별로 개별 그래프 생성
for (i in 1:nrow(power_long %>% distinct(effect_name, effect_label))) {
  effect_row <- power_long %>% 
    distinct(effect_name, effect_label) %>% 
    slice(i)
  
  effect_name_val <- effect_row$effect_name
  effect_label_val <- effect_row$effect_label
  
  # 해당 effect 데이터만 필터링
  plot_data <- power_long %>% 
    filter(effect_name == effect_name_val)
  
  # 그래프 생성
  p <- ggplot(plot_data, aes(x = effect_size, y = power)) +
    geom_line(linewidth = 1.2, color = "#377EB8") +
    geom_point(size = 2.5, alpha = 0.7, color = "#377EB8") +
    geom_hline(yintercept = 0.8, linetype = "dashed", color = "red", alpha = 0.5) +
    geom_vline(xintercept = 0, linetype = "dotted", color = "gray60") +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1)) +
    annotate("text", x = min(plot_data$effect_size), y = 0.8, 
             label = "80% power", hjust = 0, vjust = -0.5, 
             size = 3.5, color = "red") +
    labs(
      title = paste("Power Analysis:", effect_label_val),
      subtitle = sprintf("Sample size: %d subjects | Simulations: %d | Age distribution: %s",
                         sensitivity_results$actual_sample_size, n_sim, age_distribution),
      x = "Fixed Effect Size (log-odds scale)",
      y = "Statistical Power"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 10),
      axis.title = element_text(size = 11, face = "bold")
    )
  
  print(p)
  
  # 파일명 생성 (effect_name을 파일명으로 사용)
  filename <- paste0("../graphs/sensitivity_", effect_name_val, ".png")
  ggsave(filename, p, width = 10, height = 6, dpi = 300)
  
  cat(sprintf("\nSaved: %s\n", filename))
}

browseURL("../graphs/sensitivity_tense_sage_sf0.png")
browseURL("../graphs/sensitivity_tense_svot_sage.png")
browseURL("../graphs/sensitivity_asp_sage_sf0.png")
browseURL("../graphs/sensitivity_asp_svot_sage.png")

# 최소 탐지 가능 효과 크기 (Minimum Detectable Effect Size at 80% power)
mdes_80 <- power_long %>%
  filter(power >= 0.8) %>%
  group_by(effect_name, effect_label) %>%
  summarise(
    mdes_positive = ifelse(any(effect_size > 0), min(effect_size[effect_size > 0]), NA),
    mdes_negative = ifelse(any(effect_size < 0), max(effect_size[effect_size < 0]), NA),
    .groups = "drop"
  )

cat("\n=== Minimum Detectable Effect Size (80% power) ===\n")
cat("Positive effect sizes: smallest value achieving 80% power\n")
cat("Negative effect sizes: largest (least negative) value achieving 80% power\n\n")
print(mdes_80)

# 요약 테이블
cat("\n=== Summary: Power by Effect and Effect Size ===\n\n")
summary_table <- power_data %>%
  select(effect_name, effect_size, convergence_rate, power) %>%
  mutate(power = round(power, 3)) %>%
  arrange(effect_name, effect_size)

print(summary_table)

# 각 효과별 power curve 요약
cat("\n=== Power Curve Summary by Effect ===\n\n")
effect_summary <- power_long %>%
  group_by(effect_name, effect_label) %>%
  summarise(
    n_tests = n(),
    min_power = min(power, na.rm = TRUE),
    max_power = max(power, na.rm = TRUE),
    mean_power = mean(power, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(across(starts_with("min_power"):starts_with("mean_power"), ~round(., 3)))

print(effect_summary)