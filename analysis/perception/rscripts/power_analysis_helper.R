# Analysis of experimental results of
# Korean stop contrast, perception, young (pilot)
# created by Sarang Jeong on June 6, 2021

##########
# set-up #
##########

# set working directory
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

library(brms)
library(tidyverse)
library(furrr)
source("plot.R")
source("models.R")

# 한 participant의 완전한 데이터 생성 함수
create_subject_data <- function(subject_id, participant_age, design_params) {
  
  # 576 trials per participant
  data <- expand.grid(
    subject = subject_id,
    f0 = design_params$f0_levels,
    vot = design_params$vot_levels,
    poa = design_params$poa_levels,
    repetition = 1:design_params$n_repetitions
  ) %>%
    mutate(
      age = participant_age  # 각 participant는 고정된 나이
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
  
  # 각 participant의 데이터 생성
  full_data <- map2_dfr(
    1:n_subjects, 
    participant_ages,
    ~create_subject_data(.x, .y, design_params)
  )
  
  # Scaling 적용
  full_data <- full_data %>%
    mutate(
      # 전체 데이터셋 기준으로 scaling
      sf0 = scale(f0)[,1],
      svot = scale(vot)[,1],
      sage = scale(age)[,1]
    )
  
  return(full_data)
}


################################################################### SIMULATION ##################################################################################
#################################################################################################################################################################
#################################################################################################################################################################
#################################################################################################################################################################

simulate_power_scaled <- function(
    fitted_model, 
    n_subjects, 
    design_params,
    n_sim = 100,
    age_distribution = "uniform",
    use_cmdstanr = TRUE,  # cmdstanr 사용 여부
    verbose = TRUE        # 로그 출력 여부
) {
  
  # 속도 향상 핵심: 사전 컴파일된 Stan 코드 추출
  # 첫 fitting에서 컴파일된 모델을 재사용
  compiled_model <- fitted_model$model
  
  power_results <- future_map_dfr(1:n_sim, function(i) {
    
    if (verbose) {
      message(sprintf("\n[%s] Simulation %d/%d started...", Sys.time(), i, n_sim))
    }
    
    # Step 1: 새로운 완전한 데이터셋 생성 (scaled)
    new_data <- create_full_dataset(
      n_subjects, 
      design_params, 
      age_distribution
    )
    
    # Step 2: Fitted 모델로부터 response 시뮬레이션
    # 주의: 모델이 scaled 변수로 fitting 되었다면, 
    # 새 데이터도 같은 방식으로 scaling 되어야 함
    simulated_responses <- posterior_predict(
      fitted_model,
      newdata = new_data,
      ndraws = 1,
      allow_new_levels = TRUE
    )[1, ]
    
    # 시뮬레이션된 숫자를 원래 factor 레벨로 변환
    response_levels <- levels(fitted_model$data$response)
    new_data$response <- factor(simulated_responses, levels = 1:length(response_levels), labels = response_levels)
    
    # Step 3: 새 데이터로 모델 re-fitting
    if (verbose) {
      message(sprintf("  [%s] Fitting model for simulation %d...", Sys.time(), i))
    }
    
    sim_fit <- tryCatch({
      
      # 초기값을 fitted model의 추정값으로 설정 (수렴 속도 향상)
      init_values <- lapply(1:2, function(x) {
        list(
          b = as.vector(fixef(fitted_model)[, "Estimate"])
        )
      })
      
      update(
        fitted_model,
        newdata = new_data,
        refresh = 0,
        iter = 800,          # 총 iteration 더 감소 (warmup 300 + sampling 500)
        warmup = 300,        # warmup 감소
        chains = 2,          # 2개 chain
        cores = 2,
        threads = threading(4),    # threads 증가
        backend = if(use_cmdstanr) "cmdstanr" else "rstan",
        init = init_values,        # 초기값 제공으로 수렴 가속화
        control = list(
          adapt_delta = 0.85,      # 더 공격적으로 완화
          max_treedepth = 10,
          stepsize = 0.1           # 더 큰 step size
        ),
      )
    }, error = function(e) {
      if (verbose) {
        message(sprintf("  [%s] Simulation %d FAILED: %s", Sys.time(), i, e$message))
      }
      return(NULL)
    })
    
    if (is.null(sim_fit)) {
      if (verbose) {
        message(sprintf("  [%s] Simulation %d - Model fitting failed", Sys.time(), i))
      }
      return(tibble(iteration = i, converged = FALSE))
    }
    
    if (verbose) {
      message(sprintf("  [%s] Model fitting completed for simulation %d", Sys.time(), i))
    }
    
    # 수렴 확인
    rhat_max <- max(rhat(sim_fit), na.rm = TRUE)
    if (rhat_max > 1.1) {
      if (verbose) {
        message(sprintf("  [%s] Simulation %d - Convergence failed (Rhat = %.3f)", Sys.time(), i, rhat_max))
      }
      return(tibble(iteration = i, converged = FALSE, rhat_max = rhat_max))
    }
    
    if (verbose) {
      message(sprintf("  [%s] Simulation %d converged successfully (Rhat = %.3f)", Sys.time(), i, rhat_max))
    }
    
    # Step 4: 효과 추출 및 검정
    posterior <- as_draws_df(sim_fit)
    
    results <- tibble(
      iteration = i, 
      converged = TRUE,
      rhat_max = rhat_max
    )
    
    # Categorical response의 각 카테고리별 효과 검정
    # response 카테고리 수 확인
    n_categories <- length(unique(new_data$response))
    
    for (cat in 2:n_categories) {  # reference category 제외
      
      # svot:sage 교호작용
      var_svot_sage <- paste0("b_mu", cat, "_svot:sage")
      if (var_svot_sage %in% names(posterior)) {
        ci <- quantile(posterior[[var_svot_sage]], c(0.025, 0.975))
        results[[paste0("sig_svot_sage_cat", cat)]] <- !between(0, ci[1], ci[2])
        results[[paste0("est_svot_sage_cat", cat)]] <- median(posterior[[var_svot_sage]])
      }
      
      # sf0:sage 교호작용
      var_sf0_sage <- paste0("b_mu", cat, "_sf0:sage")
      if (var_sf0_sage %in% names(posterior)) {
        ci <- quantile(posterior[[var_sf0_sage]], c(0.025, 0.975))
        results[[paste0("sig_sf0_sage_cat", cat)]] <- !between(0, ci[1], ci[2])
        results[[paste0("est_sf0_sage_cat", cat)]] <- median(posterior[[var_sf0_sage]])
      }
      
      # svot 주효과
      var_svot <- paste0("b_mu", cat, "_svot")
      if (var_svot %in% names(posterior)) {
        ci <- quantile(posterior[[var_svot]], c(0.025, 0.975))
        results[[paste0("sig_svot_cat", cat)]] <- !between(0, ci[1], ci[2])
      }
      
      # sf0 주효과
      var_sf0 <- paste0("b_mu", cat, "_sf0")
      if (var_sf0 %in% names(posterior)) {
        ci <- quantile(posterior[[var_sf0]], c(0.025, 0.975))
        results[[paste0("sig_sf0_cat", cat)]] <- !between(0, ci[1], ci[2])
      }
      
      # sage 주효과
      var_sage <- paste0("b_mu", cat, "_sage")
      if (var_sage %in% names(posterior)) {
        ci <- quantile(posterior[[var_sage]], c(0.025, 0.975))
        results[[paste0("sig_sage_cat", cat)]] <- !between(0, ci[1], ci[2])
      }
    }
    
    if (verbose) {
      message(sprintf("[%s] Simulation %d/%d COMPLETED", Sys.time(), i, n_sim))
    }
    
    return(results)
    
  }, .options = furrr_options(seed = TRUE))
  
  # Power 계산 및 요약
  power_summary <- power_results %>%
    summarise(
      n_total = n(),
      n_converged = sum(converged),
      convergence_rate = mean(converged),
      across(starts_with("sig_"), ~mean(., na.rm = TRUE), .names = "power_{.col}"),
      across(starts_with("est_"), ~mean(., na.rm = TRUE), .names = "mean_{.col}")
    )
  
  return(list(
    summary = power_summary,
    detailed = power_results
  ))
}