# KL Divergence analysis by session steps
# Created on December 10, 2025

##########
# set-up #
##########

# set working directory
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

# load libraries
library(tidyverse)
source("preprocessing.R")

# Age group definitions
maximum_age_in_young_group = 39
minimum_age_in_old_group = 60

# Preprocess the data
processed_data <- basic_data_preprocessing(
  workerids_csv_path = "../data/korean_stops_perception_3_poa_all_ages-workerids.csv",
  trials_csv_path = "../data/korean_stops_perception_3_poa_all_ages-trials.csv",
  subject_information_csv_path = "../data/korean_stops_perception_3_poa_all_ages-subject_information.csv"
)

####################################
# Create age group column
####################################

processed_data <- processed_data %>%
  mutate(age_group = case_when(
    age <= maximum_age_in_young_group ~ "young",
    age >= minimum_age_in_old_group ~ "old",
    TRUE ~ "middle"
  ))

cat("Participants by age group:\n")
cat("Young (age <= 39):", n_distinct(processed_data$subject[processed_data$age_group == "young"]), "\n")
cat("Middle (40-59):", n_distinct(processed_data$subject[processed_data$age_group == "middle"]), "\n")
cat("Old (age >= 60):", n_distinct(processed_data$subject[processed_data$age_group == "old"]), "\n\n")

####################################
# Assign session numbers to each trial
####################################

processed_data <- processed_data %>%
  group_by(subject) %>%
  arrange(subject, slide_number_in_experiment) %>%
  mutate(
    session = case_when(
      slide_number_in_experiment <= 16 ~ NA_integer_,
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
  filter(!is.na(session))

####################################
# Step 1: Group sessions by POA for each participant
####################################

# Assign POA session order for each participant
session_poa_order <- processed_data %>%
  group_by(subject, session, poa, age_group) %>%
  summarise(.groups = "drop") %>%
  group_by(subject, poa) %>%
  arrange(subject, session) %>%
  mutate(poa_session_num = row_number()) %>%
  ungroup()

cat("Session POA ordering created.\n")
cat("Sample of session POA orders:\n")
print(head(session_poa_order, 20))
cat("\n")

# Join back to main data
processed_data <- processed_data %>%
  left_join(session_poa_order %>% select(subject, session, poa, poa_session_num), 
            by = c("subject", "session", "poa"))

####################################
# Step 2: Calculate probability distributions for each session
####################################

# Calculate response probabilities for each (subject, age_group, poa, poa_session_num, vot, f0)
session_distributions <- processed_data %>%
  group_by(age_group, subject, poa, poa_session_num, vot, f0) %>%
  summarise(
    lenis_count = sum(response == "lenis"),
    tense_count = sum(response == "tense"),
    asp_count = sum(response == "asp"),
    total_count = n(),
    .groups = "drop"
  ) %>%
  mutate(
    lenis_prob = lenis_count / total_count,
    tense_prob = tense_count / total_count,
    asp_prob = asp_count / total_count
  )

cat("Probability distributions calculated.\n")

####################################
# Step 3: Calculate KL divergence between consecutive sessions
####################################

# Function to calculate KL divergence
# KL(P||Q) = sum(P * log(P/Q))
calculate_kl_divergence <- function(p, q, epsilon = 1e-10) {
  # Add small epsilon to avoid log(0)
  p <- p + epsilon
  q <- q + epsilon
  # Normalize
  p <- p / sum(p)
  q <- q / sum(q)
  # Calculate KL divergence
  kl <- sum(p * log(p / q))
  return(kl)
}

# Calculate KL divergence for consecutive POA sessions
kl_divergences <- list()

for (subj in unique(session_distributions$subject)) {
  # Get age group for this subject
  age_grp <- unique(session_distributions$age_group[session_distributions$subject == subj])
  
  for (p in unique(session_distributions$poa)) {
    # Get data for this subject and POA
    subj_poa_data <- session_distributions %>%
      filter(subject == subj, poa == p) %>%
      arrange(poa_session_num)
    
    # Calculate KL divergence between session 1 and 2
    if (all(c(1, 2) %in% subj_poa_data$poa_session_num)) {
      session1 <- subj_poa_data %>% filter(poa_session_num == 1)
      session2 <- subj_poa_data %>% filter(poa_session_num == 2)
      
      # For each (vot, f0) pair, calculate KL divergence
      for (i in 1:nrow(session1)) {
        vot_val <- session1$vot[i]
        f0_val <- session1$f0[i]
        
        session2_row <- session2 %>% filter(vot == vot_val, f0 == f0_val)
        
        if (nrow(session2_row) == 1) {
          p_dist <- c(session1$lenis_prob[i], session1$tense_prob[i], session1$asp_prob[i])
          q_dist <- c(session2_row$lenis_prob, session2_row$tense_prob, session2_row$asp_prob)
          
          kl <- calculate_kl_divergence(p_dist, q_dist)
          
          kl_divergences[[length(kl_divergences) + 1]] <- data.frame(
            age_group = age_grp,
            subject = subj,
            poa = p,
            step = 1,
            vot = vot_val,
            f0 = f0_val,
            kl_divergence = kl
          )
        }
      }
    }
    
    # Calculate KL divergence between session 2 and 3
    if (all(c(2, 3) %in% subj_poa_data$poa_session_num)) {
      session2 <- subj_poa_data %>% filter(poa_session_num == 2)
      session3 <- subj_poa_data %>% filter(poa_session_num == 3)
      
      # For each (vot, f0) pair, calculate KL divergence
      for (i in 1:nrow(session2)) {
        vot_val <- session2$vot[i]
        f0_val <- session2$f0[i]
        
        session3_row <- session3 %>% filter(vot == vot_val, f0 == f0_val)
        
        if (nrow(session3_row) == 1) {
          p_dist <- c(session2$lenis_prob[i], session2$tense_prob[i], session2$asp_prob[i])
          q_dist <- c(session3_row$lenis_prob, session3_row$tense_prob, session3_row$asp_prob)
          
          kl <- calculate_kl_divergence(p_dist, q_dist)
          
          kl_divergences[[length(kl_divergences) + 1]] <- data.frame(
            age_group = age_grp,
            subject = subj,
            poa = p,
            step = 2,
            vot = vot_val,
            f0 = f0_val,
            kl_divergence = kl
          )
        }
      }
    }
  }
}

kl_data <- bind_rows(kl_divergences)

cat("KL divergences calculated.\n")
cat("Number of KL divergence calculations:", nrow(kl_data), "\n\n")

####################################
# Step 4: Average KL divergence by step for each participant
####################################

# Average across POA for each participant, age group, and step
participant_step_kl <- kl_data %>%
  group_by(age_group, subject, step) %>%
  summarise(
    mean_kl = mean(kl_divergence, na.rm = TRUE),
    .groups = "drop"
  )

cat("Participant-level KL divergences by step and age group:\n")
print(head(participant_step_kl, 20))
cat("\n")

####################################
# Step 5: Average across participants
####################################

# Average across all participants for each step and age group
average_step_kl <- participant_step_kl %>%
  group_by(age_group, step) %>%
  summarise(
    mean_kl = mean(mean_kl, na.rm = TRUE),
    sd_kl = sd(mean_kl, na.rm = TRUE),
    n_participants = n(),
    se_kl = sd_kl / sqrt(n_participants),
    .groups = "drop"
  )

cat("Average KL divergence by step and age group:\n")
print(average_step_kl)
cat("\n")

####################################
# Step 6: Create bar charts
####################################

# Create output directory
dir.create("../graphs/kl_divergence", recursive = TRUE, showWarnings = FALSE)

# Define colors for age groups
age_group_colors <- c("young" = "#4C6FB5", "middle" = "#E8A628", "old" = "#C94E4E")

# Set factor levels for age_group to control ordering
average_step_kl <- average_step_kl %>%
  mutate(age_group = factor(age_group, levels = c("young", "middle", "old")))

# Create combined bar chart with facets for each age group
kl_barchart_combined <- ggplot(average_step_kl, aes(x = factor(step), y = mean_kl, fill = age_group)) +
  geom_bar(stat = "identity", alpha = 0.8, width = 0.6) +
  geom_errorbar(
    aes(ymin = mean_kl - se_kl, ymax = mean_kl + se_kl),
    width = 0.2,
    color = "black"
  ) +
  geom_text(
    aes(label = round(mean_kl, 4)),
    vjust = -0.5,
    size = 3.5
  ) +
  scale_fill_manual(values = age_group_colors) +
  scale_x_discrete(labels = c("1" = "Step 1", "2" = "Step 2")) +
  facet_wrap(~age_group, ncol = 3, 
             labeller = labeller(age_group = c(
               "young" = "Young (≤39)",
               "middle" = "Middle (40-59)",
               "old" = "Old (≥60)"
             ))) +
  labs(
    title = "Mean KL Divergence by Step Across Age Groups",
    subtitle = "Step 1: 1st → 2nd occurrence of each POA | Step 2: 2nd → 3rd occurrence of each POA",
    x = "Step",
    y = "Mean KL Divergence"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 10, color = "gray40"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.position = "none",
    strip.text = element_text(size = 11, face = "bold")
  ) +
  ylim(0, max(average_step_kl$mean_kl + average_step_kl$se_kl) * 1.15)

# Save the combined plot
ggsave(
  "../graphs/kl_divergence/step_kl_divergence_barchart_all_ages.png",
  plot = kl_barchart_combined,
  width = 12,
  height = 5,
  dpi = 300
)

cat("Combined bar chart saved to: ../graphs/kl_divergence/step_kl_divergence_barchart_all_ages.png\n")

# Create individual plots for each age group
for (age_grp in c("young", "middle", "old")) {
  age_label <- case_when(
    age_grp == "young" ~ "Young (≤39)",
    age_grp == "middle" ~ "Middle (40-59)",
    age_grp == "old" ~ "Old (≥60)"
  )
  
  plot_data <- average_step_kl %>% filter(age_group == age_grp)
  
  individual_plot <- ggplot(plot_data, aes(x = factor(step), y = mean_kl)) +
    geom_bar(stat = "identity", fill = age_group_colors[age_grp], alpha = 0.8, width = 0.6) +
    geom_errorbar(
      aes(ymin = mean_kl - se_kl, ymax = mean_kl + se_kl),
      width = 0.2,
      color = "black"
    ) +
    geom_text(
      aes(label = round(mean_kl, 4)),
      vjust = -0.5,
      size = 4
    ) +
    labs(
      title = paste("Mean KL Divergence by Step -", age_label),
      subtitle = "Step 1: 1st → 2nd occurrence of each POA | Step 2: 2nd → 3rd occurrence of each POA",
      x = "Step",
      y = "Mean KL Divergence"
    ) +
    scale_x_discrete(labels = c("1" = "Step 1", "2" = "Step 2")) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, size = 10, color = "gray40"),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 10)
    ) +
    ylim(0, max(plot_data$mean_kl + plot_data$se_kl) * 1.15)
  
  filename <- paste0("../graphs/kl_divergence/step_kl_divergence_barchart_", age_grp, ".png")
  ggsave(filename, plot = individual_plot, width = 8, height = 6, dpi = 300)
  cat(paste0("Individual bar chart saved to: ", filename, "\n"))
}

# Save the data as CSV for reference
write.csv(
  average_step_kl,
  "../graphs/kl_divergence/average_step_kl_divergence_all_ages.csv",
  row.names = FALSE
)

write.csv(
  participant_step_kl,
  "../graphs/kl_divergence/participant_step_kl_divergence_all_ages.csv",
  row.numbers = FALSE
)

write.csv(
  kl_data,
  "../graphs/kl_divergence/all_kl_divergences_all_ages.csv",
  row.names = FALSE
)

cat("KL divergence data saved to CSV files in ../graphs/kl_divergence/\n")
print("Analysis complete!")
