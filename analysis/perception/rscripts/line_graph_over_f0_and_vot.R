# Heatmap differences for age-based groupings
# created on December 6, 2025

##########
# set-up #
##########

# set working directory
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

# load libraries
library(tidyverse)
source("preprocessing.R")
source("plot.R")

# Preprocess the data
processed_data <- basic_data_preprocessing(
  workerids_csv_path = "../data/korean_stops_perception_3_poa_all_ages-workerids.csv",
  trials_csv_path = "../data/korean_stops_perception_3_poa_all_ages-trials.csv",
  subject_information_csv_path = "../data/korean_stops_perception_3_poa_all_ages-subject_information.csv"
)

########################################
# 1. Prepare age-group-specific datasets
########################################
MAXIMUM_YOUNG_AGE <- 39
MAXIMUM_MIDDLE_AGE <- 59

processed_data <- processed_data %>%
  mutate(age_group = case_when(
    age <= MAXIMUM_YOUNG_AGE ~ "young",
    age > MAXIMUM_YOUNG_AGE & age <= MAXIMUM_MIDDLE_AGE ~ "middle",
    age > MAXIMUM_MIDDLE_AGE ~ "old"
  ))

age_group_levels <- c("young", "middle", "old")
age_group_labels <- c(young = "Young", middle = "Middle", old = "Old")
age_group_colors <- c("young" = "#ddcc77", "middle" = "#999933", "old" = "#117733")

# Create output directory if it doesn't exist
if (!dir.exists("../graphs/line_graph")) {
  dir.create("../graphs/line_graph", recursive = TRUE)
}

########################################
# 2. Create line graphs by response type
########################################

# Calculate proportions by age group and response
line_data <- processed_data %>%
  group_by(age_group, vot, response) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(age_group, vot) %>%
  mutate(proportion = count / sum(count)) %>%
  ungroup()

# Define response types and labels
response_types <- c("lenis", "tense", "asp")
response_labels <- c(lenis = "Lenis", tense = "Fortis", asp = "Aspirated")

# Create separate graphs for each response type
for (resp in response_types) {
  resp_data <- line_data %>% filter(response == resp)
  
  line_plot <- ggplot(resp_data, aes(x = vot, y = proportion, color = age_group, group = age_group)) +
    geom_line(size = 1.2) +
    geom_point(size = 3) +
    scale_color_manual(
      values = age_group_colors,
      labels = age_group_labels,
      breaks = age_group_levels,
      name = "Age Group"
    ) +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
    scale_x_continuous(breaks = 1:8) +
    labs(
      title = response_labels[resp],
      x = "VOT",
      y = "Proportion"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
      axis.title = element_text(size = 14),
      axis.text = element_text(size = 12),
      legend.title = element_text(size = 12),
      legend.text = element_text(size = 11)
    )
  
  ggsave(
    paste0("../graphs/line_graph/", resp, "_by_age_vot.png"),
    plot = line_plot,
    width = 10,
    height = 6,
    dpi = 300
  )
  
  print(line_plot)
}

# Combined graph with all response types
combined_plot <- ggplot(line_data, aes(x = vot, y = proportion, color = age_group, group = age_group)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  facet_wrap(~ factor(response, levels = response_types, labels = response_labels), ncol = 3) +
  scale_color_manual(
    values = age_group_colors,
    labels = age_group_labels,
    breaks = age_group_levels,
    name = "Age Group"
  ) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  scale_x_continuous(breaks = 1:8) +
  labs(
    title = "Perception by Response Type",
    x = "VOT",
    y = "Proportion"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 11),
    strip.text = element_text(size = 14, face = "bold")
  )

ggsave(
  "../graphs/line_graph/all_responses_by_age_vot.png",
  plot = combined_plot,
  width = 15,
  height = 6,
  dpi = 300
)

print(combined_plot)

########################################
# 3. Create line graphs by response type (F0)
########################################

# Calculate proportions by age group and response (for F0)
line_data_f0 <- processed_data %>%
  group_by(age_group, f0, response) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(age_group, f0) %>%
  mutate(proportion = count / sum(count)) %>%
  ungroup()

# Create separate graphs for each response type (F0)
for (resp in response_types) {
  resp_data <- line_data_f0 %>% filter(response == resp)
  
  line_plot_f0 <- ggplot(resp_data, aes(x = f0, y = proportion, color = age_group, group = age_group)) +
    geom_line(size = 1.2) +
    geom_point(size = 3) +
    scale_color_manual(
      values = age_group_colors,
      labels = age_group_labels,
      breaks = age_group_levels,
      name = "Age Group"
    ) +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
    scale_x_continuous(breaks = 1:8) +
    labs(
      title = response_labels[resp],
      x = "F0",
      y = "Proportion"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
      axis.title = element_text(size = 14),
      axis.text = element_text(size = 12),
      legend.title = element_text(size = 12),
      legend.text = element_text(size = 11)
    )
  
  ggsave(
    paste0("../graphs/line_graph/", resp, "_by_age_f0.png"),
    plot = line_plot_f0,
    width = 10,
    height = 6,
    dpi = 300
  )
  
  print(line_plot_f0)
}

# Combined graph with all response types (F0)
combined_plot_f0 <- ggplot(line_data_f0, aes(x = f0, y = proportion, color = age_group, group = age_group)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  facet_wrap(~ factor(response, levels = response_types, labels = response_labels), ncol = 3) +
  scale_color_manual(
    values = age_group_colors,
    labels = age_group_labels,
    breaks = age_group_levels,
    name = "Age Group"
  ) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  scale_x_continuous(breaks = 1:8) +
  labs(
    title = "Perception by Response Type",
    x = "F0",
    y = "Proportion"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 11),
    strip.text = element_text(size = 14, face = "bold")
  )

ggsave(
  "../graphs/line_graph/all_responses_by_age_f0.png",
  plot = combined_plot_f0,
  width = 15,
  height = 6,
  dpi = 300
)

print(combined_plot_f0)

########################################
# 4. Create line graphs by response type and gender (VOT)
########################################

# Calculate proportions by gender and response (for VOT)
line_data_gender_vot <- processed_data %>%
  mutate(gender = as.character(gender)) %>%
  group_by(gender, vot, response) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(gender, vot) %>%
  mutate(proportion = count / sum(count)) %>%
  ungroup()

# Define gender colors and labels
# Gender values in data are already "Female" and "Male"
# Using colorblind-friendly colors: blue for female, orange for male
gender_colors <- c("Female" = "#332288", "Male" = "#bbbbbb")
gender_labels <- c("Female" = "Female", "Male" = "Male")

# Create separate graphs for each response type (VOT, by gender)
for (resp in response_types) {
  resp_data <- line_data_gender_vot %>% filter(response == resp)
  
  line_plot_gender_vot <- ggplot(resp_data, aes(x = vot, y = proportion, color = gender, group = gender)) +
    geom_line(size = 1.2) +
    geom_point(size = 3) +
    scale_color_manual(
      values = gender_colors,
      labels = gender_labels,
      name = "Gender"
    ) +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
    scale_x_continuous(breaks = 1:8) +
    labs(
      title = response_labels[resp],
      x = "VOT",
      y = "Proportion"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
      axis.title = element_text(size = 14),
      axis.text = element_text(size = 12),
      legend.title = element_text(size = 12),
      legend.text = element_text(size = 11)
    )
  
  ggsave(
    paste0("../graphs/line_graph/", resp, "_by_gender_vot.png"),
    plot = line_plot_gender_vot,
    width = 10,
    height = 6,
    dpi = 300
  )
  
  print(line_plot_gender_vot)
}

# Combined graph with all response types (VOT, by gender)
combined_plot_gender_vot <- ggplot(line_data_gender_vot, aes(x = vot, y = proportion, color = gender, group = gender)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  facet_wrap(~ factor(response, levels = response_types, labels = response_labels), ncol = 3) +
  scale_color_manual(
    values = gender_colors,
    labels = gender_labels,
    name = "Gender"
  ) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  scale_x_continuous(breaks = 1:8) +
  labs(
    title = "Perception by Response Type and Gender",
    x = "VOT",
    y = "Proportion"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 11),
    strip.text = element_text(size = 14, face = "bold")
  )

ggsave(
  "../graphs/line_graph/all_responses_by_gender_vot.png",
  plot = combined_plot_gender_vot,
  width = 15,
  height = 6,
  dpi = 300
)

print(combined_plot_gender_vot)

########################################
# 5. Create line graphs by response type and gender (F0)
########################################

# Calculate proportions by gender and response (for F0)
line_data_gender_f0 <- processed_data %>%
  mutate(gender = as.character(gender)) %>%
  group_by(gender, f0, response) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(gender, f0) %>%
  mutate(proportion = count / sum(count)) %>%
  ungroup()

# Create separate graphs for each response type (F0, by gender)
for (resp in response_types) {
  resp_data <- line_data_gender_f0 %>% filter(response == resp)
  line_plot_gender_f0 <- ggplot(resp_data, aes(x = f0, y = proportion, color = gender, group = gender)) +
    geom_line(size = 1.2) +
    geom_point(size = 3) +
    scale_color_manual(
      values = gender_colors,
      labels = gender_labels,
      name = "Gender"
    ) +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
    scale_x_continuous(breaks = 1:8) +
    labs(
      title = response_labels[resp],
      x = "F0",
      y = "Proportion"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
      axis.title = element_text(size = 14),
      axis.text = element_text(size = 12),
      legend.title = element_text(size = 12),
      legend.text = element_text(size = 11)
    )
  
  ggsave(
    paste0("../graphs/line_graph/", resp, "_by_gender_f0.png"),
    plot = line_plot_gender_f0,
    width = 10,
    height = 6,
    dpi = 300
  )
  
  print(line_plot_gender_f0)
}

combined_plot_gender_f0 <- ggplot(line_data_gender_f0, aes(x = f0, y = proportion, color = gender, group = gender)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  facet_wrap(~ factor(response, levels = response_types, labels = response_labels), ncol = 3) +
  scale_color_manual(
    values = gender_colors,
    labels = gender_labels,
    name = "Gender"
  ) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  scale_x_continuous(breaks = 1:8) +
  labs(
    title = "Perception by Response Type and Gender",
    x = "F0",
    y = "Proportion"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 11),
    strip.text = element_text(size = 14, face = "bold")
  )

ggsave(
  "../graphs/line_graph/all_responses_by_gender_f0.png",
  plot = combined_plot_gender_f0,
  width = 15,
  height = 6,
  dpi = 300
)

print(combined_plot_gender_f0)

########################################
# 6. Create line graphs by response type with age group and gender (VOT)
########################################

# Calculate proportions by age group, gender, and response (for VOT)
line_data_age_gender_vot <- processed_data %>%
  mutate(
    gender = as.character(gender),
    age_gender = paste(age_group, gender, sep = "_")
  ) %>%
  group_by(age_gender, vot, response) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(age_gender, vot) %>%
  mutate(proportion = count / sum(count)) %>%
  ungroup()

# Define age-gender combinations
age_gender_levels <- c("young_Female", "young_Male", "middle_Female", "middle_Male", "old_Female", "old_Male")
age_gender_labels <- c(
  "young_Female" = "Young Female",
  "young_Male" = "Young Male",
  "middle_Female" = "Middle Female",
  "middle_Male" = "Middle Male",
  "old_Female" = "Old Female",
  "old_Male" = "Old Male"
)
age_gender_colors <- c(
  "young_Female" = "#FFC0CB",
  "young_Male" = "#FF6B6B",
  "middle_Female" = "#87CEEB",
  "middle_Male" = "#4169E1",
  "old_Female" = "#90EE90",
  "old_Male" = "#228B22"
)

# Create separate graphs for each response type (VOT, by age-gender)
for (resp in response_types) {
  resp_data <- line_data_age_gender_vot %>% 
    filter(response == resp) %>%
    separate(age_gender, into = c("age_grp", "gnd"), sep = "_", remove = FALSE)
  
  line_plot_age_gender_vot <- ggplot(resp_data, aes(x = vot, y = proportion, color = age_grp, shape = gnd, group = age_gender)) +
    geom_line(size = 0.8) +
    geom_point(size = 3, stroke = 0.8, fill = NA) +
    scale_color_manual(
      values = c("young" = "#ddcc77", "middle" = "#999933", "old" = "#117733"),
      labels = c("young" = "Young", "middle" = "Middle", "old" = "Old"),
      breaks = c("young", "middle", "old"),
      name = "Age Group"
    ) +
    scale_shape_manual(
      values = c("Female" = 21, "Male" = 24),
      labels = c("Female" = "Female", "Male" = "Male"),
      name = "Gender"
    ) +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
    scale_x_continuous(breaks = 1:8) +
    labs(
      title = response_labels[resp],
      x = "VOT",
      y = "Proportion"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
      axis.title = element_text(size = 14),
      axis.text = element_text(size = 12),
      legend.title = element_text(size = 12),
      legend.text = element_text(size = 10)
    )
  
  ggsave(
    paste0("../graphs/line_graph/", resp, "_by_age_gender_vot.png"),
    plot = line_plot_age_gender_vot,
    width = 10,
    height = 6,
    dpi = 300
  )
  
  print(line_plot_age_gender_vot)
}

# Combined graph with all response types (VOT, by age-gender)
combined_data_vot <- line_data_age_gender_vot %>%
  tidyr::separate(age_gender, into = c("age_grp", "gnd"), sep = "_", remove = FALSE)

combined_plot_age_gender_vot <- ggplot(combined_data_vot, aes(x = vot, y = proportion, color = age_grp, shape = gnd, group = age_gender)) +
  geom_line(size = 0.8) +
  geom_point(size = 3, stroke = 0.8, fill = NA) +
  facet_wrap(~ factor(response, levels = response_types, labels = response_labels), ncol = 3) +
  scale_color_manual(
    values = c("young" = "#ddcc77", "middle" = "#999933", "old" = "#117733"),
    labels = c("young" = "Young", "middle" = "Middle", "old" = "Old"),
    breaks = c("young", "middle", "old"),
    name = "Age Group"
  ) +
  scale_shape_manual(
    values = c("Female" = 21, "Male" = 24),
    labels = c("Female" = "Female", "Male" = "Male"),
    name = "Gender"
  ) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  scale_x_continuous(breaks = 1:8) +
  labs(
    title = "Perception by Response Type, Age Group, and Gender",
    x = "VOT",
    y = "Proportion"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    strip.text = element_text(size = 14, face = "bold")
  )

ggsave(
  "../graphs/line_graph/all_responses_by_age_gender_vot.png",
  plot = combined_plot_age_gender_vot,
  width = 15,
  height = 6,
  dpi = 300
)

print(combined_plot_age_gender_vot)

########################################
# 7. Create line graphs by response type with age group and gender (F0)
########################################

# Calculate proportions by age group, gender, and response (for F0)
line_data_age_gender_f0 <- processed_data %>%
  mutate(
    gender = as.character(gender),
    age_gender = paste(age_group, gender, sep = "_")
  ) %>%
  group_by(age_gender, f0, response) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(age_gender, f0) %>%
  mutate(proportion = count / sum(count)) %>%
  ungroup()

# Create separate graphs for each response type (F0, by age-gender)
for (resp in response_types) {
  resp_data <- line_data_age_gender_f0 %>% 
    filter(response == resp) %>%
    separate(age_gender, into = c("age_grp", "gnd"), sep = "_", remove = FALSE)
  
  line_plot_age_gender_f0 <- ggplot(resp_data, aes(x = f0, y = proportion, color = age_grp, shape = gnd, group = age_gender)) +
    geom_line(size = 0.8) +
    geom_point(size = 3, stroke = 0.8, fill = NA) +
    scale_color_manual(
      values = c("young" = "#ddcc77", "middle" = "#999933", "old" = "#117733"),
      labels = c("young" = "Young", "middle" = "Middle", "old" = "Old"),
      breaks = c("young", "middle", "old"),
      name = "Age Group"
    ) +
    scale_shape_manual(
      values = c("Female" = 21, "Male" = 24),
      labels = c("Female" = "Female", "Male" = "Male"),
      name = "Gender"
    ) +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
    scale_x_continuous(breaks = 1:8) +
    labs(
      title = response_labels[resp],
      x = "F0",
      y = "Proportion"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
      axis.title = element_text(size = 14),
      axis.text = element_text(size = 12),
      legend.title = element_text(size = 12),
      legend.text = element_text(size = 10)
    )
  
  ggsave(
    paste0("../graphs/line_graph/", resp, "_by_age_gender_f0.png"),
    plot = line_plot_age_gender_f0,
    width = 10,
    height = 6,
    dpi = 300
  )
  
  print(line_plot_age_gender_f0)
}

# Combined graph with all response types (F0, by age-gender)
combined_data_f0 <- line_data_age_gender_f0 %>%
  separate(age_gender, into = c("age_grp", "gnd"), sep = "_", remove = FALSE)

combined_plot_age_gender_f0 <- ggplot(combined_data_f0, aes(x = f0, y = proportion, color = age_grp, shape = gnd, group = age_gender)) +
  geom_line(size = 0.8) +
  geom_point(size = 3, stroke = 0.8, fill = NA) +
  facet_wrap(~ factor(response, levels = response_types, labels = response_labels), ncol = 3) +
  scale_color_manual(
    values = c("young" = "#ddcc77", "middle" = "#999933", "old" = "#117733"),
    labels = c("young" = "Young", "middle" = "Middle", "old" = "Old"),
    breaks = c("young", "middle", "old"),
    name = "Age Group"
  ) +
  scale_shape_manual(
    values = c("Female" = 21, "Male" = 24),
    labels = c("Female" = "Female", "Male" = "Male"),
    name = "Gender"
  ) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  scale_x_continuous(breaks = 1:8) +
  labs(
    title = "Perception by Response Type, Age Group, and Gender",
    x = "F0",
    y = "Proportion"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    strip.text = element_text(size = 14, face = "bold")
  )

ggsave(
  "../graphs/line_graph/all_responses_by_age_gender_f0.png",
  plot = combined_plot_age_gender_f0,
  width = 15,
  height = 6,
  dpi = 300
)

print(combined_plot_age_gender_f0)

########################################
# 8. Create line graphs by age with response type (F0=2~7, VOT=2~7)
########################################

# Create graphs for all combinations of F0 and VOT from 2 to 7
for (f0_val in 2:7) {
  for (vot_val in 2:7) {
    # Filter data for specific F0 and VOT, calculate proportions for each subject
    raw_data_age_response <- processed_data %>%
      filter(f0 == f0_val, vot == vot_val) %>%
      group_by(subject, age, response) %>%
      summarise(count = n(), .groups = "drop") %>%
      group_by(subject, age) %>%
      mutate(proportion = count / sum(count)) %>%
      ungroup()
    
    # Skip if no data for this combination
    if (nrow(raw_data_age_response) == 0) next
    
    # Create smooth line graph with confidence interval
    age_response_plot <- ggplot(raw_data_age_response, aes(x = age, y = proportion, color = response)) +
      geom_point(alpha = 0.3, size = 2) +
      geom_smooth(aes(group = response, fill = response), method = "loess", se = TRUE, size = 1.2, alpha = 0.2) +
      scale_color_manual(
        values = c("lenis" = "#E69F00", "tense" = "#56B4E9", "asp" = "#009E73"),
        labels = c("lenis" = "Lenis", "tense" = "Fortis", "asp" = "Aspirated"),
        name = "Response"
      ) +
      scale_fill_manual(
        values = c("lenis" = "#E69F00", "tense" = "#56B4E9", "asp" = "#009E73"),
        guide = "none"
      ) +
      scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
      labs(
        title = paste0("Perception by Age (F0=", f0_val, ", VOT=", vot_val, ")"),
        x = "Age",
        y = "Proportion"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 11)
      )
    
    ggsave(
      paste0("../graphs/line_graph/responses_by_age_f0_", f0_val, "_vot_", vot_val, ".png"),
      plot = age_response_plot,
      width = 10,
      height = 6,
      dpi = 300
    )
    
    print(age_response_plot)
  }
}

