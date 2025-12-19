# Response time heatmap
# created on December 9, 2025

##########
# set-up #
##########

# set working directory
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

# load libraries
library(tidyverse)
library(patchwork)

# Read the data files
trials_data <- read_csv("../data/korean_stops_perception_3_poa_all_ages-trials.csv")
workerids_data <- read_csv("../data/korean_stops_perception_3_poa_all_ages-workerids.csv")
subject_info <- read_csv("../data/korean_stops_perception_3_poa_all_ages-subject_information.csv")

# Join to get age information
trials_with_age <- trials_data %>%
  left_join(workerids_data, by = "workerid") %>%
  left_join(subject_info %>% select(workerid, age, gender), by = "workerid")

# Define age groups
MAXIMUM_YOUNG_AGE <- 39
MAXIMUM_MIDDLE_AGE <- 59

########################################
# Remove outliers and calculate average response time
########################################

# First, get all valid trials
valid_trials <- trials_with_age %>%
  filter(
    id != "practice",
    id != "check",
    !is.na(response_time_milliseconds),
    !is.na(vot),
    !is.na(f0),
    response_time_milliseconds > 0
  )

# Calculate mean and sd for outlier detection
overall_mean <- mean(valid_trials$response_time_milliseconds, na.rm = TRUE)
overall_sd <- sd(valid_trials$response_time_milliseconds, na.rm = TRUE)

# Define outlier boundaries (mean ± 3*SD)
lower_boundary <- overall_mean - 3 * overall_sd
upper_boundary <- overall_mean + 3 * overall_sd

cat("Outlier detection boundaries:\n")
cat(sprintf("  Lower boundary (mean - 3*SD): %.2f ms\n", lower_boundary))
cat(sprintf("  Mean: %.2f ms\n", overall_mean))
cat(sprintf("  Upper boundary (mean + 3*SD): %.2f ms\n", upper_boundary))
cat(sprintf("  Standard deviation: %.2f ms\n\n", overall_sd))

# Filter out outliers
trials_filtered <- valid_trials %>%
  filter(
    response_time_milliseconds >= lower_boundary,
    response_time_milliseconds <= upper_boundary
  )

n_removed <- nrow(valid_trials) - nrow(trials_filtered)
cat(sprintf("Removed %d outliers (%.2f%% of data)\n\n", 
            n_removed, 100 * n_removed / nrow(valid_trials)))

########################################
# Calculate average response time for all data
########################################

response_time_data_all <- trials_filtered %>%
  group_by(vot, f0) %>%
  summarise(
    mean_rt = mean(response_time_milliseconds, na.rm = TRUE),
    count = n(),
    .groups = "drop"
  )

# Display summary
cat("Overall response time statistics (after outlier removal):\n")
print(summary(response_time_data_all$mean_rt))
cat("\nNumber of data points per VOT-F0 combination:\n")
print(summary(response_time_data_all$count))

########################################
# Calculate average response time by age group
########################################

# Young group
response_time_data_young <- trials_filtered %>%
  filter(age <= MAXIMUM_YOUNG_AGE) %>%
  group_by(vot, f0) %>%
  summarise(
    mean_rt = mean(response_time_milliseconds, na.rm = TRUE),
    count = n(),
    .groups = "drop"
  )

# Middle group
response_time_data_middle <- trials_filtered %>%
  filter(
    age > MAXIMUM_YOUNG_AGE,
    age <= MAXIMUM_MIDDLE_AGE
  ) %>%
  group_by(vot, f0) %>%
  summarise(
    mean_rt = mean(response_time_milliseconds, na.rm = TRUE),
    count = n(),
    .groups = "drop"
  )

# Old group
response_time_data_old <- trials_filtered %>%
  filter(age > MAXIMUM_MIDDLE_AGE) %>%
  group_by(vot, f0) %>%
  summarise(
    mean_rt = mean(response_time_milliseconds, na.rm = TRUE),
    count = n(),
    .groups = "drop"
  )

cat("\nYoung group response time statistics:\n")
print(summary(response_time_data_young$mean_rt))
cat("\nMiddle group response time statistics:\n")
print(summary(response_time_data_middle$mean_rt))
cat("\nOld group response time statistics:\n")
print(summary(response_time_data_old$mean_rt))

########################################
# Helper function to create heatmap
########################################

create_rt_heatmap <- function(data, title, subtitle, use_common_scale = FALSE) {
  if (use_common_scale) {
    limits <- c(
      min(c(response_time_data_all$mean_rt, response_time_data_young$mean_rt, 
            response_time_data_middle$mean_rt, response_time_data_old$mean_rt)),
      max(c(response_time_data_all$mean_rt, response_time_data_young$mean_rt, 
            response_time_data_middle$mean_rt, response_time_data_old$mean_rt))
    )
  } else {
    limits <- c(min(data$mean_rt, na.rm = TRUE), max(data$mean_rt, na.rm = TRUE))
  }
  
  ggplot(data, aes(x = vot, y = f0)) +
    geom_tile(aes(fill = mean_rt), color = "white") +
    geom_text(aes(label = sprintf("%.0f", mean_rt)), size = 3, color = "white") +
    scale_fill_viridis_c(
      option = "viridis",
      name = "Response Time\n(milliseconds)",
      limits = limits,
      direction = -1
    ) +
    labs(
      x = "VOT",
      y = "F0",
      title = title,
      subtitle = subtitle
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, size = 11),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 10),
      legend.position = "right"
    ) +
    coord_fixed()
}

########################################
# Create and save heatmaps
########################################

output_dir <- "../graphs"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# Overall heatmap
rt_heatmap_all <- create_rt_heatmap(
  response_time_data_all,
  "Average Response Time by VOT and F0 (All Ages)",
  paste0("Averaged across all participants and trials (n = ", 
         sum(response_time_data_all$count), " observations)")
)
print(rt_heatmap_all)
ggsave(
  filename = file.path(output_dir, "response_time_heatmap_all.png"),
  plot = rt_heatmap_all,
  width = 10,
  height = 6,
  dpi = "retina"
)

# Young group heatmap (individual scale)
rt_heatmap_young_individual <- create_rt_heatmap(
  response_time_data_young,
  "Average Response Time by VOT and F0 (Young: 20-39)",
  paste0("n = ", sum(response_time_data_young$count), " observations"),
  use_common_scale = FALSE
)
print(rt_heatmap_young_individual)
ggsave(
  filename = file.path(output_dir, "response_time_heatmap_young.png"),
  plot = rt_heatmap_young_individual,
  width = 10,
  height = 6,
  dpi = "retina"
)

# Middle group heatmap (individual scale)
rt_heatmap_middle_individual <- create_rt_heatmap(
  response_time_data_middle,
  "Average Response Time by VOT and F0 (Middle: 40-59)",
  paste0("n = ", sum(response_time_data_middle$count), " observations"),
  use_common_scale = FALSE
)
print(rt_heatmap_middle_individual)
ggsave(
  filename = file.path(output_dir, "response_time_heatmap_middle.png"),
  plot = rt_heatmap_middle_individual,
  width = 10,
  height = 6,
  dpi = "retina"
)

# Old group heatmap (individual scale)
rt_heatmap_old_individual <- create_rt_heatmap(
  response_time_data_old,
  "Average Response Time by VOT and F0 (Old: 60+)",
  paste0("n = ", sum(response_time_data_old$count), " observations"),
  use_common_scale = FALSE
)
print(rt_heatmap_old_individual)
ggsave(
  filename = file.path(output_dir, "response_time_heatmap_old.png"),
  plot = rt_heatmap_old_individual,
  width = 10,
  height = 6,
  dpi = "retina"
)

########################################
# Create combined plot with all age groups (common scale)
########################################

# Create versions with common scale for comparison
rt_heatmap_young <- create_rt_heatmap(
  response_time_data_young,
  "Young: 20-39",
  paste0("n = ", sum(response_time_data_young$count)),
  use_common_scale = TRUE
)

rt_heatmap_middle <- create_rt_heatmap(
  response_time_data_middle,
  "Middle: 40-59",
  paste0("n = ", sum(response_time_data_middle$count)),
  use_common_scale = TRUE
)

rt_heatmap_old <- create_rt_heatmap(
  response_time_data_old,
  "Old: 60+",
  paste0("n = ", sum(response_time_data_old$count)),
  use_common_scale = TRUE
)

combined_age_plot <- rt_heatmap_young + rt_heatmap_middle + rt_heatmap_old +
  plot_layout(ncol = 3, guides = "collect") &
  theme(legend.position = "right")

print(combined_age_plot)

ggsave(
  filename = file.path(output_dir, "response_time_heatmap_age_groups_combined.png"),
  plot = combined_age_plot,
  width = 24,
  height = 6,
  dpi = "retina"
)

cat("\nAll response time heatmaps have been saved to:", output_dir, "\n")
cat("- response_time_heatmap_all.png\n")
cat("- response_time_heatmap_young.png\n")
cat("- response_time_heatmap_middle.png\n")
cat("- response_time_heatmap_old.png\n")
cat("- response_time_heatmap_age_groups_combined.png (all age groups side by side)\n")

