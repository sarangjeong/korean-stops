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

########################################
# 2. Prepare heatmap data for each age group
########################################
group_plot_data_list <- setNames(vector("list", length(age_group_levels)), age_group_levels)

for (group_key in age_group_levels) {
  group_data <- processed_data %>%
    filter(age_group == group_key)

  if (nrow(group_data) == 0) {
    warning(paste("No observations for age group:", group_key))
    next
  }

  group_plot_data_list[[group_key]] <- plot_data_preprocessing(group_data)
}

########################################
# 3. Define response types and age comparisons
########################################
response_types <- list(
  asp = list(column = "asp", label = "Aspirated", directory = "asp"),
  fortis = list(column = "tense", label = "Fortis", directory = "fortis"),
  lenis = list(column = "lenis", label = "Lenis", directory = "lenis")
)

age_comparison_pairs <- list(
  old_middle = list(minuend = "old", subtrahend = "middle", label = "Old - Middle"),
  middle_young = list(minuend = "middle", subtrahend = "young", label = "Middle - Young"),
  old_young = list(minuend = "old", subtrahend = "young", label = "Old - Young")
)

########################################
# 3b. Prepare POA-specific datasets
########################################
poa_levels <- c("dor", "lab", "cor")
poa_labels <- c(dor = "Dorsal", lab = "Labial", cor = "Coronal")

poa_plot_data_list <- setNames(vector("list", length(poa_levels)), poa_levels)

for (poa_key in poa_levels) {
  poa_data <- processed_data %>%
    filter(poa == poa_key)
  
  if (nrow(poa_data) == 0) {
    warning(paste("No observations for POA:", poa_key))
    next
  }
  
  poa_plot_data_list[[poa_key]] <- plot_data_preprocessing(poa_data)
}

poa_comparison_pairs <- list(
  dor_lab = list(minuend = "dor", subtrahend = "lab", label = "Dorsal - Labial"),
  lab_cor = list(minuend = "lab", subtrahend = "cor", label = "Labial - Coronal"),
  dor_cor = list(minuend = "dor", subtrahend = "cor", label = "Dorsal - Coronal")
)

########################################
# 3c. Prepare gender-specific datasets
########################################
gender_levels <- c("Male", "Female")
gender_labels <- c(Male = "Male", Female = "Female")

gender_plot_data_list <- setNames(vector("list", length(gender_levels)), gender_levels)

for (gender_key in gender_levels) {
  gender_data <- processed_data %>%
    filter(gender == gender_key)
  
  if (nrow(gender_data) == 0) {
    warning(paste("No observations for gender:", gender_key))
    next
  }
  
  gender_plot_data_list[[gender_key]] <- plot_data_preprocessing(gender_data)
}

gender_comparison_pairs <- list(
  Male_Female = list(minuend = "Male", subtrahend = "Female", label = "Male - Female")
)

########################################
# 4. Helper to plot difference heatmaps
########################################
plot_difference_heatmap <- function(data, title, fill_label, file_path, limit) {
  limit <- 0.5

  heatmap_plot <- ggplot(data, aes(x = vot, y = f0)) +
    geom_tile(aes(fill = diff_value), color = "white") +
    geom_text(aes(label = sprintf("%.2f", diff_value)), size = 3) +
    scale_fill_gradient2(
      low = "#2166AC",
      mid = "white",
      high = "#B2182B",
      midpoint = 0,
      limits = c(-limit, limit),
      breaks = seq(-limit, limit, by = 0.25),
      name = fill_label,
      oob = scales::squish
    ) +
    labs(
      x = "VOT",
      y = "F0",
      title = title,
      subtitle = "Difference in response proportion (minuend - subtrahend)"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5)
    ) +
    coord_fixed()

  ggsave(
    filename = file_path,
    plot = heatmap_plot,
    width = 10,
    height = 6,
    dpi = "retina"
  )

  return(heatmap_plot)
}

########################################
# 5. Compute differences and generate plots
########################################

# Helper function to compute differences for any grouping
compute_differences <- function(plot_data_list, comparison_pairs, response_types) {
  difference_results <- list()
  response_max_abs <- setNames(rep(0, length(response_types)), names(response_types))
  
  for (response_key in names(response_types)) {
    difference_results[[response_key]] <- list()
    
    for (comparison_key in names(comparison_pairs)) {
      minuend_key <- comparison_pairs[[comparison_key]]$minuend
      subtrahend_key <- comparison_pairs[[comparison_key]]$subtrahend
      
      minuend_data <- plot_data_list[[minuend_key]]
      subtrahend_data <- plot_data_list[[subtrahend_key]]
      
      if (is.null(minuend_data) || is.null(subtrahend_data)) {
        warning(paste(
          "Skipping comparison", comparison_key,
          "because one of the groups has no data"
        ))
        next
      }
      
      diff_data <- minuend_data %>%
        select(f0, vot, minuend_value = all_of(response_types[[response_key]]$column)) %>%
        inner_join(
          subtrahend_data %>%
            select(f0, vot, subtrahend_value = all_of(response_types[[response_key]]$column)),
          by = c("f0", "vot")
        ) %>%
        mutate(diff_value = minuend_value - subtrahend_value)
      
      max_abs_value <- max(abs(diff_data$diff_value), na.rm = TRUE)
      response_max_abs[[response_key]] <- max(response_max_abs[[response_key]], max_abs_value)
      
      difference_results[[response_key]][[comparison_key]] <- diff_data
    }
  }
  
  return(list(differences = difference_results, max_abs = response_max_abs))
}

# Helper function to save difference plots
save_difference_plots <- function(difference_results, comparison_pairs, response_types, output_subdir) {
  base_output_dir <- file.path("../graphs/heatmap_differences", output_subdir)
  
  for (response_key in names(response_types)) {
    response_dir <- file.path(base_output_dir, response_types[[response_key]]$directory)
    dir.create(response_dir, recursive = TRUE, showWarnings = FALSE)
    
    for (comparison_key in names(comparison_pairs)) {
      diff_data <- difference_results[[response_key]][[comparison_key]]
      
      if (is.null(diff_data)) {
        next
      }
      
      output_path <- file.path(
        response_dir,
        paste0(response_key, "_difference_", comparison_key, ".png")
      )
      
      plot_difference_heatmap(
        data = diff_data,
        title = paste(response_types[[response_key]]$label, "responses:", comparison_pairs[[comparison_key]]$label),
        fill_label = paste0("Δ ", response_types[[response_key]]$label, " (proportion)"),
        file_path = output_path,
        limit = 0.5
      )
      
      message(
        paste(
          "Created", response_types[[response_key]]$label,
          "difference heatmap for", comparison_pairs[[comparison_key]]$label,
          "in", output_subdir
        )
      )
    }
  }
}

########################################
# 5a. Age-based differences
########################################
age_results <- compute_differences(group_plot_data_list, age_comparison_pairs, response_types)
save_difference_plots(age_results$differences, age_comparison_pairs, response_types, "age")

########################################
# 5b. POA-based differences
########################################
poa_results <- compute_differences(poa_plot_data_list, poa_comparison_pairs, response_types)
save_difference_plots(poa_results$differences, poa_comparison_pairs, response_types, "poa")

########################################
# 5c. Gender-based differences
########################################
gender_results <- compute_differences(gender_plot_data_list, gender_comparison_pairs, response_types)
save_difference_plots(gender_results$differences, gender_comparison_pairs, response_types, "gender")

print("All difference heatmaps have been generated!")

