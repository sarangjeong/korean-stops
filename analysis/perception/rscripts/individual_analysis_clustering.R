# Age-Constrained Clustering Analysis
# Created for clustering subjects based on F0 and VOT coefficients
# with the constraint that age ranges of clusters should not overlap
# created by Sarang Jeong on November 11, 2025

##########
# set-up #
##########

# set working directory
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

# load libraries
library(tidyverse)
library(cluster)
library(factoextra)

# Load coefficient data from individual models
coefficients_data <- read.csv("../graphs/coefficients_data_no_random_effect.csv")

# Remove rows with missing values
clustering_data <- coefficients_data %>%
  filter(!is.na(asp_f0) & !is.na(asp_vot) & !is.na(tense_f0) & !is.na(tense_vot))

cat("Number of subjects for clustering:", nrow(clustering_data), "\n")

##################################################
# Age-Constrained Clustering Function
##################################################

age_constrained_clustering <- function(data, k_clusters, max_iterations = 1000, n_trials = 50) {
  # data: dataframe with columns for clustering features and age
  # k_clusters: number of clusters to create
  # max_iterations: maximum iterations for optimization
  # n_trials: number of random initializations to try
  
  cat("Starting age-constrained clustering with", k_clusters, "clusters\n")
  
  # Prepare features for clustering (F0 and VOT coefficients)
  features <- data %>%
    select(asp_f0, asp_vot, tense_f0, tense_vot) %>%
    scale()  # Standardize features
  
  best_solution <- NULL
  best_score <- -Inf
  
  for (trial in 1:n_trials) {
    # Random initialization
    initial_centers <- features[sample(nrow(features), k_clusters), ]
    current_centers <- initial_centers
    
    converged <- FALSE
    iteration <- 0
    
    while (!converged && iteration < max_iterations) {
      iteration <- iteration + 1
      
      # Assign each point to nearest cluster
      distances <- as.matrix(dist(rbind(current_centers, features)))
      distances <- distances[(k_clusters + 1):nrow(distances), 1:k_clusters]
      cluster_assignments <- apply(distances, 1, which.min)
      
      # Check age constraint
      age_ranges <- data.frame(
        cluster = cluster_assignments,
        age = data$age
      ) %>%
        group_by(cluster) %>%
        summarise(
          min_age = min(age),
          max_age = max(age),
          .groups = 'drop'
        ) %>%
        arrange(min_age)
      
      # Check if age ranges overlap
      age_valid <- TRUE
      if (nrow(age_ranges) > 1) {
        for (i in 1:(nrow(age_ranges) - 1)) {
          if (age_ranges$max_age[i] >= age_ranges$min_age[i + 1]) {
            age_valid <- FALSE
            break
          }
        }
      }
      
      if (!age_valid) {
        # If age constraint violated, try to adjust assignments
        # Sort subjects by age and assign to clusters in order
        sorted_indices <- order(data$age)
        cluster_size <- ceiling(nrow(data) / k_clusters)
        
        new_assignments <- rep(1:k_clusters, each = cluster_size, length.out = nrow(data))
        cluster_assignments[sorted_indices] <- new_assignments
      }
      
      # Update cluster centers
      new_centers <- matrix(0, nrow = k_clusters, ncol = ncol(features))
      for (k in 1:k_clusters) {
        cluster_points <- features[cluster_assignments == k, , drop = FALSE]
        if (nrow(cluster_points) > 0) {
          new_centers[k, ] <- colMeans(cluster_points)
        } else {
          # If cluster is empty, reinitialize randomly
          new_centers[k, ] <- features[sample(nrow(features), 1), ]
        }
      }
      
      # Check convergence
      if (max(abs(new_centers - current_centers)) < 1e-6) {
        converged <- TRUE
      }
      
      current_centers <- new_centers
    }
    
    # Calculate final age ranges
    final_age_ranges <- data.frame(
      cluster = cluster_assignments,
      age = data$age
    ) %>%
      group_by(cluster) %>%
      summarise(
        min_age = min(age),
        max_age = max(age),
        .groups = 'drop'
      ) %>%
      arrange(min_age)
    
    # Check if solution is valid (no overlap)
    is_valid <- TRUE
    if (nrow(final_age_ranges) > 1) {
      for (i in 1:(nrow(final_age_ranges) - 1)) {
        if (final_age_ranges$max_age[i] >= final_age_ranges$min_age[i + 1]) {
          is_valid <- FALSE
          break
        }
      }
    }
    
    if (is_valid) {
      # Calculate within-cluster sum of squares as quality metric
      wcss <- 0
      for (k in 1:k_clusters) {
        cluster_points <- features[cluster_assignments == k, , drop = FALSE]
        if (nrow(cluster_points) > 0) {
          center <- current_centers[k, ]
          wcss <- wcss + sum(rowSums((cluster_points - matrix(center, nrow = nrow(cluster_points), ncol = ncol(features), byrow = TRUE))^2))
        }
      }
      
      score <- -wcss  # Negative because we want to minimize WCSS
      
      if (score > best_score) {
        best_score <- score
        best_solution <- list(
          clusters = cluster_assignments,
          centers = current_centers,
          age_ranges = final_age_ranges,
          wcss = wcss,
          trial = trial,
          iterations = iteration
        )
      }
    }
    
    if (trial %% 10 == 0) {
      cat("Completed", trial, "trials...\n")
    }
  }
  
  if (is.null(best_solution)) {
    cat("Warning: Could not find a valid solution with non-overlapping age ranges\n")
    cat("Falling back to age-sorted clustering\n")
    
    # Fallback: sort by age and divide into k groups
    sorted_indices <- order(data$age)
    cluster_size <- ceiling(nrow(data) / k_clusters)
    cluster_assignments <- rep(1:k_clusters, each = cluster_size, length.out = nrow(data))
    cluster_assignments <- cluster_assignments[order(sorted_indices)]
    
    # Calculate centers for fallback solution
    new_centers <- matrix(0, nrow = k_clusters, ncol = ncol(features))
    for (k in 1:k_clusters) {
      cluster_points <- features[cluster_assignments == k, , drop = FALSE]
      if (nrow(cluster_points) > 0) {
        new_centers[k, ] <- colMeans(cluster_points)
      }
    }
    
    final_age_ranges <- data.frame(
      cluster = cluster_assignments,
      age = data$age
    ) %>%
      group_by(cluster) %>%
      summarise(
        min_age = min(age),
        max_age = max(age),
        .groups = 'drop'
      ) %>%
      arrange(min_age)
    
    wcss <- 0
    for (k in 1:k_clusters) {
      cluster_points <- features[cluster_assignments == k, , drop = FALSE]
      if (nrow(cluster_points) > 0) {
        center <- new_centers[k, ]
        wcss <- wcss + sum(rowSums((cluster_points - matrix(center, nrow = nrow(cluster_points), ncol = ncol(features), byrow = TRUE))^2))
      }
    }
    
    best_solution <- list(
      clusters = cluster_assignments,
      centers = new_centers,
      age_ranges = final_age_ranges,
      wcss = wcss,
      trial = NA,
      iterations = NA
    )
  }
  
  return(best_solution)
}

##################################################
# Perform Age-Constrained Clustering
##################################################

# Try different numbers of clusters (2 to 5)
set.seed(42)  # For reproducibility

results_list <- list()

for (k in 2:5) {
  cat("\n========================================\n")
  cat("Clustering with", k, "clusters\n")
  cat("========================================\n")
  
  result <- age_constrained_clustering(clustering_data, k_clusters = k, n_trials = 100)
  
  # Add cluster assignments to data
  clustering_data_k <- clustering_data
  clustering_data_k$cluster <- result$clusters
  
  # Print age ranges
  cat("\nAge ranges for each cluster:\n")
  print(result$age_ranges)
  
  # Print cluster statistics
  cluster_stats <- clustering_data_k %>%
    group_by(cluster) %>%
    summarise(
      n = n(),
      mean_age = mean(age),
      sd_age = sd(age),
      mean_asp_f0 = mean(asp_f0),
      mean_asp_vot = mean(asp_vot),
      mean_tense_f0 = mean(tense_f0),
      mean_tense_vot = mean(tense_vot),
      .groups = 'drop'
    )
  
  cat("\nCluster statistics:\n")
  print(cluster_stats)
  
  cat("\nWithin-cluster sum of squares:", result$wcss, "\n")
  
  # Save results
  results_list[[paste0("k", k)]] <- list(
    result = result,
    data = clustering_data_k,
    stats = cluster_stats
  )
  
  # Save clustered data
  output_path <- sprintf("../graphs/age_constrained_clusters_k%d.csv", k)
  write.csv(clustering_data_k, output_path, row.names = FALSE)
  cat("Saved to:", output_path, "\n")
}

##################################################
# Visualization
##################################################

# Visualize clustering results for k=3 (similar to young/middle/old)
k <- 3
clustering_result <- results_list[[paste0("k", k)]]

# Plot 1: Age distribution by cluster
age_plot <- ggplot(clustering_result$data, aes(x = factor(cluster), y = age, fill = factor(cluster))) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  labs(
    title = sprintf("Age Distribution Across %d Clusters", k),
    subtitle = "Age ranges do not overlap between clusters",
    x = "Cluster",
    y = "Age"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

print(age_plot)
ggsave(
  sprintf("../graphs/age_constrained_clustering_k%d_age_distribution.png", k),
  plot = age_plot,
  width = 8,
  height = 6,
  dpi = 300
)

# Plot 2: Coefficient space (ASP)
asp_coef_plot <- ggplot(clustering_result$data, aes(x = asp_vot, y = asp_f0, color = factor(cluster))) +
  geom_point(size = 3, alpha = 0.7) +
  labs(
    title = sprintf("Aspirated Contrast: F0 vs VOT Coefficients (%d Clusters)", k),
    subtitle = "Clusters constrained by non-overlapping age ranges",
    x = "VOT Coefficient",
    y = "F0 Coefficient",
    color = "Cluster"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    legend.position = "right"
  )

print(asp_coef_plot)
ggsave(
  sprintf("../graphs/age_constrained_clustering_k%d_asp_coefficients.png", k),
  plot = asp_coef_plot,
  width = 10,
  height = 6,
  dpi = 300
)

# Plot 3: Coefficient space (TENSE)
tense_coef_plot <- ggplot(clustering_result$data, aes(x = tense_vot, y = tense_f0, color = factor(cluster))) +
  geom_point(size = 3, alpha = 0.7) +
  labs(
    title = sprintf("Tense Contrast: F0 vs VOT Coefficients (%d Clusters)", k),
    subtitle = "Clusters constrained by non-overlapping age ranges",
    x = "VOT Coefficient",
    y = "F0 Coefficient",
    color = "Cluster"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    legend.position = "right"
  )

print(tense_coef_plot)
ggsave(
  sprintf("../graphs/age_constrained_clustering_k%d_tense_coefficients.png", k),
  plot = tense_coef_plot,
  width = 10,
  height = 6,
  dpi = 300
)

# Plot 4: Age vs F0 reliance by cluster
clustering_result$data <- clustering_result$data %>%
  mutate(
    asp_f0_reliance = abs(asp_f0) / (abs(asp_f0) + abs(asp_vot)),
    tense_f0_reliance = abs(tense_f0) / (abs(tense_f0) + abs(tense_vot))
  )

f0_reliance_plot <- ggplot(clustering_result$data, aes(x = age, y = asp_f0_reliance, color = factor(cluster))) +
  geom_point(size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, aes(fill = factor(cluster)), alpha = 0.2) +
  labs(
    title = sprintf("Age vs F0 Reliance by Cluster (%d Clusters)", k),
    subtitle = "F0 Reliance = |F0 coef| / (|F0 coef| + |VOT coef|)",
    x = "Age",
    y = "F0 Reliance (Aspirated Contrast)",
    color = "Cluster",
    fill = "Cluster"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    legend.position = "right"
  ) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2))

print(f0_reliance_plot)
ggsave(
  sprintf("../graphs/age_constrained_clustering_k%d_f0_reliance.png", k),
  plot = f0_reliance_plot,
  width = 10,
  height = 6,
  dpi = 300
)

cat("\n========================================\n")
cat("Age-constrained clustering complete!\n")
cat("Results saved to ../graphs/\n")
cat("========================================\n")
