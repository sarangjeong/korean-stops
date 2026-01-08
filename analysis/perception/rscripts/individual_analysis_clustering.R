# Clustering Analysis
# Created for clustering subjects based on F0 and VOT coefficients
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
# Perform K-means Clustering
##################################################

# Prepare features for clustering (F0 and VOT coefficients)
features <- clustering_data %>%
  select(asp_f0, asp_vot, tense_f0, tense_vot) %>%
  scale()  # Standardize features

# Try different numbers of clusters (2 to 5)
set.seed(42)  # For reproducibility

results_list <- list()

for (k in 2:10) {
  cat("\n========================================\n")
  cat("Clustering with", k, "clusters\n")
  cat("========================================\n")
  
  # Perform k-means clustering
  kmeans_result <- kmeans(features, centers = k, nstart = 100, iter.max = 1000)
  
  # Add cluster assignments to data
  clustering_data_k <- clustering_data
  clustering_data_k$cluster <- kmeans_result$cluster
  
  # Print cluster statistics
  cluster_stats <- clustering_data_k %>%
    group_by(cluster) %>%
    summarise(
      n = n(),
      mean_age = mean(age),
      sd_age = sd(age),
      min_age = min(age),
      max_age = max(age),
      mean_asp_f0 = mean(asp_f0),
      mean_asp_vot = mean(asp_vot),
      mean_tense_f0 = mean(tense_f0),
      mean_tense_vot = mean(tense_vot),
      .groups = 'drop'
    ) %>%
    arrange(mean_age)
  
  cat("\nCluster statistics:\n")
  print(cluster_stats)
  
  cat("\nWithin-cluster sum of squares:", kmeans_result$tot.withinss, "\n")
  cat("Between-cluster sum of squares:", kmeans_result$betweenss, "\n")
  cat("Total sum of squares:", kmeans_result$totss, "\n")
  cat("Ratio (between/total):", kmeans_result$betweenss / kmeans_result$totss, "\n")
  
  # Save results
  results_list[[paste0("k", k)]] <- list(
    result = kmeans_result,
    data = clustering_data_k,
    stats = cluster_stats
  )
  
  # Save clustered data
  output_path <- sprintf("../graphs/clusters_k%d.csv", k)
  write.csv(clustering_data_k, output_path, row.names = FALSE)
  cat("Saved to:", output_path, "\n")
}

##################################################
# Determine Optimal Number of Clusters
##################################################

cat("\n========================================\n")
cat("Determining optimal number of clusters\n")
cat("========================================\n")

# Elbow method
wss <- sapply(2:10, function(k) {
  kmeans(features, centers = k, nstart = 100)$tot.withinss
})

elbow_data <- data.frame(k = 2:10, wss = wss)

elbow_plot <- ggplot(elbow_data, aes(x = k, y = wss)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  labs(
    title = "Elbow Method for Optimal k",
    x = "Number of Clusters (k)",
    y = "Within-Cluster Sum of Squares"
  ) +
  theme_minimal() +
  scale_x_continuous(breaks = 2:10)

print(elbow_plot)
ggsave(
  "../graphs/clustering_elbow_method.png",
  plot = elbow_plot,
  width = 8,
  height = 6,
  dpi = 300
)

# Silhouette method
silhouette_scores <- sapply(2:10, function(k) {
  km <- kmeans(features, centers = k, nstart = 100)
  ss <- silhouette(km$cluster, dist(features))
  mean(ss[, 3])
})

silhouette_data <- data.frame(k = 2:10, avg_silhouette = silhouette_scores)

silhouette_plot <- ggplot(silhouette_data, aes(x = k, y = avg_silhouette)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  labs(
    title = "Silhouette Method for Optimal k",
    x = "Number of Clusters (k)",
    y = "Average Silhouette Width"
  ) +
  theme_minimal() +
  scale_x_continuous(breaks = 2:10)

print(silhouette_plot)
ggsave(
  "../graphs/clustering_silhouette_method.png",
  plot = silhouette_plot,
  width = 8,
  height = 6,
  dpi = 300
)

cat("\nOptimal k by silhouette method:", which.max(silhouette_scores) + 1, "\n")

##################################################
# Visualization
##################################################

# Visualize clustering results for k=10
k <- 10

# Get the data directly from the results list
clustering_data_viz <- results_list[[paste0("k", k)]]$data

# Reorder clusters by mean age for consistency
cluster_age_order <- clustering_data_viz %>%
  group_by(cluster) %>%
  summarise(mean_age = mean(age), .groups = 'drop') %>%
  arrange(mean_age) %>%
  mutate(new_cluster = row_number())

clustering_data_viz <- clustering_data_viz %>%
  left_join(cluster_age_order %>% select(cluster, new_cluster), by = "cluster") %>%
  mutate(cluster = new_cluster) %>%
  select(-new_cluster)

# Plot 1: Age distribution by cluster
age_plot <- ggplot(clustering_data_viz, aes(x = factor(cluster), y = age, fill = factor(cluster))) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  labs(
    title = sprintf("Age Distribution Across %d Clusters", k),
    x = "Cluster",
    y = "Age"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

print(age_plot)
ggsave(
  sprintf("../graphs/clustering_k%d_age_distribution.png", k),
  plot = age_plot,
  width = 8,
  height = 6,
  dpi = 300
)

# Plot 2: Coefficient space (ASP)
asp_coef_plot <- ggplot(clustering_data_viz, aes(x = asp_vot, y = asp_f0, color = factor(cluster))) +
  geom_point(size = 3, alpha = 0.7) +
  labs(
    title = sprintf("Aspirated Contrast: F0 vs VOT Coefficients (%d Clusters)", k),
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
  sprintf("../graphs/clustering_k%d_asp_coefficients.png", k),
  plot = asp_coef_plot,
  width = 10,
  height = 6,
  dpi = 300
)

# Plot 3: Coefficient space (TENSE)
tense_coef_plot <- ggplot(clustering_data_viz, aes(x = tense_vot, y = tense_f0, color = factor(cluster))) +
  geom_point(size = 3, alpha = 0.7) +
  labs(
    title = sprintf("Tense Contrast: F0 vs VOT Coefficients (%d Clusters)", k),
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
  sprintf("../graphs/clustering_k%d_tense_coefficients.png", k),
  plot = tense_coef_plot,
  width = 10,
  height = 6,
  dpi = 300
)

# Plot 4: Age vs F0 reliance by cluster
clustering_data_viz <- clustering_data_viz %>%
  mutate(
    asp_f0_reliance = abs(asp_f0) / (abs(asp_f0) + abs(asp_vot)),
    tense_f0_reliance = abs(tense_f0) / (abs(tense_f0) + abs(tense_vot))
  )

f0_reliance_plot <- ggplot(clustering_data_viz, aes(x = age, y = asp_f0_reliance, color = factor(cluster))) +
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
  sprintf("../graphs/clustering_k%d_f0_reliance.png", k),
  plot = f0_reliance_plot,
  width = 10,
  height = 6,
  dpi = 300
)

cat("\n========================================\n")
cat("Clustering complete!\n")
cat("Results saved to ../graphs/\n")
cat("========================================\n")