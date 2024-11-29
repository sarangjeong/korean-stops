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
source("preprocessing.R")

# Preprocess the data
data <- basic_data_preprocessing(
  workerids_csv_path = "../data/korean_stops_perception_3_poa_all_ages-workerids.csv",
  trials_csv_path = "../data/korean_stops_perception_3_poa_all_ages-trials.csv",
  subject_information_csv_path = "../data/korean_stops_perception_3_poa_all_ages-subject_information.csv"
)

# Step 2: Reshape Data
data_long <- data %>%
  pivot_longer(cols = starts_with("interaction"), 
               names_to = "interaction_age_group", 
               values_to = "interaction_percentage") %>%
  mutate(interaction_age_group = as.numeric(gsub("interaction_", "", interaction_age_group)))

# Step 3: Treat `age` and `interaction_age_group` as categorical variables
summary <- data_long %>%
  group_by(age, interaction_age_group) %>%
  summarise(mean_percentage = mean(interaction_percentage, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    age = factor(age),
    interaction_age_group = factor(interaction_age_group),
    text_color = ifelse(mean_percentage > 50, "white", "black") # Dynamic font color
  )

# Step 4: Plot Heatmap with Aggregated Data
plot <- ggplot(summary, aes(x = interaction_age_group, y = age, fill = mean_percentage)) +
  geom_tile(color = "black") + # Add black edges to each tile
  geom_text(aes(label = round(mean_percentage, 1), color = text_color), size = 3) +
  scale_fill_gradient(low = "white", high = "blue", name = "Mean %") +
  scale_color_identity() + # Use the calculated text colors
  labs(
    title = "Mean Interaction Percentage Between Participant Ages and Interaction Age Groups",
    x = "Interaction Age Group",
    y = "Participant Age"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_text(size = 8)
  )

# Step 5: save the plot
ggsave(
  "../graphs/age_interation.png",
  plot = plot,
  width = 10,
  height = 6,
  scale = 1,
  dpi = "retina",
)

print(plot)
