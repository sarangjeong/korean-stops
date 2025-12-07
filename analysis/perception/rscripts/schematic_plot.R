# Schematic plot for VOT and F0 grid
# created on November 28, 2025

# Load libraries
library(tidyverse)

# Set working directory
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

# Create grid data
vot_steps <- seq(10, 115, by = 15)  # 8 steps: 10, 25, 40, 55, 70, 85, 100, 115
f0_steps <- seq(135, 205, by = 10)  # 8 steps: 135, 145, 155, 165, 175, 185, 195, 205

grid_data <- expand.grid(
  vot = vot_steps,
  f0 = f0_steps
)

# Create the schematic plot
schematic_plot <- ggplot(grid_data, aes(x = vot, y = f0)) +
  geom_point(size = 4, color = "black", shape = 19) +
  scale_x_continuous(
    breaks = vot_steps,
    limits = c(0, 125)
  ) +
  scale_y_continuous(
    breaks = f0_steps,
    limits = c(125, 215)
  ) +
  coord_fixed(ratio = (125 - 0) / (215 - 125)) +
  theme_classic() +
  labs(
    x = "VOT (ms)",
    y = "f0 (Hz)",
    title = "Schematic Grid: VOT × f0"
  ) +
  theme(
    text = element_text(size = 22),
    axis.title = element_text(size = 24),
    axis.text = element_text(size = 20),
    plot.title = element_text(size = 26, face = "bold"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black")
  )

print(schematic_plot)

# Save the plot
ggsave(
  "../graphs/schematic_plot.png",
  plot = schematic_plot,
  scale = 1,
  width = 8,
  height = 6,
  dpi = "retina"
)

browseURL("../graphs/schematic_plot.png")

