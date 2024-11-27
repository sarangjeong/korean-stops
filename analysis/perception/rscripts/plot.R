# Analysis of experimental results of
# Korean stop contrast, perception, young (pilot)
# created by Sarang Jeong on June 6, 2021

f0_vot_rainbow_plot <- function(
  data, title, path
) {
  rainbow_plot <- ggplot(data, aes(vot, f0)) +
    geom_tile(aes(fill = I(rgb(1 - data$asp, 1 - data$lenis, 1 - data$tense))))+
    #               color = c("cyan", "yellow", "magenta"))) +
    # scale_color_manual(values = c("cyan", "yellow", "magenta")) +
    geom_text(aes(label =  paste(label, as.character(round(predominant_num * 100, 1))))) +
    labs(x = "VOT", y = "F0",
         title = title,
         caption = "A = aspirated, L = lenis, T = tense") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5),
          plot.caption = element_text(size = 12))
  saved_rainbow_plot = ggsave(
    path,
    plot = rainbow_plot,
    width = 10,
    height = 6,
    scale = 1,
    dpi = "retina",
  )
  
  return(saved_rainbow_plot)
}