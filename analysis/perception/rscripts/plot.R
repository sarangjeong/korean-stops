# Analysis of experimental results of
# Korean stop contrast, perception, young (pilot)
# created by Sarang Jeong on June 6, 2021

f0_vot_asp_plot <- function(
    data, title, path    
) {
  asp_plot <- ggplot(data, aes(x = vot, f0)) +
    geom_tile(aes(fill = asp * 100)) +
    geom_text(aes(label = round(asp * 100, 1))) +
    scale_fill_continuous(low = "white", high = "cyan", name = "aspirated %") +
    labs(x = "VOT", y = "F0",
         title =title) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))
  ggsave(
    path,
    plot = asp_plot,
    scale = 1,
    width = 10,
    height = 6,
    dpi = "retina",
  )
  return(asp_plot)
}

f0_vot_tense_plot <- function(
    data, title, path    
) {
  tense_plot <- ggplot(data, aes(vot, f0)) +
    geom_tile(aes(fill = tense * 100)) +
    geom_text(aes(label = round(tense * 100, 1))) +
    scale_fill_continuous(low = "white", high = "yellow", name = "tense %") +
    labs(x = "VOT", y = "F0",
         title =title) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))
  ggsave(
    path,
    plot = tense_plot,
    scale = 1,
    width = 10,
    height = 6,
    dpi = "retina",
  )
  return(tense_plot)
}

f0_vot_lenis_plot <- function(
  data, title, path    
) {
  lenis_plot <- ggplot(data, aes(x = vot, f0)) +
    geom_tile(aes(fill = lenis * 100)) +
    geom_text(aes(label = round(lenis * 100, 1))) +
    scale_fill_continuous(low = "white", high = "magenta", name = "lenis %", 
                          limits = c(0, 100), 
                          breaks = seq(0, 100, by=25)) +
    labs(x = "VOT", y = "F0",
         title =title) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))
  ggsave(
    path,
    plot = lenis_plot,
    scale = 1,
    width = 10,
    height = 6,
    dpi = "retina",
  )
  return(lenis_plot)
}

f0_vot_rainbow_plot <- function(
  data, title, path
) {
  rainbow_plot <- ggplot(data, aes(vot, f0)) +
    geom_tile(aes(fill = I(rgb(1 - asp, 1 - lenis, 1 - tense))))+
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
  
  return(rainbow_plot)
}