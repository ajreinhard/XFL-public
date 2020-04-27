library(ggplot2)
library(gridExtra)
library(grid)
library(png)
library(cowplot)
library(colorspace)
library(extrafont)


brand_plot <- function(orig_plot, save_name, asp, base_size = 5, data_home) {
  logo_size <- 0.03375
  if (asp < 1) {
    base_size_rat <- asp
    logo_size <- logo_size / asp
    base_size <- base_size / asp
  } else {
    base_size_rat <- 1
  }
  logo_file <- readPNG('C:/Users/Owner/Documents/data-viz/statbutler.png')

  author_txt <- textGrob('By Anthony Reinhard', x=unit(0.08 * (base_size_rat), 'npc'), gp=gpar(col='darkblue', family='HP Simplified', fontsize=6), hjust=0)
  data_txt <- textGrob(data_home, x=unit(1 - (.01 * (base_size_rat)), 'npc'), gp=gpar(col='grey95', family='HP Simplified', fontsize=6), hjust=1)
  footer_bg <- grid.rect(x = unit(seq(0.5,1.5,length=1000), 'npc'), gp=gpar(col = 'transparent', fill = colorRampPalette(c('grey95', 'darkblue'), space = 'rgb')(1000)))
  footer <- grobTree(footer_bg, author_txt, data_txt)
 
  plt.final <- grid.arrange(orig_plot, footer, heights=unit(c(1, 12), c('null','pt')))
  plt <- ggdraw(plt.final) + draw_image(logo_file, x = 0.01 * (base_size_rat), y = 0, hjust = 0, vjust = 0, height = logo_size, width = logo_size * (asp))
  ggsave(save_name, plt, dpi = 700, height = base_size * (asp), width = base_size)
}



theme_SB <- theme_bw() +
  theme(
    text = element_text(family='HP Simplified', color='darkblue'),
    plot.background = element_rect(fill = 'grey95', color = 'grey95'),
    panel.border = element_rect(color = 'darkblue'),
    axis.ticks = element_line(color = 'darkblue'),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 8, color = 'darkblue'),
    plot.title = element_text(size = 14),
    plot.subtitle = element_text(size = 8),
    plot.caption = element_text(size = 5)
  ) 


properLims <- function(vec) {
  labs <- labeling::extended(min(vec, na.rm = T), max(vec, na.rm = T), m = 5)
  gap <- diff(labs[1:2])
  plot_max <- ifelse(rev(labs)[1] < max(vec, na.rm = T), rev(labs)[1] + gap, rev(labs)[1])
  plot_min <- ifelse(labs[1] > min(vec, na.rm = T), labs[1] - gap, labs[1])
  return(c(plot_min,plot_max))
} 
