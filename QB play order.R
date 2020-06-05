library(tidyverse)
library(zoo)
setwd('C:/Users/rei1740/Desktop/Anthony/nfl')
source('https://github.com/ajreinhard/data-viz/raw/master/ggplot/plot_SB.R')

pbp_df <- do.call(rbind, lapply(2016:2019, function(yr) readRDS(url(paste0('https://raw.githubusercontent.com/ajreinhard/NFL/master/full-pbp/',yr,'.rds')))))
roster_df <- readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/roster-data/roster.rds'))

qb_play_count_df <- pbp_df %>%
  filter(play_type != 'no_play') %>%
  mutate(qb_id = ifelse(is.na(passer_id), rusher_id, passer_id)) %>%
  left_join(roster_df, by = c('qb_id'='teamPlayers.gsisId', 'season'='team.season')) %>%
  filter(teamPlayers.position == 'QB' & teamPlayers.displayName == 'Baker Mayfield') %>%
  group_by(qb_id) %>%
  mutate(
    play_num = row_number(),
    rolling_qb_epa = rollapply(qb_epa,300,mean,align='right',fill=NA, na.rm = T),
    rolling_cpoe = rollapply(cpoe/100,300,mean,align='right',fill=NA, na.rm = T),
    end_game = ifelse(game_id != lead(game_id), 1, 0),
    game_num = cumsum(end_game),
    game_txt = paste0('Wk ', week, ', ', season)
  ) %>%
  filter(end_game == 1)

#mean(qb_play_count_df$qb_epa[which(qb_play_count_df$play_num >= 900 & qb_play_count_df$play_num <= 1000)],na.rm = T)
#mean(qb_play_count_df$qb_epa[which(qb_play_count_df$play_num >= 800 & qb_play_count_df$play_num <= 900)],na.rm = T)


p <- ggplot(qb_play_count_df, aes(y = rolling_qb_epa, x = rolling_cpoe, group = qb_id)) + 
  geom_path(linejoin="round", na.rm = T) +
  geom_text_repel(aes(label = game_txt), color = 'darkblue', size = 2, bg.color = 'white', family = font_SB, point.padding = 0, box.padding = 0.01, bg.r = 0.15, min.segment.length = 100) +
  scale_y_continuous(limits = properLims) +
  scale_x_continuous(limits = properLims, labels = percent) +
  labs(title = 'How Has Baker Mayfield Developed?',
       subtitle = 'Trailing 300 Play Average',
       y = 'QB\nEPA',
       x = 'CPOE') +
  theme_SB

brand_plot(p, save_name = 'QB Play Path.png', data_home = 'Data: @nflfastR', fade_borders = 'tr')

?geom_grob_npc()




library(gganimate)
#> Loading required package: ggplot2

# We'll start with a static plot
p <- ggplot(iris, aes(x = Petal.Width, y = Petal.Length)) +
  geom_point()

footer_bg <- grid.rect(x = unit(seq(0.5,1.5,length=1000), 'npc'), gp=gpar(col = 'transparent', fill = colorRampPalette(c('grey95', 'darkblue'), space = 'rgb')(1000)), draw = F)
footer <- grobTree(footer_bg)
plt.final <- grid.arrange(p, footer, heights=unit(c(1, 12), c('null','pt')))
p <- as.ggplot(plt.final)

anim <- p +
  transition_states(Species,
                    transition_length = 2,
                    state_length = 1)


Scene2 <- ggproto(
  "Scene2",
  gganimate:::Scene,
  plot_frame = function(self, plot, i, newpage = is.null(vp), vp = NULL, widths = NULL, heights = NULL, ...) {
    plot <- self$get_frame(plot, i)
    plot <- ggplot_gtable(plot)
    
    # insert changes here
    logo_file <- readPNG('C:/Users/rei1740/Desktop/Anthony/statbutler.png')
    
    author_txt <- textGrob('By Anthony Reinhard', x=unit(0.08, 'npc'), gp=gpar(col='darkblue', fontfamily='Bahnschrift', fontsize=6), hjust=0)
    data_txt <- textGrob(data_home, x=unit(1 - (.01), 'npc'), gp=gpar(col='grey95', fontfamily='Bahnschrift', fontsize=6), hjust=1)
    footer_bg <- grid.rect(x = unit(seq(0.5,1.5,length=1000), 'npc'), gp=gpar(col = 'transparent', fill = colorRampPalette(c('grey95', 'darkblue'), space = 'rgb')(1000)), draw = F)
    footer <- grobTree(footer_bg, author_txt, data_txt)
    
    plt.final <- grid.arrange(plot, footer, heights=unit(c(1, 12), c('null','pt')))
    plot <- ggdraw(plt.final) + draw_image(logo_file, x = 0.002, y = 0, hjust = 0, vjust = 0, height = 0.06, width = 0.08)
    
    
    if (!is.null(widths)) plot$widths <- widths
    if (!is.null(heights)) plot$heights <- heights
    if (newpage) grid.newpage()
    grDevices::recordGraphics(
      requireNamespace("gganimate", quietly = TRUE),
      list(),
      getNamespace("gganimate")
    )
    if (is.null(vp)) {
      grid.draw(plot)
    } else {
      if (is.character(vp)) seekViewport(vp)
      else pushViewport(vp)
      grid.draw(plot)
      upViewport()
    }
    invisible(NULL)
  }
)

library(magrittr)
create_scene2 <- function(transition, view, shadow, ease, transmuters, nframes) {
  if (is.null(nframes)) nframes <- 100
  ggproto(NULL, Scene2, transition = transition, 
          view = view, shadow = shadow, ease = ease, 
          transmuters = transmuters, nframes = nframes)
}

ggplot_build2 <- gganimate:::ggplot_build.gganim
body(ggplot_build2) <- body(ggplot_build2) %>%
  as.list() %>%
  inset2(4,
         quote(scene <- create_scene2(plot$transition, plot$view, plot$shadow, 
                                      plot$ease, plot$transmuters, plot$nframes))) %>%
  as.call()

prerender2 <- gganimate:::prerender
body(prerender2) <- body(prerender2) %>%
  as.list() %>%
  inset2(3,
         quote(ggplot_build2(plot))) %>%
  as.call()

animate2 <- gganimate:::animate.gganim
body(animate2) <- body(animate2) %>%
  as.list() %>%
  inset2(7,
         quote(plot <- prerender2(plot, nframes_total))) %>%
  as.call()


trace(gganimate:::create_scene, edit=TRUE)


animate2(anim, nframes_total = 30)

#https://stackoverflow.com/questions/55362961/using-a-gtable-object-with-gganimate
?annotate

anim_save('test logo.gif', animation = animate(anim, data_home = 'Data: All Mine Baby'))





