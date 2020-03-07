


###drive graph
game_df$plot_yd100 <- ifelse(game_df$posteam==game_df$home_team,100-game_df$yardline_100,game_df$yardline_100)


ggplot(data = game_df[which(game_df$play_type!='kickoff' & game_df$play_type!='extra_point'),], aes(x = game_seconds_remaining, y = plot_yd100, group = drive_id)) +
  geom_line(aes(color = posteam), size = 1) +
  scale_color_manual(values=tm_col,name = 'Team') +
  scale_x_reverse(limits = c(3600,0), breaks = seq(3150,0,-900), labels = paste0('Q',1:4), expand = c(0,10)) +
  labs(title='Game Drives',
       #subtitle = 'Excluding Special Teams and PATs',
       x = 'Time Remaining',
       y = 'Yard Line') +
  annotate("rect", xmin = 2700, xmax = 1800, ymin = -Inf, ymax = Inf, alpha = .05) +
  annotate("rect", xmin = 900, xmax = 0, ymin = -Inf, ymax = Inf, alpha = .05) +
  theme_bw() +
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
  ) + 
  theme(
    axis.title.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.x = element_text(size = 10, color = 'darkblue'),
    legend.position = 'bottom',
    legend.title = element_blank(),
    legend.background = element_rect(fill = 'white',color = 'darkblue'),
    legend.key = element_blank()
  )





