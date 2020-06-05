library(tidyverse)

setwd('C:/Users/rei1740/Desktop/Anthony/nfl')
source('https://github.com/ajreinhard/data-viz/raw/master/ggplot/plot_SB.R')

pbp_df <- readRDS(url('https://raw.githubusercontent.com/ajreinhard/NFL/master/full-pbp/2019.rds'))
roster_df <- readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/roster-data/roster.rds'))

drop_df <- pbp_df %>%
  filter(incompletion_type == 'Dropped Pass') %>%
  group_by(season, receiver_id) %>%
  summarize(
    'drops' = sum(pass_attempt),
    'drop_epa' = sum(epa, na.rm = T) - sum(air_epa, na.rm = T)
  ) %>%
  left_join(roster_df, by = c('receiver_id'='teamPlayers.gsisId', 'season'='team.season')) %>%
  arrange(drop_epa) %>%
  top_n(25, -drop_epa) %>%
  mutate(
    receiver_id = factor(receiver_id,rev(receiver_id)),
    player_line = paste0(row_number(),') ',teamPlayers.displayName,', ', gsub('O','',teamPlayers.position),' (',drops,')')
  )

spec_df <- pbp_df %>%  filter(incompletion_type == 'Dropped Pass' & receiver_player_name == 'C.Beasley')

y_min <- min(drop_df$drop_epa)

p <- ggplot(drop_df, aes(x = receiver_id, y = drop_epa, fill = team.abbr, label = player_line)) + 
  geom_bar(aes(color = team.abbr), stat = 'identity', show.legend = F, size = 0.5, width = 0.8) +
  geom_grob(data = drop_df, aes(x = as.numeric(receiver_id) + 0.2, y = drop_epa + y_min/40, label = grob_img_adj(ESPN_logo_url(team.abbr), whitewash = 0.3), vp.height = 0.021)) +
  geom_image(aes(image = teamPlayers.headshot_url), asp = 9/16, size = 0.06) +
  geom_text(color = 'white', size = 2.9, nudge_y = y_min/25, hjust = 1, family = font_SB) +
  coord_flip() +
  scale_y_reverse(limits = properLimsRev, expand = expansion(mult = c(0,0.03))) +
  scale_fill_manual(values = NFL_pri) +
  scale_color_manual(values = NFL_sec) +
  labs(title = 'EPA Impact from Dropped Passes by Receiver, 2019',
       subtitle = 'Cumulative EPA minus Cumulative Air EPA on Dropped Passes',
       x = 'Player',
       y = 'Cumulative EPA Lost') +
  theme_SB +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.major.y = element_blank()
  )


brand_plot(p, asp = 9/16, save_name = 'rec drops 2019.png', data_home = 'Data: @nflfastR & Sportradar', fade_borders = 'tr', axis_rot = T)

