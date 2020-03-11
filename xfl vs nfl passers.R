library(mgcv)
library(ggplot2)
library(extrafont)
library(scales)
#loadfonts(device = 'win')


pbp <- read.csv('https://github.com/ajreinhard/xfl-public/raw/master/epa/scraped PBP.csv', stringsAsFactors=F)

##run xflscrapR first
xfl_air_yards_bam <- bam(complete_pass ~ s(air_yards), data = pbp, method = "fREML", discrete = TRUE, family = binomial(link='logit'))

yr_pbp <- lapply(2016:2019, function(yr) {
  read.csv(paste0('https://github.com/ryurko/nflscrapR-data/raw/master/play_by_play_data/regular_season/reg_pbp_',yr,'.csv'), stringsAsFactors=F)
})
pbp_NFL <- do.call(rbind, yr_pbp)
table(pbp_NFL$play_type)

#pbp_brady <- pbp_NFL[which(substring(pbp_NFL$game_id,1,4)=='2019' & pbp_NFL$passer_player_name == 'T.Brady'),]

#pbp_NFL <- read.csv('C:/Users/rei1740/Desktop/Anthony/XFL/adv2019pbp.csv', stringsAsFactors = F)
nfl_air_yards_bam <- bam(complete_pass ~ s(air_yards), data = pbp_NFL, method = "fREML", discrete = TRUE, family = binomial(link='logit'))
#nfl_air_yards_bam <- bam(complete_pass ~ s(air_yards), data = pbp_brady, method = "fREML", discrete = TRUE, family = binomial(link='logit'))



fit_predict <- expand.grid('air_yards'=-5:40)

fit_predict$pred_XFL <- predict(xfl_air_yards_bam, fit_predict)
fit_predict$pred_XFLerror <- predict(xfl_air_yards_bam, fit_predict, se = TRUE)$se.fit

fit_predict$true_pred_XFL <- exp(fit_predict$pred_XFL)/(1+exp(fit_predict$pred_XFL))
fit_predict$pred_XFLlow = exp(fit_predict$pred_XFL  - 1.96*fit_predict$pred_XFLerror)/(1+exp(fit_predict$pred_XFL  - 1.96*fit_predict$pred_XFLerror))
fit_predict$pred_XFLhigh = exp(fit_predict$pred_XFL  + 1.96*fit_predict$pred_XFLerror)/(1+exp(fit_predict$pred_XFL  + 1.96*fit_predict$pred_XFLerror))


fit_predict$pred_NFL <- predict(nfl_air_yards_bam, fit_predict)
fit_predict$pred_NFLerror <- predict(nfl_air_yards_bam, fit_predict, se = TRUE)$se.fit

fit_predict$true_pred_NFL <- exp(fit_predict$pred_NFL)/(1+exp(fit_predict$pred_NFL))
fit_predict$pred_NFLlow = exp(fit_predict$pred_NFL  - 1.96*fit_predict$pred_NFLerror)/(1+exp(fit_predict$pred_NFL  - 1.96*fit_predict$pred_NFLerror))
fit_predict$pred_NFLhigh = exp(fit_predict$pred_NFL  + 1.96*fit_predict$pred_NFLerror)/(1+exp(fit_predict$pred_NFL  + 1.96*fit_predict$pred_NFLerror))


league_colors <- c('XFL'="red",'NFL'="blue")

ggplot(data = fit_predict, aes(x = air_yards)) +
  geom_line(aes(y = true_pred_NFL, color = 'NFL'), show.legend = T) +
  geom_ribbon(aes(ymin = pred_NFLlow, ymax = pred_NFLhigh, fill = 'NFL'), alpha = 0.25) +
  geom_line(aes(y = true_pred_XFL, color = 'XFL')) +
  geom_ribbon(aes(ymin = pred_XFLlow, ymax = pred_XFLhigh, fill = 'XFL'), alpha = 0.25) +
  labs(title = 'Completion Percentage by Depth of Target', 
       subtitle = 'NFL Passers from 2016 to 2019 vs XFL Passers through Week 4 of 2020', 
       x = 'Air Yards', 
       y = 'Completion Percentage',
       caption  = 'By Anthony Reinhard  |  Data from @nflscrapR and @xflscrapR') +
  scale_y_continuous(labels = percent, limits = c(0,1)) +
  scale_color_manual('League',values = league_colors) +
  scale_fill_manual('League',values = league_colors) +
  theme_bw() +
  theme(
    text = element_text(family='Bahnschrift', color='darkblue'),
    plot.background = element_rect(fill = 'grey95'),
    panel.border = element_rect(color = 'darkblue'),
    axis.ticks = element_line(color = 'darkblue'),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 8, color = 'darkblue'),
    plot.title = element_text(size = 14),
    plot.subtitle = element_text(size = 8),
    plot.caption = element_text(size = 5),
    legend.background = element_rect(fill = 'grey90',color = 'darkblue'),
    legend.key = element_blank()
  )

ggsave('XFL v NFL Passers.png', width=5, height=5, dpi = 1000)
