setwd('C:/Users/Owner/Documents/GitHub/XFL')
library(mgcv)
library(ggplot2)
library(extrafont)
library(scales)
library(png)

### read in XFL pbp and make model
pbp <- read.csv('https://github.com/ajreinhard/xfl-public/raw/master/epa/scraped PBP.csv', stringsAsFactors=F)
xfl_air_yards_bam <- bam(complete_pass ~ s(air_yards), data = pbp, method = "fREML", discrete = TRUE, family = binomial(link='logit'))

### read in AAF pbp and make model
plays_df <- read.csv('C:/Users/Owner/Documents/AWS/AAF/plays.csv', stringsAsFactors = F)
stats_df <- read.csv('C:/Users/Owner/Documents/AWS/AAF/stats.csv', stringsAsFactors = F)

air_df <- stats_df[which(stats_df$Season=='REGULAR' & stats_df$Nullified==F & grepl('AIR_YARDS',stats_df$Type)),]
plays_df <- merge(plays_df, air_df[,c('PlayID','PlayerID','Type','Value')], by = 'PlayID', all.x = T, suffixes = c('','_Air'))
plays_df$complete_pass <- ifelse(plays_df$Type_Air=='AIR_YARDS_COMPLETE',1, ifelse(plays_df$Type_Air=='AIR_YARDS_INCOMPLETE',0,NA))
names(plays_df)[which(names(plays_df)=='Value_Air')] <- 'air_yards'

aaf_air_yards_bam <- bam(complete_pass ~ s(air_yards), data = plays_df, method = "fREML", discrete = TRUE, family = binomial(link='logit'))

### scrape NFL pbp and make model
yr_pbp <- lapply(2016:2019, function(yr) {
  read.csv(paste0('https://github.com/ryurko/nflscrapR-data/raw/master/play_by_play_data/regular_season/reg_pbp_',yr,'.csv'), stringsAsFactors=F)
})
pbp_NFL <- do.call(rbind, yr_pbp)

nfl_air_yards_bam <- bam(complete_pass ~ s(air_yards), data = pbp_NFL, method = "fREML", discrete = TRUE, family = binomial(link='logit'))

fit_predict <- expand.grid('air_yards'=-5:40)

### XFL pred 
fit_predict$pred_XFL <- predict(xfl_air_yards_bam, fit_predict)
fit_predict$pred_XFLerror <- predict(xfl_air_yards_bam, fit_predict, se = TRUE)$se.fit

fit_predict$true_pred_XFL <- exp(fit_predict$pred_XFL)/(1+exp(fit_predict$pred_XFL))
fit_predict$pred_XFLlow = exp(fit_predict$pred_XFL  - 1.96*fit_predict$pred_XFLerror)/(1+exp(fit_predict$pred_XFL  - 1.96*fit_predict$pred_XFLerror))
fit_predict$pred_XFLhigh = exp(fit_predict$pred_XFL  + 1.96*fit_predict$pred_XFLerror)/(1+exp(fit_predict$pred_XFL  + 1.96*fit_predict$pred_XFLerror))

### AAF pred
fit_predict$pred_aaf <- predict(aaf_air_yards_bam, fit_predict)
fit_predict$pred_aaferror <- predict(aaf_air_yards_bam, fit_predict, se = TRUE)$se.fit

fit_predict$true_pred_aaf <- exp(fit_predict$pred_aaf)/(1+exp(fit_predict$pred_aaf))
fit_predict$pred_aaflow = exp(fit_predict$pred_aaf  - 1.96*fit_predict$pred_aaferror)/(1+exp(fit_predict$pred_aaf  - 1.96*fit_predict$pred_aaferror))
fit_predict$pred_aafhigh = exp(fit_predict$pred_aaf  + 1.96*fit_predict$pred_aaferror)/(1+exp(fit_predict$pred_aaf  + 1.96*fit_predict$pred_aaferror))

### NFL pred
fit_predict$pred_NFL <- predict(nfl_air_yards_bam, fit_predict)
fit_predict$pred_NFLerror <- predict(nfl_air_yards_bam, fit_predict, se = TRUE)$se.fit

fit_predict$true_pred_NFL <- exp(fit_predict$pred_NFL)/(1+exp(fit_predict$pred_NFL))
fit_predict$pred_NFLlow = exp(fit_predict$pred_NFL  - 1.96*fit_predict$pred_NFLerror)/(1+exp(fit_predict$pred_NFL  - 1.96*fit_predict$pred_NFLerror))
fit_predict$pred_NFLhigh = exp(fit_predict$pred_NFL  + 1.96*fit_predict$pred_NFLerror)/(1+exp(fit_predict$pred_NFL  + 1.96*fit_predict$pred_NFLerror))

league_colors <- c('XFL'='#ff9a00','NFL'='#ff165d','AAF'='#3ec1d3')

ggplot(data = fit_predict, aes(x = air_yards)) +
  geom_ribbon(aes(ymin = pred_XFLlow, ymax = pred_XFLhigh, fill = 'XFL'), alpha = 0.2) +
  geom_ribbon(aes(ymin = pred_aaflow, ymax = pred_aafhigh, fill = 'AAF'), alpha = 0.2) +
  geom_ribbon(aes(ymin = pred_NFLlow, ymax = pred_NFLhigh, fill = 'NFL'), alpha = 0.2) +
  geom_line(aes(y = true_pred_XFL, color = 'XFL')) +
  geom_line(aes(y = true_pred_aaf, color = 'AAF')) +
  geom_line(aes(y = true_pred_NFL, color = 'NFL')) +
  labs(title = 'Completion Percentage by Depth of Target', 
       subtitle = 'NFL Passers from 2016 to 2019 vs XFL & AAF Passers', 
       x = 'Air Yards', 
       y = 'Completion Percentage',
       caption  = 'By Anthony Reinhard  |  Data from @nflscrapR, @xflscrapR via @CFB_Moose, and Alliance GraphQL API') +
  scale_y_continuous(labels = percent, limits = c(0,1)) +
  scale_color_manual('League',values = league_colors) +
  scale_fill_manual('League',values = league_colors) +
  theme_bw() +
  theme(
    text = element_text(family='HP Simplified', color='darkblue'),
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

ggsave('NFL vs AAF-XFL Passers.png', width=5, height=5, dpi = 700)



