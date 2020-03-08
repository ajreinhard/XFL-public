library(ggplot2)
library(ggimage)
library(gganimate)
library(extrafont)

setwd('C:/Users/Owner/Documents/GitHub/XFL')

teams_df <- read.csv('teams.csv', stringsAsFactors=F)
pbp_df <- read.csv('epa/scraped PBP.csv', stringsAsFactors=F)
pbp_df$Week <- floor((pbp_df$Game+3.5)/4)

all_epa_wk <- lapply(1:max(pbp_df$Week), function(wk) {

off_epa <- aggregate(cbind(plays=1,epa) ~ posteam, data = pbp_df, FUN = sum, subset = (play_type == 'pass' | play_type == 'run') & Week <= wk)
off_epa$epa_per_play <- off_epa$epa / off_epa$plays

def_epa <- aggregate(cbind(plays=1,epa) ~ defteam, data = pbp_df, FUN = sum, subset = (play_type == 'pass' | play_type == 'run') & Week <= wk)
def_epa$epa_per_play <- def_epa$epa / def_epa$plays

all_epa <- merge(off_epa, def_epa, by.x = c('posteam'), by.y = c('defteam'), suffixes = c('_off','_def'))
all_epa$logos <- paste0('logos/',all_epa$posteam,'.png')
all_epa$Week <- wk

return(all_epa)
})

all_epa <- do.call(rbind, all_epa_wk)

pbp_df$posteam <- factor(pbp_df$posteam)
pbp_df$defteam <- factor(pbp_df$defteam)

all_epa_gm <- lapply(1:max(pbp_df$Game), function(gm) {

off_epa <- aggregate(cbind(plays=1,epa) ~ posteam, data = pbp_df, FUN = sum, subset = (play_type == 'pass' | play_type == 'run') & Game <= gm, drop = F)
off_epa$epa_per_play <- off_epa$epa / off_epa$plays

def_epa <- aggregate(cbind(plays=1,epa) ~ defteam, data = pbp_df, FUN = sum, subset = (play_type == 'pass' | play_type == 'run') & Game <= gm, drop = F)
def_epa$epa_per_play <- def_epa$epa / def_epa$plays

all_epa <- merge(off_epa, def_epa, by.x = c('posteam'), by.y = c('defteam'), suffixes = c('_off','_def'))
all_epa$logos <- paste0('logos/',all_epa$posteam,'.png')
all_epa$Game <- gm

return(all_epa)
})

all_epa <- do.call(rbind, all_epa_gm)

week0 <- all_epa[which(all_epa$Game==min(all_epa$Game)),]
week0$logos <- NA
week0$Game <- 0
week_last <- week0
week_last$Game <- max(pbp_df$Game)+1
all_epa <- rbind(week0,all_epa,week_last)

all_epa$bar_len_xmax <- (max(all_epa$epa_per_play_off, na.rm = T)-min(all_epa$epa_per_play_off, na.rm = T)) * (all_epa$Game/max(all_epa$Game)) + min(all_epa$epa_per_play_off, na.rm = T)
#all_epa$bar_len_xmax <- (max(all_epa$epa_per_play_off, na.rm = T)-min(all_epa$epa_per_play_off, na.rm = T)) * (1:nrow(all_epa)/nrow(all_epa)) + min(all_epa$epa_per_play_off, na.rm = T)
all_epa$bar_len_xmin <- min(all_epa$epa_per_play_off, na.rm = T)
all_epa$bar_len_ymax <- max(all_epa$epa_per_play_def, na.rm = T) + .05
all_epa$bar_len_ymin <- max(all_epa$epa_per_play_def, na.rm = T) + .03

all_epa$logos <- ifelse(is.na(all_epa$epa_per_play_off),NA,all_epa$logos)

main_plot <- ggplot(data = all_epa, aes(x = epa_per_play_off, y = epa_per_play_def)) +
	geom_image(aes(image = logos), size = 0.17) +
	geom_rect(aes(xmax = bar_len_xmax, xmin = bar_len_xmin, ymax = bar_len_ymax, ymin = bar_len_ymin)) +
	scale_y_reverse() +
	labs(
	y = 'Defensive EPA / Play',
	x = 'Offensive EPA / Play',
	caption = 'By Anthony Reinhard  |  Data from @xflscrapR  |  EPA model from @nflscrapeR',
	title = 'XFL Team EPA per Play',
	subtitle = 'Calculated using NFL Expected Points model'
	) +
	theme_bw() +
	theme(
	  text = element_text(family='HP Simplified', color='darkblue'),
	  plot.background = element_rect(fill = 'grey95'),
        panel.border = element_rect(color = 'darkblue'),
        axis.ticks = element_line(color = 'darkblue'),
        axis.title = element_text(size = 13),
        axis.text = element_text(size = 11, color = 'darkblue'),
        plot.title = element_text(size = 19),
        plot.subtitle = element_text(size = 12),
        plot.caption = element_text(size = 10)
	)


main_plot + transition_time(Game) + enter_grow()
anim_save('EPA_3.gif', dpi=1000)
?anim_save