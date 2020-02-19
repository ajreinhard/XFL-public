library(ggplot2)
library(ggimage)
library(gganimate)

setwd('C:/Users/Owner/Documents/GitHub/XFL')

teams_df <- read.csv('teams.csv', stringsAsFactors=F)
pbp_df <- read.csv('epa/all_pbp.csv', stringsAsFactors=F)
pbp_df$Week <- floor((pbp_df$GameID+3.5)/4)

all_epa_wk <- lapply(1:max(pbp_df$Week), function(wk) {

off_epa <- aggregate(cbind(plays=1,epa) ~ ClubCode, data = pbp_df, FUN = sum, subset = (PlayType == 'Pass' | PlayType == 'Rush') & Week <= wk)
off_epa$epa_per_play <- off_epa$epa / off_epa$plays

def_epa <- aggregate(cbind(plays=1,epa) ~ DefTeam, data = pbp_df, FUN = sum, subset = (PlayType == 'Pass' | PlayType == 'Rush') & Week <= wk)
def_epa$epa_per_play <- def_epa$epa / def_epa$plays

all_epa <- merge(off_epa, def_epa, by.x = c('ClubCode'), by.y = c('DefTeam'), suffixes = c('_off','_def'))
all_epa$logos <- paste0('logos/',all_epa$ClubCode,'.png')
all_epa$Week <- wk

return(all_epa)
})

all_epa <- do.call(rbind, all_epa_wk)


all_epa_gm <- lapply(1:max(pbp_df$GameID), function(gm) {

off_epa <- aggregate(cbind(plays=1,epa) ~ ClubCode, data = pbp_df, FUN = sum, subset = (PlayType == 'Pass' | PlayType == 'Rush') & GameID <= gm)
off_epa$epa_per_play <- off_epa$epa / off_epa$plays

def_epa <- aggregate(cbind(plays=1,epa) ~ DefTeam, data = pbp_df, FUN = sum, subset = (PlayType == 'Pass' | PlayType == 'Rush') & GameID <= gm)
def_epa$epa_per_play <- def_epa$epa / def_epa$plays

all_epa <- merge(off_epa, def_epa, by.x = c('ClubCode'), by.y = c('DefTeam'), suffixes = c('_off','_def'))
all_epa$logos <- paste0('logos/',all_epa$ClubCode,'.png')
all_epa$GameID <- gm

return(all_epa)
})

all_epa <- do.call(rbind, all_epa_gm)

week0 <- all_epa[which(all_epa$GameID==1),]
week0$logos <- NA
week0$GameID <- 0
week_last <- week0
week_last$GameID <- max(pbp_df$GameID)+1
all_epa <- rbind(week0,all_epa,week_last)

#all_epa$bar_len_xmax <- (max(all_epa$epa_per_play_off)-min(all_epa$epa_per_play_off)) * (all_epa$GameID/max(all_epa$GameID)) + min(all_epa$epa_per_play_off)
all_epa$bar_len_xmax <- (max(all_epa$epa_per_play_off)-min(all_epa$epa_per_play_off)) * (1:nrow(all_epa)/nrow(all_epa)) + min(all_epa$epa_per_play_off)
all_epa$bar_len_xmin <- min(all_epa$epa_per_play_off)
all_epa$bar_len_ymax <- max(all_epa$epa_per_play_def) + .05
all_epa$bar_len_ymin <- max(all_epa$epa_per_play_def) + .03


main_plot <- ggplot(data = all_epa, aes(x = epa_per_play_off, y = epa_per_play_def)) +
	geom_image(aes(image = logos), size = 0.15) +
	geom_rect(aes(xmax = bar_len_xmax, xmin = bar_len_xmin, ymax = bar_len_ymax, ymin = bar_len_ymin)) +
	scale_y_reverse() +
	labs(
	y = 'Defensive EPA / Play',
	x = 'Offensive EPA / Play',
	caption = 'By Anthony Reinhard\nData from XFL.com\nEPA model from @nflscrapeR',
	title = 'XFL Team EPA per Play',
	subtitle = 'Calculated using NFL Expected Points model'
	) +
	theme_bw() +
	theme(
	  text = element_text(color='darkblue'),
        plot.background = element_rect(fill = 'grey95'),
        panel.border = element_rect(color = 'darkblue'),
        axis.ticks = element_line(color = 'darkblue'),
        axis.title = element_text(size = 10),
        axis.text = element_text(size = 8, color = 'darkblue'),
        plot.title = element_text(size = 14),
        plot.subtitle = element_text(size = 8),
        plot.caption = element_text(size = 5)
	)

main_plot + transition_time(GameID) + enter_grow()
anim_save('EPA.gif')


