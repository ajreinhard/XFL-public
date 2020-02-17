library(ggplot2)
library(ggimage)

setwd('C:/Users/Owner/Documents/GitHub/XFL')

teams_df <- read.csv('teams.csv', stringsAsFactors=F)
PFF_df <- read.csv('epa/PFF grades.csv', stringsAsFactors=F)

pbp_df <- do.call(rbind, lapply(dir('epa/post-epa',full=T), function(i) read.csv(i, stringsAsFactors=F)))

off_epa <- aggregate(cbind(plays=1,epa) ~ ClubCode, data = pbp_df, FUN = sum, subset = PlayType == 'Pass' | PlayType == 'Rush')
off_epa$epa_per_play <- off_epa$epa / off_epa$plays

def_epa <- off_epa
def_epa$ClubCode <- c('STL','SEA','LA','HOU','TB','DC','DAL','NY')

off_epa <- merge(off_epa, teams_df, by.x = 'ClubCode', by.y = 'Abbr', all.x=T)
off_epa <- merge(off_epa, PFF_df[which(PFF_df$Type=='Offense'),], by.x = 'Full', by.y = 'Team', all.x=T)
off_epa$logos <- paste0('logos/',off_epa$ClubCode,'.png')

def_epa <- merge(def_epa, teams_df, by.x = 'ClubCode', by.y = 'Abbr', all.x=T)
def_epa <- merge(def_epa, PFF_df[which(PFF_df$Type=='Defense'),], by.x = 'Full', by.y = 'Team', all.x=T)
def_epa$logos <- paste0('logos/',def_epa$ClubCode,'.png')


ggplot(data = off_epa,aes(y = Grade, x = epa_per_play)) +
	geom_image(aes(image = logos), size = 0.15) +
	labs(y = 'PFF Team Offensive Grade',
	x = 'Offensive EPA / Play',
	caption = 'By Anthony Reinhard\nData from XFL.com and PFF\nEPA model from @nflscrapeR',	title = 'XFL Team Offense',
	subtitle = 'Through Week 1 of 2020') +
	theme_bw() +
	theme(
        #text = element_text(family='Bahnschrift', color='darkblue'),
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


ggsave('offense.png',dpi = 1000)


ggplot(data = def_epa,aes(y = Grade, x = epa_per_play)) +
	geom_image(aes(image = logos), size = 0.15) +
	scale_x_reverse() +
	labs(y = 'PFF Team Defensive Grade',
	x = 'Defensive EPA / Play',
	caption = 'By Anthony Reinhard\nData from XFL.com and PFF\nEPA model from @nflscrapeR',
	title = 'XFL Team Defense',
	subtitle = 'Through Week 1 of 2020') +
	theme_bw() +
	theme(
        #text = element_text(family='Bahnschrift', color='darkblue'),
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


ggsave('defense.png',dpi = 1000)