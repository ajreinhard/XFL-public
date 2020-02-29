library(ggplot2)
library(ggimage)
library(extrafont)

setwd('C:/Users/Owner/Documents/GitHub/XFL')

teams_df <- read.csv('teams.csv', stringsAsFactors=F)
PFF_df <- read.csv('epa/PFF grades.csv', stringsAsFactors=F)
pbp_df <- read.csv('epa/scraped PBP.csv', stringsAsFactors=F)


off_epa <- aggregate(cbind(plays=1,epa) ~ posteam, data = pbp_df, FUN = sum, subset = play_type == 'pass' | play_type == 'run')
off_epa$epa_per_play <- off_epa$epa / off_epa$plays

def_epa <- aggregate(cbind(plays=1,epa) ~ defteam, data = pbp_df, FUN = sum, subset = play_type == 'pass' | play_type == 'run')
def_epa$epa_per_play <- def_epa$epa / def_epa$plays


off_epa <- merge(off_epa, teams_df, by.x = 'posteam', by.y = 'Abbr', all.x=T)
off_epa <- merge(off_epa, PFF_df[which(PFF_df$Type=='Offense'),], by.x = 'Full', by.y = 'Team', all.x=T)
off_epa$logos <- paste0('logos/',off_epa$posteam,'.png')

def_epa <- merge(def_epa, teams_df, by.x = 'defteam', by.y = 'Abbr', all.x=T)
def_epa <- merge(def_epa, PFF_df[which(PFF_df$Type=='Defense'),], by.x = 'Full', by.y = 'Team', all.x=T)
def_epa$logos <- paste0('logos/',def_epa$defteam,'.png')


psblk_df <- PFF_df[which(PFF_df$Week=='Season thru 3' & PFF_df$Type=='Pass Block'),]
rnblk_df <- PFF_df[which(PFF_df$Week=='Season thru 3' & PFF_df$Type=='Run Block'),]
blocking <- merge(psblk_df, rnblk_df, by = 'Name', suffixes = c('_pass','_run'))
blocking  <- merge(blocking, teams_df, by.x = 'Name', by.y = 'Full')
blocking$logos <- paste0('logos/',blocking$Abbr,'.png')


ggplot(data = blocking, aes(y = Grade_pass, x = Grade_run)) +
	geom_image(aes(image = logos), size = 0.15) +
	labs(y = 'PFF Team Pass Blocking Grade',
	x = 'PFF Team Run Blocking Grade',
	caption = 'By Anthony Reinhard  |  Data from XFL.com and PFF',
	title = 'XFL Offensive Line Grades',
	subtitle = 'Through Week 3 of 2020') +
	scale_x_continuous(lim = c(40,90)) +
	scale_y_continuous(lim = c(40,90)) +
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
        plot.caption = element_text(size = 5)
	)

ggsave('blocking.png',dpi = 1000)



off_df <- PFF_df[which(PFF_df$Week=='Season thru 3' & PFF_df$Type=='Offense'),]
def_df <- PFF_df[which(PFF_df$Week=='Season thru 3' & PFF_df$Type=='Defense'),]
szn_grade <- merge(off_df, def_df, by = 'Name', suffixes = c('_off','_def'))
szn_grade <- merge(szn_grade, teams_df, by.x = 'Name', by.y = 'Full')
szn_grade$logos <- paste0('logos/',szn_grade$Abbr,'.png')


ggplot(data = szn_grade, aes(y = Grade_def, x = Grade_off)) +
	geom_image(aes(image = logos), size = 0.15) +
	labs(y = 'PFF Team Defense Grade',
	x = 'PFF Team Offense Grade',
	caption = 'By Anthony Reinhard  |  Data from XFL.com and PFF',
	title = 'XFL Team Grades',
	subtitle = 'Through Week 3 of 2020') +
	scale_x_continuous(lim = c(55,80)) +
	scale_y_continuous(lim = c(55,80)) +
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
        plot.caption = element_text(size = 5)
	)

ggsave('teamPFF.png',dpi = 1000)






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