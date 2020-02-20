library(ggplot2)
library(ggimage)
setwd('C:/Users/Owner/Documents/GitHub/XFL')

teams_df <- read.csv('teams.csv', stringsAsFactors=F)
teams_df$UseColor <- teams_df$Color2
teams_df$UseColor[4] <- teams_df$Color3[4]
teams_df$UseColor[1] <- teams_df$Color1[1]
teams_df$UseColor[2] <- teams_df$Color1[2]

#all_pbp <- do.call(rbind, lapply(dir('epa/post-epa',full=T), function(i) read.csv(i, stringsAsFactors=F)))
all_pbp <- read.csv('epa/all_pbp.csv', stringsAsFactors=F)
all_rb <- names(which(table(all_pbp$RusherName)>10))
all_qb <- names(which(table(all_pbp$PasserName)>5))
all_rb <- all_rb[which(!all_rb %in% all_qb)]

all_qb_df <- all_pbp[which(all_pbp$RusherName %in% all_rb),]
all_qb_df$epa <- ifelse(all_qb_df$epa < -4.5, -4.5, all_qb_df$epa)

all_qb_plays <- aggregate(cbind('plays'=1,epa) ~ RusherName + ClubCode, data = all_qb_df, FUN = sum)
all_qb_plays$epa_per_play <- all_qb_plays$epa/all_qb_plays$plays

all_qb_plays <- merge(all_qb_plays, teams_df, by.x = 'ClubCode', by.y = 'Abbr', all.y = T)
all_qb_plays <- all_qb_plays[order(-all_qb_plays$epa_per_play),]
all_qb_plays$RusherName <- factor(all_qb_plays$RusherName , levels = all_qb_plays$RusherName )

tm_colors <- all_qb_plays$UseColor
names(tm_colors) <- all_qb_plays$ClubCode

all_qb_plays$logos <- paste0('logos/',all_qb_plays$ClubCode,'.png')
all_qb_plays$logos_y <- ifelse(all_qb_plays$epa_per_play < 0, 0, all_qb_plays$epa_per_play) + .06

ggplot(data = all_qb_plays, aes(x = RusherName, y = epa_per_play, fill = ClubCode)) + 
	geom_bar(stat="identity") + 
	scale_fill_manual(values=tm_colors) +
	geom_image(aes(image = logos, y = logos_y), size = 0.13) +
	labs(title='XFL RB EPA per Play',
       caption = paste0('By Anthony Reinhard\nData from XFL.com | EPA Model from @nflscrapR',
				'\nIncludes Passes, Sacks, and Runs | Excludes receiver fumbles | Plays <= -4.5 EPA are capped'),
       subtitle='Through Week 2 of 2020 using NFL Expected Points model',
       y = 'EPA / Play') +
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
	) + 
	theme(legend.position = "none"
		,axis.title.x=element_blank()
		,axis.text.x=element_text(angle = -25, size = 6)
		,axis.ticks.x=element_blank())


ggsave('XFL RB Week 2.png', width=5, height=5)

#scale_y_continuous(breaks=seq(0,.8,.1),labels=round(seq(-0.3,0.5,0.1),1), limits = c(0,.8)) + 
all_gms <- aggregate(cbind('plays'=1,epa) ~ PasserRusher + ClubCode + GameID, data = all_qb_df, FUN = sum)
all_gms$epa_per_play <- all_gms$epa/all_gms$plays
