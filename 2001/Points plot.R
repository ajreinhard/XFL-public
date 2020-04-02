library(ggplot2)
library(ggimage)
library(extrafont)
setwd('C:/Users/Owner/Documents/GitHub/XFL/2001')

sched_df <- read.csv('sched.csv', stringsAsFactors=F)
teams_df <- read.csv('teams.csv', stringsAsFactors=F)

points_df <- data.frame(rbind(cbind(sched_df$Away, sched_df$AwayScore, sched_df$HomeScore),cbind(sched_df$Home, sched_df$HomeScore, sched_df$AwayScore)), stringsAsFactors = F)
names(points_df) <- c('Tm','PF','PA')
points_df$PF <- as.numeric(points_df$PF)
points_df$PA <- as.numeric(points_df$PA)

points_agg_df <- aggregate(.~Tm, data = points_df, FUN = mean)
points_agg_df$logo <- paste0('logos/',points_agg_df$Tm,'.png')

	geom_image(aes(image = logo), size = .12) +
	scale_y_reverse(limits = c(28, 12), breaks = seq(28, 12, -4)) +
	scale_x_continuous(limits = c(12, 28), breaks = seq(12, 28, 4)) +
	labs(title='XFL Average Points Scored & Allowed',
	 subtitle = '2001 Season',
       caption = 'By Anthony Reinhard',
       x = 'Average Points For',
       y = 'Average Points Against') +
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

ggsave('Points XFL.png', width = 5, height = 5, dpi = 700)


