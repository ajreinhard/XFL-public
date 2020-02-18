library(ggplot2)
library(ggimage)
library(scales)
setwd('C:/Users/Owner/Documents/GitHub/XFL')

teams_df <- read.csv('teams.csv', stringsAsFactors=F)
teams_df$UseColor <- teams_df$Color2
teams_df$UseColor[4] <- teams_df$Color3[4]
teams_df$UseColor[1] <- teams_df$Color3[1]
teams_df$UseColor[2] <- teams_df$Color1[2]
teams_df$UseColor[8] <- teams_df$Color4[8]
teams_df$UseColor[7] <- teams_df$Color1[7]
teams_df$UseColor[3] <- teams_df$Color4[3]

sim_res <- read.csv('sim_res.csv', stringsAsFactors=F)

po_res <- data.frame(t(apply(sim_res[,c('WEST_RU','EAST_RU','XFL_RU','XFL_CHAMP')], 1, function(x) ifelse(is.na(match(teams_df$Abbr,x)),0,1))),stringsAsFactors=F)
names(po_res) <- teams_df$Abbr
po_res <- cbind(sim_res[,paste0('WeeklyWinner',1:4)],po_res)



team_games <- lapply(1:4, function(x) {
	playing_tms <- unique(po_res[,paste0('WeeklyWinner',x)])
	return(t(sapply(1:2, function(j) {
		with_win <- mean(po_res[which(po_res[,paste0('WeeklyWinner',x)]==playing_tms[j]),paste0(playing_tms[j])])
		with_loss <- mean(po_res[which(po_res[,paste0('WeeklyWinner',x)]!=playing_tms[j]),paste0(playing_tms[j])])
	 	current_po <- mean(po_res[,paste0(playing_tms[j])])
		return(rbind(playing_tms[j],current_po,with_win,with_loss))
	})))
})

team_lev_df <- data.frame(do.call(rbind,team_games),stringsAsFactors=F)
for (j in 2:4) team_lev_df[,j] <- as.numeric(team_lev_df[,j])
names(team_lev_df) <- c('Team','Current','WithWin','WithLoss')
team_lev_df$TotalLev <- team_lev_df$WithWin - team_lev_df$WithLoss
team_lev_df$Team <- factor(team_lev_df$Team,team_lev_df$Team[order(team_lev_df$TotalLev)])
team_lev_df$Logo <-  paste0('logos/',team_lev_df$Team,'.png')
team_lev_df <- team_lev_df[order(team_lev_df$Team),]

tm_colors <- teams_df$Helm
names(tm_colors) <- teams_df$Abbr

ggplot(data = team_lev_df, aes(x = Team, ymin = WithLoss, 
         ymax = WithWin, lower = WithLoss, 
         upper = WithWin, middle = WithLoss, fill = Team)) +
	geom_boxplot(stat = 'identity', linetype = 1, size = .1, width = .7) +
	geom_crossbar(aes(y = Current, ymin = Current, ymax = Current), linetype = 3, size = .1, width = .7) +
	geom_image(aes(image = Logo, y = Current), size = .10) +
	geom_text(aes(x = 8.6, y = team_lev_df$Current[8], label = '\nNow'), size = 1.8, color = 'darkblue') +
	geom_text(aes(x = 8.6, y = team_lev_df$WithWin[8], label = 'With\nWin'), size = 1.8, color = 'darkblue') +
	geom_text(aes(x = 8.6, y = team_lev_df$WithLoss[8], label = 'With\nLoss'), size = 1.8, color = 'darkblue') +
	geom_point(aes(x = 8.9, y = 1.2)) +
	scale_fill_manual(values=tm_colors) +
	scale_y_continuous(limits= c(0,1), breaks = seq(0,1,.2), labels = percent) +
	coord_flip() + 
	labs(title='XFL Week 3 Playoff Leverage',
       caption = 'By Anthony Reinhard\nModel from statbutler.com/xfl',
       subtitle='Potential change in probability of making the playoffs with a win or loss this week',
       y = 'Playoff %') +
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
		,axis.title.y=element_blank()
	)

ggsave('po leverage.png', width=5, height=5)
