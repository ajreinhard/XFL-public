setwd('C:/Users/Owner/Documents/GitHub/XFL/2001')

sched_df <- read.csv('sched.csv', stringsAsFactors=F)
teams_df <- read.csv('teams.csv', stringsAsFactors=F)

sched_df <- sched_df[which(!is.na(sched_df$HomeScore)),]
sched_df$matchup <- apply(sched_df[,c('Home','Away')], 1, function(x) paste(sort(x), collapse = '_'))
sched_df$matchup <- apply(sched_df[,c('Home','Away')], 1, function(x) paste(sort(x), collapse = '_'))

teams <- sort(unique(c(sched_df$Home,sched_df$Away)))

K_fct <- 20
home_adv <- 65

### zero elo start
implied_start <- rep(1500,8)

rate_start <- data.frame('Tm'=sort(teams), Rate=implied_start, stringsAsFactors = F)
gms_ply_df <- sched_df[which(!is.na(sched_df$HomeScore)),]

rate_curr <- rate_start
ply_gm_df <- c()
rate_wk <- rate_start$Rate

for (wk in unique(gms_ply_df$Week)) {
  this_wk <- gms_ply_df[which(gms_ply_df$Week==wk),]
  this_wk <- merge(merge(this_wk, rate_curr, by.x = 'Home', by.y = 'Tm'),rate_curr, by.x = 'Away', by.y = 'Tm', suffix = c('Home','Away'))
  this_wk$HomeWinProb <- 1/(1+10^(-(this_wk$RateHome - this_wk$RateAway + home_adv)/400))
  this_wk$HomeWin <- ifelse(this_wk$HomeScore > this_wk$AwayScore, 1, 0)
  this_wk$EloChng <-  (this_wk$HomeWin - this_wk$HomeWinProb) * 
    K_fct *
    log(abs(this_wk$HomeScore - this_wk$AwayScore) + 1) * 
    (2.2/(2.2 + (ifelse(this_wk$HomeWin==1,1/1000,-1/1000) * (this_wk$RateHome - this_wk$RateAway + home_adv))))
  this_wk$RateHomeNew <- this_wk$RateHome + this_wk$EloChng
  this_wk$RateAwayNew <- this_wk$RateAway - this_wk$EloChng
  rate_new <- data.frame('Tm'=c(this_wk$Away, this_wk$Home), 'NewRate'=c(this_wk$RateAwayNew, this_wk$RateHomeNew), stringsAsFactors = F)
  rate_curr <- merge(rate_curr, rate_new, by = 'Tm', all.x = T)
  rate_curr$Rate <- ifelse(is.na(rate_curr$NewRate), rate_curr$Rate, rate_curr$NewRate)
  rate_curr$NewRate <- NULL
  ply_gm_df <- rbind(ply_gm_df, this_wk)
  rate_wk <- rbind(rate_curr$Rate,rate_wk)
}




####create ratings plot
library(ggplot2)
library(ggimage)
library(extrafont)
tm_rep <- c(matrix(teams,nrow(rate_wk),8,byrow=T))
wk_rep <- rep(nrow(rate_wk):1,8)
hist_rt <- (c(rate_wk)-1500)/25

main_ratings_df <- data.frame('tm'=tm_rep,'wk'=wk_rep,'rate'=hist_rt,stringsAsFactors=F)
main_ratings_df$Abbr <- teams_df$Abbr[match(main_ratings_df$tm, teams_df$Abbr)]
main_ratings_df$logo <- ifelse(main_ratings_df$wk == max(main_ratings_df$wk), paste0('logos/',main_ratings_df$Abbr,'.png'),NA)
week_ord <- c(paste0('Week ',1:10),'Semifinals','Million Dollar Game','Post-Season')
main_ratings_df$wk <- factor(week_ord[main_ratings_df$wk], week_ord)

fill_col <- teams_df$Color2
names(fill_col) <- teams_df$Abbr
stk_col <- teams_df$Color1
names(stk_col) <- teams_df$Abbr

ggplot(data = main_ratings_df, aes(x = wk, y = rate, group = tm)) +
	geom_hline(yintercept=0, color='grey85',size = 1) +
	geom_line(aes(color = tm), size = 1) +
	geom_point(stroke = 1.4, aes(color = tm, fill = tm), size = 2.5, shape = 21) +
	geom_image(aes(image = logo), size = .05, nudge_x = .6, asp = (16/9)) +
	scale_color_manual(values=fill_col) +
	scale_fill_manual(values=stk_col) +
	scale_y_continuous(limit = c(-8,8), breaks = seq(-8,8,4)) +
	scale_x_discrete(drop = F, expand = expansion(add = c(.4,1))) +
	labs(title='XFL Elo Ratings',
	 subtitle = 'Week by Week Ratings During 2001 Season',
       caption = 'By Anthony Reinhard',
       x = 'Ratings Entering Week',
       y = 'Rating') +
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
	) + 
	theme(legend.position = "none", axis.text.x=element_text(angle = -25, size = 7, hjust=.1, vjust=.7))

ggsave('Ratings XFL.png', width = 5 * (16/9), height = 5, dpi = 700)











