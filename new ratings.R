setwd('C:/Users/Owner/Documents/GitHub/XFL')


sched_df <- read.csv('https://github.com/ajreinhard/xfl-public/raw/master/sched.csv', stringsAsFactors=F)

sched_df <- sched_df[which(!is.na(sched_df$HomeScore)),]
sched_df$matchup <- apply(sched_df[,c('Home','Away')], 1, function(x) paste(sort(x), collapse = '_'))
sched_df$matchup <- apply(sched_df[,c('Home','Away')], 1, function(x) paste(sort(x), collapse = '_'))

teams <- sort(unique(c(sched_df$Home,sched_df$Away)))

tm_sched_list <- lapply(teams, function(tm) {
  team_df <- sched_df[grep(tm, sched_df$matchup),]
  team_df$diff <- ifelse(team_df$Home==tm, team_df$HomeScore - team_df$AwayScore - 2.6, team_df$AwayScore - team_df$HomeScore + 2.6)
  team_df$opp <- gsub('_','',gsub(tm, '', team_df$matchup))
  return(cbind(tm,team_df[,c('opp','diff')]))
})

games_df <- do.call(rbind, tm_sched_list)
cap <- 10
games_df$diff <- ifelse(games_df$diff>cap, cap, ifelse(games_df$diff < -cap,-cap,games_df$diff))

ratings_start <- aggregate(diff ~ tm, data = games_df, FUN = mean)
games_df <- merge(games_df, ratings_start, by.x = 'opp', by.y = 'tm', suffixes = c('','_opp'))
ratings <- aggregate(cbind(diff, diff_opp) ~ tm, data = games_df, FUN = mean)
ratings$final_rating <- ratings$diff + ratings$diff_opp

games_df$diff_opp <- NULL

for (i in 1:1000) {
  games_df$final_rating <- NULL
  games_df <- merge(games_df, ratings[,c('tm','final_rating')], by.x = 'opp', by.y = 'tm')
  ratings <- aggregate(cbind(diff, final_rating) ~ tm, data = games_df, FUN = mean)
  ratings$final_rating <- ratings$diff + ratings$final_rating
}
SRS_ratings <- ratings$final_rating
all_ratings <- data.frame(SRS_ratings)
row.names(all_ratings) <- ratings$tm


####begin vegas

vegas_df <- read.csv('vegas.csv', stringsAsFactors = F)

spread_df <- data.frame(sapply(teams, function(tm) ifelse(vegas_df$Home==tm, 1, ifelse(vegas_df$Away==tm, -1, 0))))
spread_df$HomeLine <- as.numeric(vegas_df$HomeLine)

week <- c(matrix(1:4,4,nrow(vegas_df)/4,byrow = T))
time_weights <- 1/((max(week) - week) + 0.4)

my_model <- glm(HomeLine ~ ., data = spread_df, weights = time_weights)

mod_home_adv <- my_model$coefficients[1]

my_model$coefficients[9] <- 0
ratings <- my_model$coefficients
ratings <- ratings[2:9] - mean(ratings[2:9], na.rm = T)
all_ratings$Vegas <- ratings

#### regular elo ratings

games_df <- read.csv('sched.csv', stringsAsFactors = F)
teams_df <- read.csv('teams.csv', stringsAsFactors = F)

K_fct <- 20
home_adv <- 65

implied_start <- 1500 + 25 * c('DAL'=1.4,'DC'=0.45,'HOU'=-0.6,'LA'=-0.45,'NY'=0.15,'SEA'=-1.5,'STL'=-0.65,'TB'=1.2)

rate_start <- data.frame('Tm'=sort(teams_df$Abbr), Rate=implied_start, stringsAsFactors = F)
gms_ply_df <- games_df[which(!is.na(games_df$HomeScore)),]

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
main_ratings <- rate_wk
all_ratings$Elo <- (rate_curr$Rate - 1500)/25



### zero elo start

implied_start <- rep(1500,8)

rate_start <- data.frame('Tm'=sort(teams_df$Abbr), Rate=implied_start, stringsAsFactors = F)
gms_ply_df <- games_df[which(!is.na(games_df$HomeScore)),]

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

all_ratings$EloZero <- (rate_curr$Rate - 1500)/25


###start double elo

implied_start <- rep(1500,8)

K_fct <- 40

rate_start <- data.frame('Tm'=sort(teams_df$Abbr), Rate=implied_start, stringsAsFactors = F)
gms_ply_df <- games_df[which(!is.na(games_df$HomeScore)),]

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

all_ratings$EloDouble <- (rate_curr$Rate - 1500)/25


write.csv(all_ratings,'multi_ratings.csv')




####create ratings plot
library(ggplot2)
library(ggimage)
library(extrafont)
tm_rep <- c(matrix(row.names(all_ratings),nrow(main_ratings),8,byrow=T))
wk_rep <- rep(nrow(main_ratings):1,8)
hist_rt <- (c(main_ratings)-1500)/25

main_ratings_df <- data.frame('tm'=tm_rep,'wk'=wk_rep,'rate'=hist_rt,stringsAsFactors=F)
main_ratings_df$logo <- ifelse(main_ratings_df$wk == max(main_ratings_df$wk), paste0('logos/',main_ratings_df$tm,'.png'),NA)
week_ord <- c(paste0('Week ',1:10),'Semifinals','Championship','Post-Season')
main_ratings_df$wk <- factor(week_ord[main_ratings_df$wk], week_ord)

teams_df$Color2[4] <- teams_df$Color3[4]
teams_df$Color1[4] <- teams_df$Color4[4]

fill_col <- teams_df$Color2
names(fill_col) <- teams_df$Abbr
stk_col <- teams_df$Color1
names(stk_col) <- teams_df$Abbr

ggplot(data = main_ratings_df, aes(x = wk, y = rate, group = tm)) +
	geom_line(aes(color = tm), size = 1) +
	geom_point(stroke = 1.4, aes(color = tm, fill = tm), size = 2.5, shape = 21) +
	geom_image(aes(image = logo), size = .08, nudge_x = .5, asp = (16/9)) +
	scale_color_manual(values=fill_col) +
	scale_fill_manual(values=stk_col) +
	scale_x_discrete(drop = F) +
	labs(title='XFL Elo Ratings',
	 subtitle = 'Week by Week Ratings During 2020 Season',
       caption = 'By Anthony Reinhard  |  Model from statbutler.com/xfl',
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

ggsave('Ratings over time.png', width = 5 * (16/9), height = 5, dpi = 700)




