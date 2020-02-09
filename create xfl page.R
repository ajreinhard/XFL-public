library(xml2)
library(htmlTable)
library(scales)

setwd('C:/Users/Owner/Documents/GitHub/XFL')

sim_df <- read.csv('sim results.csv', stringsAsFactors = F, row.names = 1)
xfl_teams <- row.names(sim_df)
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

if (length(ply_gm_df)!=0) {
games_df <- merge(games_df, ply_gm_df[,c('Away','Home','Week','RateHome','RateAway')], by = c('Away','Home','Week'), all.x = T)
rnk_chg <- rank(rate_wk[1,] + 1:8/10000)-rank(rate_wk[2,] + 1:8/10000)
rnk_chg <- ifelse(rnk_chg==0,'No',ifelse(rnk_chg>0,paste0('Up_',rate_curr$Tm),paste0('Down_',rate_curr$Tm)))
} else {
games_df$RateHome <- NA
games_df$RateAway <- NA
rnk_chg <- rep('No',8)
}

xfl_rank <- row.names(sim_df)[order(-(sim_df$current_ratings + sim_df$po_odds/1000))]

games_df <- merge(merge(games_df, sim_df[,c('pre_season_ratings','current_ratings')], by.x = 'Away', by.y = 'row.names', all.x = T), sim_df[,c('pre_season_ratings','current_ratings')], by.x = 'Home', by.y = 'row.names', all.x = T, suffixes = c('Away','Home'))
games_df$pre_season_ratingsAway <- NULL
games_df$pre_season_ratingsHome <- NULL

games_df$current_ratingsHome <- ifelse(is.na(games_df$RateHome),games_df$current_ratingsHome,games_df$RateHome)
games_df$current_ratingsAway <- ifelse(is.na(games_df$RateAway),games_df$current_ratingsAway,games_df$RateAway)

games_df$Time <- as.numeric(games_df$Time)
games_df$GameDateTime <- as.POSIXct(strptime(paste0(games_df$Date,' ', ifelse(games_df$Time==12,games_df$Time, games_df$Time+12),':00:00'),'%m/%d/%Y %H:%M:%S'))
games_df$HomeWinProb <- 1/(1+10^(-(games_df$current_ratingsHome - games_df$current_ratingsAway + home_adv)/400))
games_df$HomeSpread <- (games_df$current_ratingsHome - games_df$current_ratingsAway + home_adv)/25
games_df$weekday <- format(games_df$GameDateTime, format = "%a")

games_df <- games_df[order(games_df$GameDateTime),]

sim_df$pretty_rate_curr <- sprintf("%.2f",round((sim_df$current_ratings - 1500)/25,2))
sim_df$arrow <- paste0('<img src="tv-logo/arrow',rnk_chg,'.png" height=25em align="right" vertical-align="middle">')
sim_df$Record <- paste0(sim_df$wins,'-',sim_df$losses)

primary_df <- data.frame('arrow'=sim_df$arrow,'Team' = xfl_teams, 'Record'=sim_df$Record, 'Rating'=sim_df$pretty_rate_curr, stringsAsFactors = F)
primary_df$'Playoff %' <- percent(sim_df$po_odds,.1)
primary_df$'Champion %' <- percent(sim_df$champ_odds,.1)
primary_df$'Expected Wins' <- sprintf("%.1f",round(sim_df$wins_avg,1))
primary_df <- primary_df[order(-(sim_df$current_ratings + sim_df$po_odds/1000)),]

color_df <- merge(primary_df[,c('Team','Team')], teams_df[,c('Abbr','Color1','Color2')], by.x = 'Team', by.y = 'Abbr', all.x = T, sort = F)
cell_color <- matrix(paste0('color:',color_df$Color2,';background-color:',color_df$Color1),ncol = ncol(primary_df)-2, nrow = nrow(primary_df))
align_arg <- 'lccc'

primary_df$Team <- paste0('#',1:nrow(primary_df),' ',primary_df$Team, ' (',primary_df$Record,')',primary_df$arrow)
primary_df$Record <- NULL
primary_df$arrow <- NULL

games_df$concat <- paste0(games_df$Away,'_',games_df$Home)



tm_tbls <- sapply(xfl_teams, function(tm) {
  tm_games <- games_df[grepl(tm, games_df$concat),]
  tm_games$'Opponent' <- ifelse(tm_games$Home==tm, tm_games$Away, tm_games$Home)
  tm_games$'Win Probability' <- percent(ifelse(tm_games$Home==tm, tm_games$HomeWinProb, 1 - tm_games$HomeWinProb),.1)
  tm_games$tm_spread <- round(ifelse(tm_games$Home==tm, tm_games$HomeSpread, -tm_games$HomeSpread),1)
  tm_games$'Point Spread' <- ifelse(tm_games$tm_spread == 0,'Toss Up',ifelse(tm_games$tm_spread > 0, paste0('Fav by ',tm_games$tm_spread), paste0('Dog by ',-tm_games$tm_spread)))
  tm_games$'Game Info' <- paste0(tm_games$weekday,' ',gsub('/2020','',tm_games$Date),' @',tm_games$Time,'pm ET<br><img src="tv-logo/',tm_games$Network,'.png" height="20em" width=auto')
  tm_games$Score <- paste0(ifelse(tm_games$Home==tm, ifelse(tm_games$HomeScore > tm_games$AwayScore, 'W ','L '),ifelse(tm_games$HomeScore > tm_games$AwayScore, 'L ','W ')), ifelse(tm_games$Home==tm, paste0(tm_games$HomeScore,'-',tm_games$AwayScore), paste0(tm_games$AwayScore,'-',tm_games$HomeScore)))
  tm_games$Score[which(is.na(tm_games$HomeScore))] <- ''
  tm_games$GameOrder <- 1:nrow(tm_games)
  #color_df <- merge(tm_games[,c('Opponent','GameOrder')], teams_df[,c('Abbr','Full','Color1','Color2')], by.x = 'Opponent', by.y = 'Abbr', all.x = T, sort = F)
  #color_df <- color_df[order(color_df$GameOrder),]
  #cell_color <- matrix('',ncol = 6, nrow = nrow(tm_games))
  #cell_color[,3] <- paste0('background: repeating-linear-gradient(90deg, transparent 0%, ',color_df$Color1,' 50%, transparent 100%);color:',color_df$Color2)
  #tm_games$'Opponent' <- paste0(ifelse(tm_games$Home==tm,'','@ '),color_df$Full)
  tm_games$'Opponent' <- paste0(ifelse(tm_games$Home==tm,'','@  '),'<img src="team-text/',tm_games$Opponent,'.png" height="30em" width=auto>')
  #tbl_only <- htmlTable(tm_games[,c('Week','Game Info','Opponent','Win Probability','Point Spread','Score')], rnames = F, css.class='team', css.cell=cell_color)
  tbl_only <- htmlTable(tm_games[,c('Week','Game Info','Opponent','Win Probability','Point Spread','Score')], rnames = F, css.class='team')
  button_top  <- paste0('<button type="button" class="collapsible" style="background-image: url(\'logos/',tm,'.png\');">',teams_df$Full[match(tm,teams_df$Abbr)],'</button><div class="content"><p>')
  button_bottom <- '</p></div>'
  return(paste0(button_top,tbl_only,button_bottom))
})

tm_tbls_header <- '<h2>Team Schedules</h2><p>Below are my full-season forecasts for each XFL team. Click on each team to expand their schedule.</p>'


curr_week <- games_df$Week[match(NA,games_df$HomeScore)]
upcom_gms <- games_df[which(games_df$Week==curr_week),]
upcom_gms <- merge(merge(upcom_gms, teams_df, by.x = 'Away', by.y = 'Abbr', all.x = T), teams_df, by.x = 'Home', by.y = 'Abbr', all.x = T, suffixes = c('Away','Home'))
upcom_gms <- upcom_gms[order(upcom_gms$GameDateTime),]

upcom_gms$GameHeaderAway <- paste0('#',match(upcom_gms$Away,xfl_rank),' ',upcom_gms$CityAway,ifelse(upcom_gms$HomeSpread<0,paste0('(-',sprintf("%.1f",round(upcom_gms$HomeSpread,1)),')'),''))
upcom_gms$GameHeaderHome <- paste0('#',match(upcom_gms$Home,xfl_rank),' ',upcom_gms$CityHome,ifelse(upcom_gms$HomeSpread>0,paste0('(-',sprintf("%.1f",round(upcom_gms$HomeSpread,1)),')'),''))

upcom_gms$GameByLine <- ifelse(is.na(upcom_gms$HomeScore), paste0(upcom_gms$weekday,' at ',upcom_gms$Time,'pm on ',upcom_gms$Network), paste0(upcom_gms$Away,' ',upcom_gms$AwayScore,', ',upcom_gms$Home,' ',upcom_gms$HomeScore))

upcoming_games <- paste(paste0('<tr>',
			'<td colspan=3 class="win-bar">',
			'<div class="team-bar" style="width:',((1-upcom_gms$HomeWinProb)/2)*100,'%;left:',(upcom_gms$HomeWinProb/2)*100,'%;background-color:',upcom_gms$Color1Away,';border-color:',upcom_gms$Color2Away,';text-align:left;color:',upcom_gms$Color2Away,'">',percent(1-upcom_gms$HomeWinProb,.1),'</div>',
			'<div class="team-bar" style="width:',(upcom_gms$HomeWinProb/2)*100,'%;left:',(upcom_gms$HomeWinProb/2)*100,'%;background-color:',upcom_gms$Color1Home,';border-color:',upcom_gms$Color2Home,';text-align:right;color:',upcom_gms$Color2Home,'">',percent(upcom_gms$HomeWinProb,.1),'</div>',
			'</td>',
			'</tr><tr>',
			'<td rowspan="4"><img src="logos/',upcom_gms$Away,'.png" height="130em"></td>',
			'<td align="left">',upcom_gms$GameHeaderAway,'</td>',
			'<td align="right" rowspan="4"><img src="logos/',upcom_gms$Home,'.png" height="130em"></td>',
			'</tr><tr>',
			'<td align="center">At</td>',
			'</tr><tr>',
			'<td align="right">',upcom_gms$GameHeaderHome,'</td>',
			'</tr><tr>',
			'<td align="center">',upcom_gms$GameByLine,'</td>',
			'</tr>')
			,collapse='')

up_gms_head <- paste0('<h2>Upcoming Games</h2><p>Here are my forecasts for this week\'s XFL games! The length of the bars below reflect win probability and point spread can be found in the parenthesis.</p>')
upcoming_games <- paste0(up_gms_head,'<table class="games-now"><tbody>',upcoming_games,'</tbody></table>')

header <- '<head><title>XFL Elo Projections</title><link rel="stylesheet" type="text/css" href="ajr-theme.css?v=2"></head>'

last_update <- format(Sys.time(),'%h %d @ %I:%M%p %Z')
#'<a href="https://www.bovada.lv/sports/football/xfl/odds-to-win-the-2020-xfl-championship-game-202002081401">XFL champion futures odds on Bovada</a>. ',
my_sect <- paste0('<body><h1>XFL Elo Projections</h1>',
		'<p class="sect">Welcome! This page is dedicated XFL elo rankings. The methodology I\'m using is ',
		'based on <a href = "https://fivethirtyeight.com/features/introducing-nfl-elo-ratings/">FiveThirtyEight\'s ',
		'pre-2019 NFL elo model</a>. Pre-season ratings were computed based on ',
		'XFL championship futures from six different sportsbooks. ',
		'The difference between two team\'s ratings is equal to the point spread between those two teams. ',
		'The home team will also get a 2.6 point home-field advantage. An average team will have a rating of zero. ',
		'Projections below are based on 10,000 simulations. ',
		'This page will be updated as games are played.</p>',
		'<p><b>Last Update: ',last_update,'</b></p>')
coll_scrpt <- '<script src="collapsible.js"></script>'

primary_html <- htmlTable(primary_df, css.class='primary', css.cell=cell_color, align=align_arg, rnames = F)
my_html <- paste0(header,paste(c(my_sect,primary_html,upcoming_games,tm_tbls_header,tm_tbls,coll_scrpt), collapse = ''))

write_html(read_html(my_html), 'index.html')
