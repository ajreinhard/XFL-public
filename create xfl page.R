library(xml2)
library(htmlTable)
library(scales)

setwd('C:/Users/Owner/Documents/GitHub/XFL')

epa_week_of <- 5

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

if (nrow(ply_gm_df)!=0) {
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
games_df$GameID <- 1:43

sim_df$pretty_rate_curr <- sprintf("%+.2f",round((sim_df$current_ratings - 1500)/25,2))
sim_df$arrow <- paste0('<img src="tv-logo/arrow',rnk_chg,'.png" height=26em align="right" vertical-align="middle">')
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
  tm_games$Score <- ifelse(tm_games$Score=='','',paste0('<a href = "http://stats.xfl.com/',tm_games$GameID,'">',tm_games$Score,'</a>'))
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

upcom_gms$GameHeaderAway <- paste0('#',match(upcom_gms$Away,xfl_rank),' ',upcom_gms$CityAway,ifelse(upcom_gms$HomeSpread<0,paste0('(',sprintf("%.1f",round(upcom_gms$HomeSpread,1)),')'),''))
upcom_gms$GameHeaderHome <- paste0('#',match(upcom_gms$Home,xfl_rank),' ',upcom_gms$CityHome,ifelse(upcom_gms$HomeSpread>0,paste0('(-',sprintf("%.1f",round(upcom_gms$HomeSpread,1)),')'),''))

upcom_gms$GameByLine <- paste0('<a href="http://stats.xfl.com/',upcom_gms$GameID,'">',ifelse(is.na(upcom_gms$HomeScore), paste0(upcom_gms$weekday,' at ',upcom_gms$Time,'pm on ',upcom_gms$Network), paste0(upcom_gms$Away,' ',upcom_gms$AwayScore,', ',upcom_gms$Home,' ',upcom_gms$HomeScore)),'</a>')

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


###create champ matrix
sim_res_df <- read.csv('sim_res.csv', stringsAsFactors=F)
champ_match <- apply(sim_res_df[,c('XFL_CHAMP','XFL_RU')], 1, function(x) paste(sort(x), collapse = '_'))

east <- c('DC','NY','STL','TB')
west <- c('DAL','HOU','LA','SEA')

east_chmp_mx <- sapply(east, function(tm) ifelse(grepl(tm, champ_match),1,0))
west_chmp_mx <- sapply(west, function(tm) ifelse(grepl(tm, champ_match),tm,''))
west_chmp_vec <- apply(west_chmp_mx, 1, paste, collapse = '')

row.names(east_chmp_mx) <- west_chmp_vec

sim_cnt <- aggregate(east_chmp_mx~west_chmp_vec, FUN = sum)
sim_cnt$west_chmp_vec <-NULL
sim_cnt <- sim_cnt/nrow(sim_res_df)
chmp_color_mx <- sapply(sim_cnt, function(x) paste0('background-color:',rgb(1-((x)/max(sim_cnt)),1,1-((x)/max(sim_cnt)))))
sim_cnt <- sapply(sim_cnt, percent, accuracy = .1)
row.names(sim_cnt) <- west
row.names(sim_cnt) <- paste0('<img src="logos/',west,'.png" height=70em width=auto>')
colnames(sim_cnt) <- paste0('<img src="logos/',east,'.png" height=70em width=auto>')

chmp_intro <- paste0('<h2>XFL Championship Matchup Odds</h2><p>The matrix below shows the probability of each possible XFL Championship Game matchup. ',
			'Teams from the Eastern Conference are along the top and teams from the Western Conference are along the left side. ',
			'The XFL Championship Game will be played in Houston at 3pm ET on Sunday, April 26th.</p>')

chmp_html <- paste0(chmp_intro,htmlTable(sim_cnt, css.class = 'championship-matrix', css.cell = chmp_color_mx))
chmp_html <- gsub('border-bottom: 1px solid grey','',chmp_html)
###end champ matrix

###begin create EPA
off_epa <- read.csv('epa/offensive_summary.csv', row.names = 1, stringsAsFactors=F)
def_epa <- read.csv('epa/defensive_summary.csv', row.names = 1, stringsAsFactors=F)

names(off_epa) <- gsub('\\.',' ',gsub('\\.\\.\\.',' / ',names(off_epa)))
names(def_epa) <- gsub('\\.',' ',gsub('\\.\\.\\.',' / ',names(def_epa)))

off_epa <- rbind(off_epa[order(-off_epa$'Offensive EPA / Play'[1:8]),],off_epa[9,])
def_epa <- rbind(def_epa[order(def_epa$'Defensive EPA / Play'[1:8]),],def_epa[9,])

for (i in grep('Rat',names(off_epa))) off_epa[,i] <- percent(off_epa[,i], accuracy = 0.1)
for (i in grep('Rat',names(def_epa))) def_epa[,i] <- percent(def_epa[,i], accuracy = 0.1)
for (i in grep('EPA',names(off_epa))) off_epa[,i] <- sprintf("%+.2f",round(off_epa[,i], digits = 2))
for (i in grep('EPA',names(def_epa))) def_epa[,i] <- sprintf("%+.2f",round(def_epa[,i], digits = 2))

for (i in grep('EPA',names(off_epa))) off_epa[,i] <- ifelse(off_epa[,i]=='-0.00' | off_epa[,i]=='+0.00','0.00',off_epa[,i])
for (i in grep('EPA',names(def_epa))) def_epa[,i] <- ifelse(def_epa[,i]=='-0.00' | def_epa[,i]=='+0.00','0.00',def_epa[,i])

off_color <- paste0('background-color:',teams_df$Color1[match(row.names(off_epa),teams_df$Abbr)],';color:',teams_df$Color2[match(row.names(off_epa),teams_df$Abbr)],';')
off_color[9] <- 'background-color:#012169;color:#FFFFFF;border-top: 5px solid black;'
def_color <- paste0('background-color:',teams_df$Color1[match(row.names(def_epa),teams_df$Abbr)],';color:',teams_df$Color2[match(row.names(def_epa),teams_df$Abbr)],';')
def_color[9] <- 'background-color:#012169;color:#FFFFFF;border-top: 5px solid black;'

off_epa$Team <- row.names(off_epa)
def_epa$Team <- row.names(def_epa)
off_epa <- off_epa[,c(ncol(off_epa),1:(ncol(off_epa)-1))]
def_epa <- def_epa[,c(ncol(def_epa),1:(ncol(def_epa)-1))]

off_color_mx <- matrix(off_color,ncol = ncol(off_epa), nrow=nrow(off_epa))
def_color_mx <- matrix(def_color,ncol = ncol(def_epa), nrow=nrow(def_epa))

off_epa_html <- htmlTable(off_epa, css.cell = off_color_mx, css.class = 'epa', rnames = F, cgroup = c('','All','Dropback','Rush',''), n.cgroup = c(1,2,2,2,1), header = c('Team',rep(c('EPA/Play','Success<br>Rate'),3),'Dropback<br>Ratio'))
def_epa_html <- htmlTable(def_epa, css.cell = def_color_mx, css.class = 'epa', rnames = F, cgroup = c('','All','Dropback','Rush',''), n.cgroup = c(1,2,2,2,1), header = c('Team',rep(c('EPA/Play','Success<br> Rate'),3),'Dropback<br>Ratio'))

off_epa_header <- '<h3>Offensive Plays</h3><p>Teams with a higher EPA and a higher success rate have better offenses.</p>'
def_epa_header <- '<h3>Defensive Plays</h3><p>Teams with a lower EPA and a lower success rate have better defenses.</p>'

epa_last_upd <- paste0('<p><b>EPA stats are upadated through Week ',epa_week_of,'</b></p>')
epa_intro <- paste0('<h2>Expected Points Added and Success Rate</h2><p>The two tables below show some advanced metrics for each team. ',
			'The first is EPA or Expected Points Added. The general goal of an expected points model is to put the production from each play into the appropriate context. ',
			'This is especially important in football where context can be burried in the box score. We know that a five yard gain on 3rd & 4 is worth significantly more than a 10 yard gain on 4th & 12 and EPA will tell us that one of those two plays was far more valuable than the other. ',
			'EPA can be calculated by taking the difference between the number of points a team was expected to score before a play and the number of points a team is expected to score after that play is over. The second metric that I\'ve included is success rate. ',
			'Success rate is defined as the percentage of plays where a team achieved a positive EPA.</p>',
			'<p>All of the data below was retrieved via <a href = "https://github.com/keegan-abdoo/xflscrapR">xflscrapR</a>, a tool created by Keegan Abdoo and Caio Brighenti that was created to scrape play-by-play data from <a href="https://xfl.com">xfl.com</a>. ',
			'The EPA model was created with <a href = "https://github.com/ryurko/nflscrapR">nflscrapR</a>, an NFL web scraper. Since the nflscrapR EPA model is fitted for the NFL, these results should be taken with caution.</p>',epa_last_upd)
			

epa_html <- paste0(epa_intro,off_epa_header,off_epa_html,def_epa_header,def_epa_html)
###end create EPA


header <- '<head><title>XFL Elo Projections</title><link rel="stylesheet" type="text/css" href="ajr-theme.css?v=4"></head>'

last_update <- format(file.info('sim results.csv')$mtime,'%h %d @ %I:%M%p %Z')
#'<a href="https://www.bovada.lv/sports/football/xfl/odds-to-win-the-2020-xfl-championship-game-202002081401">XFL champion futures odds on Bovada</a>. ',
my_sect <- paste0('<body><h1>XFL Elo Projections</h1>',
		'<p class="sect">Welcome! This page is dedicated XFL Elo rankings. The methodology I\'m using is ',
		'based on <a href = "https://fivethirtyeight.com/features/introducing-nfl-elo-ratings/">FiveThirtyEight\'s ',
		'pre-2019 NFL Elo model</a>. Pre-season ratings were computed based on ',
		'XFL championship futures from six different sportsbooks. ',
		'You can read about my pre-season ratings in my post on <a href = "https://xflfanzone.net/2020/02/07/deep-routes-improbable-probability-xfl-week-1/">xflfanzone.net</a>. ',
		'The difference between two team\'s ratings is equal to the point spread between those two teams. ',
		'The home team will also get a 2.6 point home-field advantage. An average team will have a rating of zero. ',
		'Projections below are based on 10,000 simulations. ',
		'This page will be updated as games are played.</p>',
		'<p><b>Last Update: ',last_update,'</b></p>')
coll_scrpt <- '<script src="collapsible.js"></script>'

primary_html <- htmlTable(primary_df, css.class='primary', css.cell=cell_color, align=align_arg, rnames = F)
primary_html <- gsub('border-collapse: collapse; margin-top: 1em; margin-bottom: 1em;','',primary_html)

my_html <- paste0(header,paste(c(my_sect,primary_html,chmp_html,upcoming_games,epa_html,tm_tbls_header,tm_tbls,coll_scrpt), collapse = ''))

write_html(read_html(my_html), 'index.html')
