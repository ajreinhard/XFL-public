library(xml2)
library(htmlTable)
library(scales)

setwd('C:/Users/Owner/Documents/GitHub/XFL')

sim_df <- read.csv('sim results.csv', stringsAsFactors = F, row.names = 1)
xfl_teams <- row.names(sim_df)
games_df <- read.csv('sched.csv', stringsAsFactors = F)
teams_df <- read.csv('teams.csv', stringsAsFactors = F)
home_adv <- 65

games_df <- merge(merge(games_df, sim_df[,c('pre_season_ratings','current_ratings')], by.x = 'Away', by.y = 'row.names', all.x = T), sim_df[,c('pre_season_ratings','current_ratings')], by.x = 'Home', by.y = 'row.names', all.x = T, suffixes = c('Away','Home'))
games_df$pre_season_ratingsAway <- NULL
games_df$pre_season_ratingsHome <- NULL
games_df$Time <- as.numeric(games_df$Time)
games_df$GameDateTime <- as.POSIXct(strptime(paste0(games_df$Date,' ', ifelse(games_df$Time==12,games_df$Time, games_df$Time+12),':00:00'),'%m/%d/%Y %H:%M:%S'))
games_df$HomeWinProb <- 1/(1+10^(-(games_df$current_ratingsHome - games_df$current_ratingsAway + home_adv)/400))
games_df$HomeSpread <- (games_df$current_ratingsHome - games_df$current_ratingsAway + home_adv)/25

games_df <- games_df[order(games_df$GameDateTime),]

sim_df$pretty_rate_curr <- round((sim_df$current_ratings - 1500)/25,2)

primary_df <- data.frame('Team' = xfl_teams,'Rating'=sim_df$pretty_rate_curr, stringsAsFactors = F)
primary_df$'Playoff %' <- percent(sim_df$po_odds,.1)
primary_df$'Champion %' <- percent(sim_df$champ_odds,.1)
primary_df$'Expected Wins' <- round(sim_df$wins_avg,1)

primary_df <- primary_df[order(-(primary_df$Rating + sim_df$po_odds/1000)),]

color_df <- merge(primary_df[,c('Team','Team')], teams_df[,c('Abbr','Color1','Color2')], by.x = 'Team', by.y = 'Abbr', all.x = T, sort = F)
cell_color <- matrix(paste0('color:',color_df$Color2,';background-color:',color_df$Color1),ncol = ncol(primary_df), nrow = nrow(primary_df))

primary_df$Team <- paste0('#',1:nrow(primary_df),' ',primary_df$Team)
games_df$concat <- paste0(games_df$Away,'_',games_df$Home)

tm_tbls <- sapply(xfl_teams, function(tm) {
  #tm <- xfl_teams[1]
  tm_games <- games_df[grepl(tm, games_df$concat),]
  tm_games$'Opponent' <- ifelse(tm_games$Home==tm, tm_games$Away, tm_games$Home)
  tm_games$'Win Probability' <- percent(ifelse(tm_games$Home==tm, tm_games$HomeWinProb, 1 - tm_games$HomeWinProb),.1)
  tm_games$tm_spread <- ifelse(tm_games$Home==tm, tm_games$HomeSpread, -tm_games$HomeSpread)
  tm_games$'Point Spread' <- ifelse(tm_games$tm_spread > 0, paste0('Fav by ',tm_games$tm_spread), paste0('Dog by ',-tm_games$tm_spread))
  tm_games$'Game Info' <- paste0(gsub('/2020','',tm_games$Date),' @',tm_games$Time,'pm ET on ',tm_games$Network)
  tm_games$GameOrder <- 1:nrow(tm_games)
  color_df <- merge(tm_games[,c('Opponent','GameOrder')], teams_df[,c('Abbr','Full','Color1','Color2')], by.x = 'Opponent', by.y = 'Abbr', all.x = T, sort = F)
  color_df <- color_df[order(color_df$GameOrder),]
  cell_color <- matrix('',ncol = 5, nrow = nrow(tm_games))
  cell_color[,3] <- paste0('background: repeating-linear-gradient(90deg, transparent 0%, ',color_df$Color1,' 50%, transparent 100%);color:',color_df$Color2)
  tm_games$'Opponent' <- paste0(ifelse(tm_games$Home==tm,'','@ '),color_df$Full)
  tbl_only <- htmlTable(tm_games[,c('Week','Game Info','Opponent','Win Probability','Point Spread')], rnames = F, css.class='team', css.cell=cell_color)
  button_top  <- paste0('<button type="button" class="collapsible" style="background-image: url(\'logos/',tm,'.png\');">',tm,'</button><div class="content"><p>')
  button_bottom <- '</p></div>'
  return(paste0(button_top,tbl_only,button_bottom))
})


header <- '<head><title>XFL Elo Projections</title><link rel="stylesheet" type="text/css" href="ajr-theme.css"></head>'

my_sect <- paste0('<h2>XFL Elo Projections<h2>',
		'<p class="sect">Welcome! This page is dedicated XFL elo rankings. The methodology I\'m using is ',
		'based on <a href = "https://fivethirtyeight.com/features/introducing-nfl-elo-ratings/">FiveThirtyEight\'s ',
		'pre-2019 NFL elo model</a>. Pre-season ratings were computed based on ',
		'<a href="https://www.bovada.lv/sports/football/xfl/odds-to-win-the-2020-xfl-championship-game-202002081401">XFL champion ',
		'futures odds on Bovada</a>. The difference between two team\'s ratings is equal to the point spread between those two teams. ',
		'The home team will also get a 2.6 point home-field advantage.</p>')
coll_scrpt <- '<script src="collapsible.js"></script>'

primary_html <- htmlTable(primary_df, css.class='primary', css.cell=cell_color, rnames = F)
my_html <- paste(c(header,my_sect,primary_html,tm_tbls,coll_scrpt), collapse = '<br>')

write_html(read_html(my_html), 'xfl.html')
