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
primary_df$'Playoff %' <- percent(sim_df$po_odds)
primary_df$'Champion %' <- percent(sim_df$champ_odds)
primary_df$'Expected Wins' <- round(sim_df$wins_avg,1)

primary_df <- primary_df[order(-(primary_df$Rating + sim_df$po_odds/1000)),]

color_df <- merge(primary_df[,c('Team','Team')], teams_df[,c('Abbr','Color1','Color2')], by.x = 'Team', by.y = 'Abbr', all.x = T, sort = F)
#cell_color <- matrix('',ncol = ncol(primary_df), nrow = nrow(primary_df))
#cell_color <- paste0('color:',color_df$Color1,';background-color:',color_df$Color2)
cell_color <- matrix(paste0('color:',color_df$Color2,';background-color:',color_df$Color1),ncol = ncol(primary_df), nrow = nrow(primary_df))

primary_df$Team <- paste0('#',1:nrow(primary_df),' ',primary_df$Team)
games_df$concat <- paste0(games_df$Away,'_',games_df$Home)

tm_tbls <- sapply(xfl_teams, function(tm) {
  tm_games <- games_df[grepl(tm, games_df$concat),]
  tm_games$'Opponent' <- ifelse(tm_games$Home==tm, tm_games$Away, paste0('@ ',tm_games$Home))
  tm_games$'Win Probability' <- percent(ifelse(tm_games$Home==tm, tm_games$HomeWinProb, 1 - tm_games$HomeWinProb))
  tm_games$tm_spread <- ifelse(tm_games$Home==tm, tm_games$HomeSpread, -tm_games$HomeSpread)
  tm_games$'Point Spread' <- ifelse(tm_games$tm_spread > 0, paste0('Fav by ',tm_games$tm_spread), paste0('Dog by ',-tm_games$tm_spread))
  tm_games$'Game Info' <- paste0(gsub('/2020','',tm_games$Date),' @',tm_games$Time,'pm ET on ',tm_games$Network)
  tbl_only <- htmlTable(tm_games[,c('Week','Game Info','Opponent','Win Probability','Point Spread')], rnames = F)
  button_top  <- paste0('<button type="button" class="collapsible" style="background-image: url(\'logos/',tm,'.png\');">',tm,'</button><div class="content"><p>')
  button_bottom <- '</p></div>'
  return(paste0(button_top,tbl_only,button_bottom))
})



header <- '<head><title>XFL Elo Projections</title><link rel="stylesheet" type="text/css" href="ajr-theme.css"></head>'
my_sect <- '<p class="sect">Hello All!</p>'
coll_scrpt <- '<script src="collapsible.js"></script>'


primary_html <- htmlTable(primary_df, css.class='primary', css.cell=cell_color, rnames = F)
my_html <- paste(c(header,my_sect,primary_html,tm_tbls,coll_scrpt), collapse = '<br>')

write_html(read_html(my_html), 'xfl.html')
