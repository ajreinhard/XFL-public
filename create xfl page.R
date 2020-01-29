library(xml2)
library(htmlTable)
library(scales)

setwd('C:/Users/rei1740/Desktop/Anthony/XFL')

sim_df <- read.csv('sim results.csv', stringsAsFactors = F, row.names = 1)
xfl_teams <- row.names(sim_df)
games_df <- read.csv('sched.csv', stringsAsFactors = F)
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

primary_df <- data.frame(row.names = row.names(sim_df),'Rating'=sim_df$pretty_rate_curr, stringsAsFactors = F)
primary_df$'Playoff %' <- percent(sim_df$po_odds)
primary_df$'Champion %' <- percent(sim_df$champ_odds)
primary_df$'Expected Wins' <- round(sim_df$wins_avg,1)

primary_df <- primary_df[order(-(primary_df$Rating + sim_df$po_odds/1000)),]


games_df$concat <- paste0(games_df$Away,'_',games_df$Home)

tm <- xfl_teams[1]

tm_tbls <- sapply(xfl_teams, function(tm) {
  tm_games <- games_df[grepl(tm, games_df$concat),]
  tm_games$'Opponent' <- ifelse(tm_games$Home==tm, tm_games$Away, paste0('@ ',tm_games$Home))
  tm_games$'Win Probability' <- percent(ifelse(tm_games$Home==tm, tm_games$HomeWinProb, 1 - tm_games$HomeWinProb))
  tm_games$tm_spread <- ifelse(tm_games$Home==tm, tm_games$HomeSpread, -tm_games$HomeSpread)
  tm_games$'Point Spread' <- ifelse(tm_games$tm_spread > 0, paste0('Fav by ',tm_games$tm_spread), paste0('Dog by ',-tm_games$tm_spread))
  tm_games$'Game Info' <- paste0(gsub('/2020','',tm_games$Date),' @',tm_games$Time,'pm ET on ',tm_games$Network)
  return(htmlTable(tm_games[,c('Week','Game Info','Opponent','Win Probability','Point Spread')], rnames = F))
})



my_html <- paste(c(htmlTable(primary_df),tm_tbls), collapse = '<br>')

write_html(read_html(my_html), 'xfl.html')
