sched_df <- read.csv('https://github.com/ajreinhard/xfl-public/raw/master/sched.csv', stringsAsFactors=F)


sched_df <- sched_df[which(!is.na(sched_df$HomeScore)),]
sched_df$matchup <- apply(sched_df[,c('Home','Away')], 1, function(x) paste(sort(x), collapse = '_'))

teams <- sort(unique(c(sched_df$Home,sched_df$Away)))

tm_sched_list <- lapply(teams, function(tm) {
  team_df <- sched_df[grep(tm, sched_df$matchup),]
  team_df$diff <- ifelse(team_df$Home==tm, team_df$HomeScore - team_df$AwayScore - 2.6, team_df$AwayScore - team_df$HomeScore + 2.6)
  team_df$opp <- gsub('_','',gsub(tm, '', team_df$matchup))
  return(cbind(tm,team_df[,c('opp','diff')]))
})

games_df <- do.call(rbind, tm_sched_list)
ratings_start <- aggregate(diff ~ tm, data = games_df, FUN = mean)
games_df <- merge(games_df, ratings_start, by.x = 'opp', by.y = 'tm', suffixes = c('','_opp'))
ratings <- aggregate(cbind(diff, diff_opp) ~ tm, data = games_df, FUN = mean)
ratings$final_rating <- new_ratings$diff + new_ratings$diff_opp

games_df$diff_opp <- NULL

for (i in 1:1000) {
  games_df$final_rating <- NULL
  games_df <- merge(games_df, ratings[,c('tm','final_rating')], by.x = 'opp', by.y = 'tm')
  ratings <- aggregate(cbind(diff, final_rating) ~ tm, data = games_df, FUN = mean)
  ratings$final_rating <- ratings$diff + ratings$final_rating
}
SRS_ratings <- ratings


vegas_df <- read.csv('vegas.csv', stringsAsFactors = F)
#vegas_df$matchup <- apply(vegas_df[,c('Home','Away')], 1, function(x) paste(sort(x), collapse = '_'))

#vegas_df$Home <- sample(teams, 24, replace = T)
#vegas_df$Away <- sample(teams, 24, replace = T)
#vegas_df$HomeLine <- rnorm(24) * 11
#next_wk <- sample(teams, 8, replace = F)
#vegas_df <- rbind(vegas_df,cbind(Home=next_wk[1:4],Away=next_wk[5:8],HomeLine=rnorm(4)*11))


spread_df <- data.frame(sapply(teams, function(tm) ifelse(vegas_df$Home==tm, 1, ifelse(vegas_df$Away==tm, -1, 0))))
spread_df$HomeLine <- as.numeric(vegas_df$HomeLine)

week <- c(matrix(1:4,4,nrow(vegas_df)/4,byrow = T))
time_weights <- 1/((max(week) - week) + 0.4)

my_model <- glm(HomeLine ~ ., data = spread_df, weights = time_weights)

mod_home_adv <- my_model$coefficients[1]

my_model$coefficients[9] <- 0
ratings <- my_model$coefficients
ratings <- ratings[2:9] - mean(ratings[2:9], na.rm = T)
data.frame(rev(sort(ratings)))













my_model$coefficients[9] <- 0
ratings2 <- my_model$coefficients[2:9] - my_model$coefficients[1]

sum(my_model$coefficients, na.rm = T)

sum(ratings2)


