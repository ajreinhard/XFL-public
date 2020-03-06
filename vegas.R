setwd('C:/Users/rei1740/Desktop/Anthony/XFL')

vegas_df <- read.csv('vegas.csv', stringsAsFactors = F)
teams <- sort(unique(c(vegas_df$Home,vegas_df$Away)))

spread_df <- data.frame(sapply(teams, function(tm) ifelse(vegas_df$Home==tm, 1, ifelse(vegas_df$Away==tm, -1, 0))))
spread_df$HomeLine <- as.numeric(vegas_df$HomeLine)

vegas_rating_df <- sapply(3:(nrow(spread_df)/4), function(wk) {
  spread_df <- spread_df[c(1:(wk*4)),]
  week <- c(matrix(1:wk,4,nrow(spread_df)/4,byrow = T))
  time_weights <- 1/((max(week) - week) + 0.4)
  
  my_model <- glm(HomeLine ~ ., data = spread_df, weights = time_weights)
  
  mod_home_adv <- my_model$coefficients[1]
  
  my_model$coefficients[9] <- 0
  ratings <- my_model$coefficients
  ratings <- ratings[2:9] - mean(ratings[2:9], na.rm = T)
  return(c(ratings,mod_home_adv))
})


vegas_rating_df
#data.frame(rev(sort(ratings)))


