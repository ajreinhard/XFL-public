setwd('C:/Users/rei1740/Desktop/Anthony/XFL')

vegas_df <- read.csv('vegas.csv', stringsAsFactors = F)
teams_df <- read.csv('https://github.com/ajreinhard/xfl-public/raw/master/teams.csv', stringsAsFactors = F)

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





####create ratings plot
library(ggplot2)
library(ggimage)
library(extrafont)
wk_rep <- c(matrix(1:ncol(vegas_rating_df)+2,8,ncol(vegas_rating_df),byrow=T))
tm_rep <- rep(teams,ncol(vegas_rating_df))
hist_rt <- c(vegas_rating_df[1:8,])

main_ratings_df <- data.frame('tm'=tm_rep,'wk'=wk_rep,'rate'=hist_rt,stringsAsFactors=F)

main_ratings_df$logo <- ifelse(main_ratings_df$wk == max(main_ratings_df$wk), paste0('https://github.com/ajreinhard/xfl-public/raw/master/logos/',tolower(main_ratings_df$tm),'.png'),NA)
week_ord <- c(paste0('Week ',1:10),'Semifinals','Championship','Post-Season')
main_ratings_df$wk <- factor(week_ord[main_ratings_df$wk], week_ord)


fill_col <- teams_df$Color2
names(fill_col) <- teams_df$Abbr
stk_col <- teams_df$Color1
names(stk_col) <- teams_df$Abbr

fill_col[4] <- teams_df$Color4[4]
stk_col[4] <- teams_df$Color3[4]


ggplot(data = main_ratings_df, aes(x = wk, y = rate, group = tm)) +
  geom_hline(yintercept=0, color='grey85',size = 1) +
  geom_line(aes(color = tm), size = 1) +
  geom_point(stroke = 1.4, aes(color = tm, fill = tm), size = 2.5, shape = 21) +
  geom_image(aes(image = logo), size = .08, nudge_x = .5, asp = (16/9)) +
  scale_color_manual(values=fill_col) +
  scale_fill_manual(values=stk_col) +
  scale_x_discrete(drop = F) +
  labs(title='XFL Implied Vegas Ratings',
       subtitle = 'Week by Week Ratings During 2020 Season',
       caption = 'By Anthony Reinhard',
       x = 'Ratings Entering Week',
       y = 'Rating') +
  theme_bw() +
  theme(
    text = element_text(family='Bahnschrift', color='darkblue'),
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

ggsave('Ratings over time.png', width = 5 * (16/9), height = 5, dpi = 1000)




