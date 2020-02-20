library(ggplot2)
library(ggimage)
library(extrafont)
setwd('C:/Users/rei1740/Desktop/Anthony/XFL')

pbp_df <- read.csv('reg_pbp_2020.csv', stringsAsFactors=F)
league_adv_df <- read.csv('4thDown.csv', stringsAsFactors=F)


pbp_df$Decision <- pbp_df$PlayType
pbp_df$Decision[which(pbp_df$PlayType %in% c('Rush','Pass'))] <- 'Go'
pbp_df$YardlineGrp <- cut(pbp_df$Yardline100, 100-c(0,31,42,45,47,51,53,62,65,66,69,72,79,86,99), labels= 14:1, right = F)
pbp_df$DistGrp <- pbp_df$Distance
pbp_df$DistGrp[which(pbp_df$DistGrp>=11)] <- '11+'
pbp_df$DistGrpFct <- factor(pbp_df$DistGrp, levels = c(1:10,'11+'))

all_fourth_df <- pbp_df[which(pbp_df$Down==4 & pbp_df$Decision!='Penalty'),]

#table(pbp_df$Yardline100[which(pbp_df$YardlineGrp==9)])

tm_dec_df <- aggregate(PlayID ~ ClubCode + Decision + DistGrp + YardlineGrp, data = pbp_df, FUN = length, subset = Down==4 & Decision!='Penalty')
tm_dec_df <- merge(tm_dec_df, league_adv_df, by = c('DistGrp','YardlineGrp'), all.x = T)
tm_dec_df$DecisionRank <- as.numeric(factor(tm_dec_df$Decision, levels = c('Go', 'Field Goal', 'Punt')))
tm_dec_df$SuggestedRank <- as.numeric(factor(tm_dec_df$Suggested, levels = c('Go', 'Field Goal', 'Punt')))

#tm_dec_df$Eval <- ifelse(tm_dec_df$DecisionRank == tm_dec_df$SuggestedRank, 'Same', ifelse(tm_dec_df$DecisionRank < tm_dec_df$SuggestedRank, 'More', 'Less'))
#aggregate(PlayID ~ ClubCode + Eval, data = tm_dec_df, FUN = sum)


tm_dec_df$Same <- ifelse(tm_dec_df$DecisionRank == tm_dec_df$SuggestedRank, tm_dec_df$PlayID, 0)
tm_dec_df$More <- ifelse(tm_dec_df$DecisionRank < tm_dec_df$SuggestedRank, tm_dec_df$PlayID, 0)
tm_dec_df$Less <- ifelse(tm_dec_df$DecisionRank > tm_dec_df$SuggestedRank, tm_dec_df$PlayID, 0)

agg_dec_df <- aggregate(cbind(PlayID, Same, More, Less) ~ ClubCode, data = tm_dec_df, FUN = sum)
agg_dec_df$SameOrMorePct <- 1 - (agg_dec_df$Less / agg_dec_df$PlayID)

league_adv_df$DistGrpFct <- factor(league_adv_df$DistGrp, levels = c(1:10,'11+'))

guide_heatmap <- expand.grid(Yardline100 = 1:99, DistGrpFct=levels(league_adv_df$DistGrpFct))
guide_heatmap$YardlineGrp <- cut(guide_heatmap$Yardline100, 100-c(0,31,42,45,47,51,53,62,65,66,69,72,79,86,99), labels= 14:1, right = F)
guide_heatmap <- merge(guide_heatmap, league_adv_df, by = c('DistGrpFct','YardlineGrp'), all.x = T)

capped_ydge <- guide_heatmap[which(guide_heatmap$DistGrp=='11+'),]

ext_ydge <- lapply(c(11:15,'16+'), function(x) {
  capped_ydge$DistGrp <- x
  return(capped_ydge)
})

guide_heatmap <- guide_heatmap[which(guide_heatmap$DistGrp!='11+'),]

guide_heatmap <- data.frame(rbind(guide_heatmap, do.call(rbind, ext_ydge)),stringsAsFactors = F)
guide_heatmap$DistGrpFct <- factor(guide_heatmap$DistGrp, levels = c(1:15,'16+'))
guide_heatmap$Yardline100 <- as.numeric(guide_heatmap$Yardline100)

all_fourth_df$DistGrp <- all_fourth_df$Distance
all_fourth_df$DistGrp[which(all_fourth_df$DistGrp>=16)] <- '16+'
all_fourth_df$DistGrpFct <- factor(all_fourth_df$DistGrp, levels = c(1:15,'16+'))


#league_adv_df$YardGrpSize <- rev(diff(rev(100-c(0,31,42,45,47,51,53,62,65,66,69,72,79,86,99))))[league_adv_df$YardlineGrp]
#league_adv_df$SuggestedFct <- factor(league_adv_df$Suggested, levels = c('Punt','Go','Field Goal'))

my_colors <- c('Go'='green','Field Goal'='orange','Punt'='pink')


ggplot() +
  geom_tile(data = guide_heatmap, aes(x = DistGrpFct, y = Yardline100, fill = Suggested)) +
  geom_hline(yintercept=seq(10, 90, by=10), color = 'grey50', size = .2, linetype = 2) +
  geom_jitter(data = all_fourth_df, aes(x = DistGrpFct, y = Yardline100, fill = Decision), shape = 21, color = 'darkblue', position=position_jitter(width=0.4), show.legend = F, stroke = .6) + 
  scale_y_reverse(limits = c(100,0), expand = c(0, 0), breaks = seq(0, 100, by=10)) +  
  scale_fill_manual(values=my_colors) +
  labs(title='XFL 4th Down Decision Making',
       caption = 'By Anthony Reinhard\nData from XFL.com',
       subtitle='Through Week 2 of 2020',
       y = 'Yards from End Zone',
       x = 'Yards to go on 4th Down',
       fill = 'Play Type') +
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
  theme(
    legend.background = element_rect(fill = 'grey90',color = 'darkblue'),
    panel.grid = element_blank()
  )

ggsave('XFL Go For it.png', width=5 * (16/9), height=5, dpi = 1000)

