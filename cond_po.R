library(ggplot2)
library(scales)
library(extrafont)
library(png)
setwd('C:/Users/Owner/Documents/GitHub/XFL')

sim_res_df <- read.csv('sim_res.csv', stringsAsFactors=F)
teams_df <- read.csv('teams.csv', stringsAsFactors=F)

teams_df$Color2[4] <- teams_df$Color3[4]
teams_df$Color1[4] <- teams_df$Color4[4]

bar_fill <- teams_df$Color1
names(bar_fill) <- teams_df$Abbr
line_fill <- teams_df$Color2
names(line_fill) <- teams_df$Abbr

wins_df <- sim_res_df[,grep('_Wins',names(sim_res_df))]
names(wins_df) <- gsub('_Wins','',names(wins_df))

playoff_df <- sim_res_df[,c('WEST_RU','EAST_RU','XFL_RU','XFL_CHAMP')]
tm_playoff_df <- data.frame(t(apply(playoff_df, 1, function(x) ifelse(is.na(match(teams_df$Abbr, x)),0,1))))
names(tm_playoff_df) <- teams_df$Abbr

wins_to_po_list <- lapply(teams_df$Abbr, function(x) {
  wins_to_po <- aggregate(tm_playoff_df[,paste0(x)] ~ wins_df[,paste0(x)], FUN = mean, drop = F)
  names(wins_to_po) <- c('Wins','PO_prob')
  wins_to_po$Team <- x
  return(wins_to_po)
})

plot_df <- data.frame(do.call(rbind, wins_to_po_list), stringsAsFactors = F)


#games_left <- nrow(plot_df)/8-1
#plot_df$close_rec <- rep(paste0(0:games_left,'-',games_left:0),8)
plot_df$close_rec <- factor(paste0(plot_df$Wins,'-',10-plot_df$Wins), paste0(0:10,'-',10:0))

plot_df$PO_prob[which(plot_df$PO_prob==0)] <- NA
plot_df$text <- percent(round(plot_df$PO_prob,2))
plot_df$text[which(plot_df$text=='0%')] <- '<1%'

plot_df$Team <- factor(plot_df$Team,c('DC','NY','STL','TB','DAL','HOU','LA','SEA'))

ggplot(plot_df, aes(x=close_rec, y=PO_prob, fill = Team, color = Team, label = text)) + 
  geom_col(width = 1) +
  geom_text(color = 'darkblue', size = 2, nudge_y = .05, family='HP Simplified') +
  facet_wrap( ~ Team, nrow=4, scales='free_x', shrink= F) +
  labs(title='XFL Playoff Probability by Final Record',
       caption = 'By Anthony Reinhard  |  Model from statbutler.com/xfl',
       subtitle='Through Week 5 of 2020',
       y = 'Probability of Making the Playoffs',
       x = 'Final Record') +
  scale_y_continuous(labels = percent, limits = c(0,1.05), breaks = seq(0,1,.25)) +
  scale_fill_manual(values=bar_fill) +
  scale_color_manual(values=line_fill) +
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
  theme (
    legend.position = "none",
    strip.text.x = element_text(color = 'grey85'),
    strip.background = element_rect(fill = 'grey85', color = 'darkblue'),
    panel.grid.minor = element_blank()
  )


ggsave(paste0('cond_po.png'), dpi=700, height = 5 * (16/9), width = 5)

orig_plot <- readPNG('cond_po.png')
png('cond_po_post.png',width = 5, height = 5 * (16/9), units = 'in', res = 500)
par(usr = c(0,nrow(orig_plot),0,ncol(orig_plot)),mar = rep(0, 4))
plot(0,type='n',axes=FALSE,ann=FALSE)
lim <- par()
rasterImage(orig_plot, lim$usr[1], lim$usr[3], lim$usr[2], lim$usr[4])

#img1
#points(.845, .92)
#img3
#points(.845, .445)
#img4
#points(1.235, .445)


#i <- 8
#col_mid <- ifelse(i %% 2==1, 0.845, 1.235)
#row_mid <- (5-ceiling(i/2)) * .475 - 0.92

for (i in 1:8) {
  tm_logo <-  readPNG(paste0('team-text/',levels(plot_df$Team)[i],'.png'))
  col_mid <- ifelse(i %% 2==1, 0.845, 1.235)
  row_mid <- (5-ceiling(i/2)) * .49 - 1.055
  width_split <- ((ncol(tm_logo)/nrow(tm_logo)) * .04)/2.8
  rasterImage(tm_logo, col_mid - width_split, row_mid, col_mid + width_split, row_mid+.04)
}
dev.off()




#for (i in 1:8) {
#  download.file(paste0('https://github.com/ajreinhard/xfl-public/raw/master/team-text/',colnames(wins_mx)[i],'.png'),destfile=paste0('better text/',colnames(wins_mx)[i],'.png'), mode = 'wb')
#}
