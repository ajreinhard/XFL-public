library(ggplot2)
library(scales)
library(extrafont)
library(png)
setwd('C:/Users/Owner/Documents/GitHub/XFL')

sim_res_df <- read.csv('https://github.com/ajreinhard/xfl-public/raw/master/sim_res.csv', stringsAsFactors=F)
teams_df <- read.csv('https://github.com/ajreinhard/xfl-public/raw/master/teams.csv', stringsAsFactors=F)

teams_df$Color2[4] <- teams_df$Color3[4]
teams_df$Color1[4] <- teams_df$Color4[4]


bar_fill <- teams_df$Color1
names(bar_fill) <- teams_df$Abbr
line_fill <- teams_df$Color2
names(line_fill) <- teams_df$Abbr

wins_df <- sim_res_df[,grep('_Wins',names(sim_res_df))]
names(wins_df) <- gsub('_Wins','',names(wins_df))

wins_mx <- sapply(wins_df, function(tm) table(factor(tm, levels = 0:10)))/nrow(sim_res_df)
plot_df <- expand.grid('Wins'=0:10, 'Team'=colnames(wins_mx))
plot_df$Freq <- c(wins_mx)
plot_df$Freq[which(plot_df$Freq==0)] <- NA
plot_df$text <- percent(round(plot_df$Freq,2))
plot_df$text[which(plot_df$text=='0%')] <- '<1%'

plot_df$Team <- factor(plot_df$Team,c('DC','NY','STL','TB','DAL','HOU','LA','SEA'))

ggplot(plot_df, aes(x=Wins, y=Freq, fill = Team, color = Team, label = text)) + 
  geom_col(width = 1) +
  geom_text(color = 'darkblue', size = 2, nudge_y = .015, family='Bahnschrift') +
  facet_wrap( ~ Team, nrow=4, scales='free_x', shrink= F) +
  labs(title='XFL Win Distribution',
       caption = 'By Anthony Reinhard  |  Model from statbutler.com/xfl',
       subtitle='Through Week 3 of 2020',
       y = 'Frequency',
       x = 'Win Total') +
  scale_y_continuous(labels = percent, limits = c(0,.3)) +
  scale_x_continuous(breaks = 0:10) +
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


ggsave(paste0('win_plot_pre.png'), dpi=1000, height = 5 * (16/9), width = 5)

orig_plot <- readPNG('win_plot_pre.png')
png('win_plot_post.png',width = 5, height = 5 * (16/9), units = 'in', res = 500)
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
  tm_logo <-  readPNG(paste0('better text/',levels(plot_df$Team)[i],'.png'))
  col_mid <- ifelse(i %% 2==1, 0.845, 1.235)
  row_mid <- (5-ceiling(i/2)) * .49 - 1.055
  width_split <- ((ncol(tm_logo)/nrow(tm_logo)) * .04)/2.8
  rasterImage(tm_logo, col_mid - width_split, row_mid, col_mid + width_split, row_mid+.04)
}
dev.off()



#for (i in 1:8) {
#  download.file(paste0('https://github.com/ajreinhard/xfl-public/raw/master/team-text/',colnames(wins_mx)[i],'.png'),destfile=paste0('better text/',colnames(wins_mx)[i],'.png'), mode = 'wb')
#}
