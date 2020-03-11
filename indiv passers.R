library(mgcv)
library(ggplot2)
library(extrafont)
library(scales)
library(png)
setwd('C:/Users/rei1740/Desktop/Anthony/XFL')


pbp <- read.csv('https://github.com/ajreinhard/xfl-public/raw/master/epa/scraped PBP.csv', stringsAsFactors=F)

## fix penalties to match official league count
pbp$penalty <- ifelse(grepl('penalty',pbp$ShortPlayDescription, ignore.case = T),1,0)

pass_att_list <- rev(sort(table(pbp$passer_player_name[which(pbp$pass_attempt==1 & pbp$penalty==0 & pbp$ShortPlayDescription!='')])))[1:8]
cut_off_plyr <- rev(sort(table(pbp$passer_player_name[which(pbp$pass_attempt==1 & pbp$penalty==0 & pbp$ShortPlayDescription!='')])))[9]

if (floor(pass_att_list[8]/10) * 10 > cut_off_plyr) {
  min_pass <- floor(pass_att_list[8]/10) * 10
} else {
  min_pass <- ceiling(mean(c(cut_off_plyr, pass_att_list[8])))
}

## get pred for full league
xfl_air_yards_bam <- bam(complete_pass ~ s(air_yards), data = pbp, method = "fREML", discrete = TRUE, family = binomial(link='logit'))

## expected comp pct
pbp$exp_comp_pct <- exp(predict(xfl_air_yards_bam, pbp))/(1+exp(predict(xfl_air_yards_bam, pbp)))
exp_comp_df <- aggregate(cbind(exp_comp_pct, complete_pass) ~ passer_player_name, data = pbp, FUN = mean, subset = pass_attempt==1 & penalty==0 & ShortPlayDescription!='')
exp_comp_df$CPOE <- exp_comp_df$complete_pass - exp_comp_df$exp_comp_pct 
CPOE_order <- exp_comp_df$passer_player_name[order(-exp_comp_df$CPOE)]

## order by CPOE
player_list <- CPOE_order[sort(match(names(pass_att_list),CPOE_order))]


#player_list <- names(which(table(pbp$passer_player_name[which(!is.na(pbp$air_yards))])>50))
#aggregate(cbind(pass_attempt, complete_pass) ~ passer_player_name, data = pbp, FUN = sum, subset = air_yards >= 25)


player_bam <- function(pl_name) {
  fit_predict <- expand.grid('player'=pl_name,'air_yards'=-5:40)
  player_air_yards_bam <- bam(complete_pass ~ s(air_yards), data = pbp, method = "fREML", discrete = TRUE, family = binomial(link='logit'), subset = passer_player_name==pl_name)
  fit_predict$pred_comp <- predict(player_air_yards_bam, fit_predict)
  fit_predict$pred_comp_error <- predict(player_air_yards_bam, fit_predict, se = TRUE)$se.fit
  return(fit_predict)
}

##get pred and errors for each passer
pred_df <- data.frame(do.call(rbind, lapply(player_list, player_bam)), stringsAsFactors = F)

## add in pred for league
pred_df$pred_XFL <- predict(xfl_air_yards_bam, pred_df)
pred_df$true_comp_pred_XFL <- exp(fit_predict$pred_XFL)/(1+exp(fit_predict$pred_XFL))

## calculate individuals
pred_df$true_comp_pred <- exp(pred_df$pred_comp)/(1+exp(pred_df$pred_comp))
pred_df$pred_low = exp(pred_df$pred_comp  - 1.96*pred_df$pred_comp_error)/(1+exp(pred_df$pred_comp  - 1.96*pred_df$pred_comp_error))
pred_df$pred_high = exp(pred_df$pred_comp  + 1.96*pred_df$pred_comp_error)/(1+exp(pred_df$pred_comp  + 1.96*pred_df$pred_comp_error))


ggplot(data = pred_df, aes(x = air_yards)) +
  geom_line(aes(y = true_comp_pred), show.legend = F, color = 'blue') +
  geom_ribbon(aes(ymin = pred_low, ymax = pred_high), fill = 'blue', alpha = 0.25) +
  geom_line(aes(y = true_comp_pred_XFL), show.legend = F, color = 'red') +
  facet_wrap( ~ player, nrow = 4, shrink= F) +
  labs(title = 'XFL Completion Percentage by Depth of Target', 
       subtitle = 'Through Week 5 of 2020', 
       x = 'Air Yards', 
       y = 'Completion Percentage',
       caption  = 'By Anthony Reinhard  |  Data from @xflscrapR via @CFB_Moose') +
  scale_y_continuous(labels = percent, limits = c(0,1)) +
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
    plot.caption = element_text(size = 5),
    strip.text.x = element_text(color='darkblue'),
    strip.background = element_rect(fill = 'grey90', color = 'darkblue'),
    legend.background = element_rect(fill = 'grey90',color = 'darkblue'),
    legend.key = element_blank()
  ) + 
  theme(
    panel.grid.minor = element_blank()
  )


ggsave(paste0('passers_pre.png'), dpi=1000, height = 5 * (16/9), width = 5)



orig_plot <- readPNG('passers_pre.png')
png('passers_post.png',width = 5, height = 5 * (16/9), units = 'in', res = 500)
par(usr = c(0,nrow(orig_plot),0,ncol(orig_plot)),mar = rep(0, 4))
plot(0,type='n',axes=FALSE,ann=FALSE)
lim <- par()
rasterImage(orig_plot, lim$usr[1], lim$usr[3], lim$usr[2], lim$usr[4])


for (i in 1:8) {
  head_sht <- readPNG(paste0('headshots/',player_list[1],'.png'))
  tm_logo <- readPNG(paste0('headshots/',player_list[1],'.png'))
  col_mid <- ifelse(i %% 2==1, 1, 1.385)
  row_mid <- .9 - (floor((1:8-1)/2) * .48)
  width_split_hd <- ((ncol(head_sht)/nrow(head_sht)) * .055)/2.8
  rasterImage(head_sht, col_mid - width_split_hd, row_mid, col_mid + width_split_hd, row_mid+.055)
  width_split_tm <- ((ncol(tm_logo)/nrow(tm_logo)) * .055)/2.8
  rasterImage(tm_logo, col_mid - width_split_tm - .3, row_mid, col_mid + width_split_tm - .3, row_mid+.055)
}
dev.off()





