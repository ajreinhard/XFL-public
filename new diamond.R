library(ggplot2)
library(ggimage)
library(gganimate)
library(extrafont)
library(grid)
library(png)

setwd('C:/Users/Owner/Documents/GitHub/XFL')

teams_df <- read.csv('teams.csv', stringsAsFactors=F)
pbp_df <- read.csv('epa/scraped PBP Final.csv', stringsAsFactors=F)

off_epa <- aggregate(epa ~ posteam, data = pbp_df, FUN = mean, subset = (PlayType == 'Pass' | PlayType == 'Rush'))
def_epa <- aggregate(cbind('epa'=-epa) ~ defteam, data = pbp_df, FUN = mean, subset = (PlayType == 'Pass' | PlayType == 'Rush'))

tot_epa <- merge(off_epa, def_epa, by.x = 'posteam', by.y = 'defteam', suffixes= c('_off','_def'))

tot_epa$x <- cos(45*pi/180)*tot_epa$epa_off - sin(45*pi/180)*tot_epa$epa_def
tot_epa$y <- cos(45*pi/180)*tot_epa$epa_def + sin(45*pi/180)*tot_epa$epa_off

lab_x <- labeling::extended(min(tot_epa$x, na.rm = T), max(tot_epa$x, na.rm = T), m = 5)
lab_y <- labeling::extended(min(tot_epa$y, na.rm = T), max(tot_epa$y, na.rm = T), m = 5)
diag_brks <- max(diff(lab_x)[1],diff(lab_y)[1])

bound_up_r <- max(tot_epa$epa_off) + diff(range(tot_epa$epa_off)) * .05
bound_lw_l <- min(tot_epa$epa_off) - diff(range(tot_epa$epa_off)) * .05
bound_up_l <- max(tot_epa$epa_def) + diff(range(tot_epa$epa_def)) * .05
bound_lw_r <- min(tot_epa$epa_def) - diff(range(tot_epa$epa_def)) * .05




mid_x <- mean(pbp_df$epa[which(pbp_df$PlayType == 'Pass' | pbp_df$PlayType == 'Rush')], na.rm = T)


properLims <- function(vec) {
  labs <- labeling::extended(min(vec, na.rm = T), max(vec, na.rm = T), m = 5)
  gap <- diff(labs[1:2])
  plot_max <- ifelse(rev(labs)[1] < max(vec, na.rm = T), rev(labs)[1] + gap, rev(labs)[1])
  plot_min <- ifelse(labs[1] > min(vec, na.rm = T), labs[1] - gap, labs[1])
  return(c(plot_min,plot_max))
} 

#properLims(c(tot_epa$epa_off, tot_epa$epa_def))
#limits = properLims

ggplot(data = tot_epa, aes(x = x, y = y, image = paste0('logos/',posteam,'.png'))) +
	geom_abline(intercept = seq(-.15,.15,.05)/cos(45*pi/180), linetype='solid', size = 2, color = 'grey95') +
	geom_abline(intercept = seq(-.15,.15,.05)/cos(45*pi/180), slope = -1, linetype='solid', size = 2, color = 'grey95') +
	geom_abline(intercept = 0, linetype='solid', size = 2, color = 'grey50') +
	geom_abline(intercept = 0, slope = -1, linetype='solid', size = 2, color = 'grey50') +
	geom_abline(intercept = c(bound_up_r, bound_lw_l)/cos(45*pi/180), slope = -1, linetype='solid', size = 2, color = 'darkblue') +
	geom_abline(intercept = c(bound_up_l, bound_lw_r)/cos(45*pi/180), slope = 1, linetype='solid', size = 2, color = 'darkblue') +
	labs(title = 'XFL EPA / Play') +
	geom_image(size = 0.17) +
	theme_bw() +
	theme(
	  text = element_text(family='HP Simplified', color='darkblue'),
	  plot.background = element_rect(fill = 'grey95', color = 'grey95'),
        panel.border = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 19),
        plot.subtitle = element_text(size = 12),
        plot.caption = element_text(size = 10)
	)

#ggsave('diamond epa-plot_reg.png', height = 5, width = 5, dpi = 1000)

