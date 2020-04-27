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
def_epa <- aggregate(epa ~ defteam, data = pbp_df, FUN = mean, subset = (PlayType == 'Pass' | PlayType == 'Rush'))

tot_epa <- merge(off_epa, def_epa, by.x = 'posteam', by.y = 'defteam', suffixes= c('_off','_def'))

league_avg <- mean(pbp_df$epa[which(pbp_df$PlayType == 'Pass' | pbp_df$PlayType == 'Rush')], na.rm = T)

x_max <- max(-min(tot_epa$epa_off - league_avg),max(tot_epa$epa_off - league_avg))
y_max <- max(-min(tot_epa$epa_def - league_avg),max(tot_epa$epa_def - league_avg))
axis_len <-  max(x_max,y_max) * 1.05

set_lim <- c(league_avg - axis_len, league_avg + axis_len)



tot_epa$rel_off <- tot_epa$epa_off + league_avg
tot_epa$rel_def <- tot_epa$epa_def + league_avg


set_lim <- properLims(unlist(tot_epa[,c(2:5)]))

properLims <- function(vec) {
  labs <- labeling::extended(min(vec, na.rm = T), max(vec, na.rm = T), m = 5)
  gap <- diff(labs[1:2])
  plot_max <- ifelse(rev(labs)[1] < max(vec, na.rm = T), rev(labs)[1] + gap, rev(labs)[1])
  plot_min <- ifelse(labs[1] > min(vec, na.rm = T), labs[1] - gap, labs[1])
  return(c(plot_min,plot_max))
} 


off_gap <- diff(labeling::extended(min(tot_epa$epa_off, na.rm = T), max(tot_epa$epa_off, na.rm = T), m = 5))[1]
def_gap <- diff(labeling::extended(min(tot_epa$epa_def, na.rm = T), max(tot_epa$epa_def, na.rm = T), m = 5))[1]
def_gap/off_gap

spread_by <- (max(c(diff(range(tot_epa$epa_off)),diff(range(tot_epa$epa_def))))-min(c(diff(range(tot_epa$epa_off)),diff(range(tot_epa$epa_def)))))/2
new_lims <- range(tot_epa$epa_def) + c(-spread_by,spread_by)
#limit = rev(new_lims)




set_lim <- properLims(c(tot_epa$epa_off, tot_epa$epa_def))

#properLims(c(tot_epa$epa_off, tot_epa$epa_def))
#limits = properLims

p <- ggplot(data = tot_epa, aes(x = epa_off, y = epa_def, image = paste0('logos-rot/',posteam,'.png'))) +
	geom_abline(intercept = league_avg * -2, linetype='dashed', size = 0.3, color = 'red') +
	geom_image(size = 0.17) +
	scale_y_reverse(limits = rev(set_lim)) +
	scale_x_continuous(limits = set_lim) +
	labs(	y = 'Defensive EPA / Play',
		x = 'Offensive EPA / Play'
	) +
	coord_fixed(ratio = 1, expand = T) +
	theme_bw() +
	theme(
	  text = element_text(family='HP Simplified', color='darkblue'),
	  plot.background = element_rect(fill = 'grey95', color = 'grey95'),
        panel.border = element_rect(color = 'darkblue'),
        axis.ticks = element_line(color = 'darkblue'),
        axis.title = element_text(size = 13),
        axis.text.x = element_text(size = 11, color = 'darkblue', angle = -45, hjust=0),
        axis.text.y = element_text(size = 11, color = 'darkblue', angle = -45, vjust=1),
        axis.title.y = element_text(angle = 270),
        plot.title = element_text(size = 19),
        plot.subtitle = element_text(size = 12),
        plot.caption = element_text(size = 10),
	  plot.margin = margin(1, 1, 1, 1, 'cm'),
	  aspect.ratio = 1
	)

ggsave('diamond epa-plot.png', height = 5, width = 5, dpi = 1000)


plot_png <- readPNG('diamond epa-plot.png')
png(paste0('diamond epa.png'), bg = 'grey95', width = 500, height = 500 * 16/9)
par(usr = c(0,nrow(plot_png),0,ncol(plot_png)),mar = rep(0, 4))
pl <- plot(0,type='n',axes=FALSE,ann=FALSE)
lim <- par()
rasterImage(plot_png, lim$usr[1], lim$usr[3], lim$usr[2], lim$usr[4])

pushViewport(viewport(name = "rotate", angle = 45, width = 1.5, height = 1))
print(p, vp = "rotate")
dev.off()






