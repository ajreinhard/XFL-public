rm(list = ls())

library(ggplot2)
library(png)
library(ggimage)

setwd('C:/Users/Owner/Documents/GitHub/XFL/XFL')
#install.packages('ggimage', version = '0.2.5')
#devtools::install_github("GuangchuangYu/ggimage")


XFL_samp_df <- data.frame('logos'=dir('logos',full=T)[1:8],'x'=rnorm(8),'y'=rnorm(8), stringsAsFactors=F)


ggplot(data = XFL_samp_df,aes(x = x, y = y)) +
	geom_image(aes(image = logos), size = 0.13) +
      geom_hline(yintercept=0, color = 'grey90', size = 1.1) + 
      geom_vline(xintercept=0, color = 'grey90', size = 1.1) + 
	labs(x = 'Offensive EPA / Play',
	y = 'Defensive EPA / Play',
	caption = 'By Anthony Reinhard | Data from XFL.com\nEPA formula from @nflscrapeR',
	title = 'XFL Offensive & Defensive Efficiency',
	subtitle = 'Through Week 1 of 2020') +
	theme_bw() +
	theme(axis.title = element_text(size = 10),
	axis.text = element_text(size = 6),
	plot.title = element_text(size = 12),
	plot.subtitle = element_text(size = 10),
      plot.caption = element_text(size = 6))


ggsave('sample.png',dpi = 1000)
