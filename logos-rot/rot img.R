

save_img <- function(i) {
img_plt <- ggplot(data = tot_epa[i,], aes(x = 0, y = 0, image = paste0('logos/',posteam,'.png')), xlim = c(-1,1), ylim = c(-1,1)) +
	geom_image(size = 1) +
	theme_void() + 
	theme(rect = element_rect(fill = 'transparent', color = 'transparent'))

png(paste0('logos-rot/',tot_epa$posteam[i],'.png'), bg = 'transparent')
pushViewport(viewport(name = 'rotate', angle = -45, width = 1, height = 1))
print(img_plt, vp = 'rotate')
dev.off()
}

for (i in 1:8) save_img(i)


main_plot <- ggplot(data = data.frame(NA),aes(x = 0, y = 0, image = 'diamond epa-plot.png'), xlim = c(-1,1), ylim = c(-1,1)) +
	geom_image(size = 1) +
	theme_void() + 
	theme(rect = element_rect(fill = 'transparent', color = 'transparent'))

png('diamond epa-plot-rot.png', bg = 'transparent')
pushViewport(viewport(name = 'rotate', angle = 45, width = 1, height = 1))
print(main_plot, vp = 'rotate')
dev.off()
