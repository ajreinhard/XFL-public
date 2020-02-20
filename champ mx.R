library(htmlTable)
library(scales)
library(xml2)
setwd('C:/Users/rei1740/Desktop/Anthony/XFL')


sim_res_df <- read.csv('https://github.com/ajreinhard/xfl-public/raw/master/sim_res.csv', stringsAsFactors=F)

champ_match <- apply(sim_res_df[,c('XFL_CHAMP','XFL_RU')], 1, function(x) paste(sort(x), collapse = '_'))
names(sim_res_df)

champ_match[1]

east <- c('DC','NY','STL','TB')
west <- c('DAL','HOU','LA','SEA')

grep('DC', champ_match)

east_chmp_mx <- sapply(east, function(tm) ifelse(grepl(tm, champ_match),1,0))
west_chmp_mx <- sapply(west, function(tm) ifelse(grepl(tm, champ_match),tm,''))
west_chmp_vec <- apply(west_chmp, 1, paste, collapse = '')

row.names(east_chmp_mx) <- west_chmp_vec

sim_cnt <- aggregate(east_chmp_mx~west_chmp_vec, FUN = sum)
sim_cnt$west_chmp_vec <-NULL
sim_cnt <- sim_cnt/nrow(sim_res_df)
chmp_color_mx <- sapply(sim_cnt, function(x) paste0('background-color:',rgb(1-((x/1.5)/max(sim_cnt)),1,1-((x/1.5)/max(sim_cnt)))))
sim_cnt <- sapply(sim_cnt, percent, accuracy = .1)
row.names(sim_cnt) <- west
row.names(sim_cnt) <- paste0('<img src="logos/',west,'.png">')
names(sim_cnt) <- paste0('<img src="logos/',east,'.png">')


my_html <- htmlTable(sim_cnt, css.class = 'championship-matrix', css.cell = chmp_color_mx)
write_html(read_html(my_html), 'index.html')



