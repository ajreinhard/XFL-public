library(gsheet)
library(stringr)
setwd('C:/Users/rei1740/Desktop/Anthony/XFL')


pbp_df <- read.csv('https://github.com/ajreinhard/xfl-public/raw/master/epa/scraped PBP.csv', stringsAsFactors=F)

air_yards <- gsheet2tbl('https://docs.google.com/spreadsheets/d/1Esh5R0zJFXlislSUngzm3GeLNDPLF4uckUknWFIoVZ0/edit#gid=0')
air_yards <- air_yards[which(!is.na(air_yards$START)),]
tail(air_yards)

air_yards$PlayIden <- paste0(air_yards$GAMEID,'_',air_yards$QTR,'_',air_yards$START)
pbp_df$PlayIden <- paste0(pbp_df$Game,'_',pbp_df$qtr,'_',ifelse(substr(pbp_df$time,1,1)==1,pbp_df$time,substr(pbp_df$time,2,nchar(pbp_df$time))))

table(pbp_df$Game[which(pbp_df$play_type=='pass' & pbp_df$qb_scramble==0 & pbp_df$sack==0)])
table(air_yards$GAMEID)

table(air_yards$QTR[which(air_yards$GAMEID==1)])


nrow(air_yards)

table(air_yards$GAMEID[is.na(match(air_yards$PlayIden, pbp_df$PlayIden))])
table(is.na(match(air_yards$PlayIden, pbp_df$PlayIden)))
