library(gsheet)
library(stringr)
setwd('C:/Users/Owner/Documents/GitHub/XFL')

#pbp_df <- read.csv('https://github.com/ajreinhard/xfl-public/raw/master/epa/scraped PBP.csv', stringsAsFactors=F)
pbp_df <- read.csv('epa/scraped PBP Final.csv', stringsAsFactors=F)

air_yards <- gsheet2tbl('https://docs.google.com/spreadsheets/d/1Esh5R0zJFXlislSUngzm3GeLNDPLF4uckUknWFIoVZ0/edit#gid=0')
air_yards <- air_yards[which(!is.na(air_yards$START)),]
tail(air_yards)

air_yards$START <- ifelse(grepl('pbp has',air_yards$NOTES) & grepl(':',air_yards$NOTES), gsub("[^0-9:]", "", air_yards$NOTES), air_yards$START)
air_yards$PlayIden <- paste0(air_yards$GAMEID,'_',air_yards$QTR,'_',ifelse(substr(air_yards$START,1,1)==1 | nchar(air_yards$START)==4,air_yards$START,substr(air_yards$START,1,nchar(air_yards$START))))
pbp_df$PlayIden <- paste0(pbp_df$Game,'_',pbp_df$qtr,'_',ifelse(substr(pbp_df$time,1,1)==1,pbp_df$time,substr(pbp_df$time,2,nchar(pbp_df$time))))

air_yards$play_id <- pbp_df$play_id[match(air_yards$PlayIden, pbp_df$PlayIden)]
write.csv(air_yards, 'air_yards_updated3.csv', row.names=F)


###will check to see if any non-pass plays have been assigned air yards
###good for penalties, kickoffs
air_yards_final <- read.csv('epa/air_yards_fixed.csv', stringsAsFactors=F)
non_pass_air_yards <- air_yards_final$play_id[which(pbp_df$pass_attempt[match(air_yards_final$play_id, pbp_df$play_id)]==0 | is.na(pbp_df$pass_attempt[match(air_yards_final$play_id, pbp_df$play_id)]))]
air_yards_final[which(air_yards_final$play_id %in% non_pass_air_yards),]

which(table(air_yards_final$play_id)>1)





air_yards$
pbp_df$




table(pbp_df$Game[which(pbp_df$play_type=='pass' & pbp_df$qb_scramble==0 & pbp_df$sack==0)])
table(air_yards$GAMEID)

table(air_yards$QTR[which(air_yards$GAMEID==1)])


nrow(air_yards)

table(air_yards$GAMEID[is.na(match(air_yards$PlayIden, pbp_df$PlayIden))])
table(is.na(match(air_yards$PlayIden, pbp_df$PlayIden)))
