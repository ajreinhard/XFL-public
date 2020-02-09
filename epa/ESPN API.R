library(RJSONIO)
library(nflscrapR)

#game_id <- '401131047'
#game_json <- fromJSON(paste0('http://site.api.espn.com/apis/site/v2/sports/football/nfl/summary?event=',game_id))

game_id <- '401181814'
game_json <- fromJSON(paste0('http://site.api.espn.com/apis/site/v2/sports/football/xfl/summary?event=',game_id))

all_col_names <- unique(unlist(sapply(game_json$drives$previous, function(drv) sapply(drv$plays, names))))

full_game_plays <- sapply(game_json$drives$previous, function(drv) sapply(drv$plays, unlist))
unique_names <- unique(names(unlist(full_game_plays)))

play_list <- lapply(full_game_plays, function(drv) {
	drive_df <- data.frame(t(sapply(drv, function(x) x[match(unique_names, names(x))])),stringsAsFactors=F)
	names(drive_df) <- unique_names
	return(drive_df)
})

full_play_df <- do.call(rbind, play_list)
full_play_df$period.number <- as.numeric(full_play_df$period.number)
full_play_df$qtr_mins <- as.numeric(sapply(strsplit(full_play_df$clock.displayValue,':'),function(x) x[1]))
full_play_df$qtr_secs <- as.numeric(sapply(strsplit(full_play_df$clock.displayValue,':'),function(x) x[2]))
full_play_df$qtr_tot_secs <- (full_play_df$qtr_mins)*60 + full_play_df$qtr_secs
full_play_df$start.half_tot_secs <- ifelse(full_play_df$period.number %% 2==0, full_play_df$qtr_tot_secs, full_play_df$qtr_tot_secs + 900)
full_play_df$start.goal_to_go <- ifelse(grepl('Goal',full_play_df$start.shortDownDistanceText),1,0)
full_play_df$start.down[which(full_play_df$start.down==0)] <- NA
full_play_df$start.distance[which(full_play_df$type.id==2)] <- full_play_df$end.distance[which(full_play_df$type.id==2)]
full_play_df$start.yardsToEndzone <- as.numeric(full_play_df$start.yardsToEndzone)

calculate_expected_points(full_play_df
					,half_seconds_remaining='start.half_tot_secs'
					,yard_line_100='start.yardsToEndzone'
					,down='start.down'
					,yards_to_go='start.distance'
					,goal_to_go='start.goal_to_go') 


head(full_play_df)






str(full_play_df)

names(game_json$drives[[1]][[1]])
names(game_json)
