library(rvest)
library(RJSONIO)

setwd('C:/Users/Owner/Documents/GitHub/XFL/epa')

retrive_games <- function(game_ids) {

game_list <- lapply(game_ids, function(i) {
URL <- paste0('http://stats.xfl.com/',i)
webpage <- read_html(URL)

js_scrip <- html_nodes(webpage, xpath = '//script[@type="text/javascript"]')[6] %>% html_text()
pbp_json <- fromJSON(substring(js_scrip, 21, nchar(js_scrip)-6))
pbp_df <- data.frame(do.call(rbind,lapply(pbp_json$plays,function(x) sapply(x, function(y) ifelse(is.null(y), NA, y)))), stringsAsFactors = F)

pbp_df$home_team <- pbp_json$homeClubCode
pbp_df$away_team <- pbp_json$awayClubCode
pbp_df$side_of_field <- sapply(strsplit(pbp_df$Yardline,' '), function(x) x[[1]])
pbp_df$yardline_50 <- as.numeric(sapply(strsplit(pbp_df$Yardline,' '), function(x) x[[2]]))
pbp_df$GameID <- as.numeric(pbp_df$GameID)
pbp_df$Distance <- as.numeric(pbp_df$Distance)

pbp_df$StartAwayScore <- as.numeric(pbp_df$StartAwayScore)
pbp_df$StartHomeScore <- as.numeric(pbp_df$StartHomeScore)
pbp_df$EndAwayScore <- as.numeric(pbp_df$EndAwayScore)
pbp_df$EndHomeScore <- as.numeric(pbp_df$EndHomeScore)

keep_cols <- c('GameID','PlayID','Quarter','StartTime','ClubCode',
			'PlayDescription','DrivePlays','DriveNetYards',
			'home_team','away_team','Down','Distance','yardline_50', 'side_of_field',
			'PlayType','StartHomeScore','StartAwayScore','EndHomeScore',
			'EndAwayScore','ShortPlayDescription','playScoringTeamId')  

return(pbp_df[,paste0(keep_cols)])
})
full_game_df <- do.call(rbind, game_list)
return(full_game_df)
}



clean_data <- function(df){

col_names <- c("Game", "play_id", "qtr", "time", "posteam", "desc",
			"DrivePlays", "DriveYards", "home_team", "away_team",
			"down","ydstogo","yardline_50", "side_of_field",
			"PlayType","total_home_score","total_away_score","EndHomeScore",
			"EndAwayScore","ShortPlayDescription","td_team")  
names(df) <- col_names

  pbp1 <- df %>%
    select(col_names) %>%
    mutate(# Fix Game 7 issue where one drive has the wrong Offense
           # posteam = if_else(Game == 7 & qtr == 4 & between(play_id, 655, 690), "DAL", posteam),
           # Add Defensive Team
           defteam = if_else(posteam == home_team, home_team, away_team),
           posteam_type = if_else(posteam==home_team, "home", "away"),
           defteam = if_else(posteam == home_team, away_team, home_team),
           week = ceiling(Game/4),
           # Score Columns
           posteam_score = if_else(posteam == home_team, total_home_score, total_away_score),
           defteam_score = if_else(posteam == away_team, total_home_score, total_away_score),
           posteam_score_post = if_else(posteam == home_team, EndHomeScore, EndAwayScore),
           defteam_score_post = if_else(posteam == away_team, EndHomeScore, EndAwayScore),
           score_differential = posteam_score - defteam_score,
           score_differential_post = posteam_score_post - defteam_score_post,
           pos_score_change = score_differential_post - score_differential,
           # Create GameIDs in NFL's GSIS format
           game_id = if_else(Game%%4 %in% c(1, 2),
                            str_remove_all(glue("{ymd(20200208) + (week-1)*7}"), "-"),
                            str_remove_all(glue("{ymd(20200209) + (week-1)*7}"), "-")),
           game_id = if_else(Game%%2 == 1,
                            as.character(glue("{game_id}00")),
                            as.character(glue("{game_id}01"))),
           game_id = as.numeric(game_id),
           # GameTime
           quarter_seconds_remaining = as.numeric(str_extract(as.character(time), "(?<=:)[0-9]{2}")) * 60 +
             as.numeric(str_extract(as.character(time), "[0-9]{2}$")),
           half_seconds_remaining = if_else(qtr %in% c("2","4"), quarter_seconds_remaining, quarter_seconds_remaining + 900),
           game_seconds_remaining = quarter_seconds_remaining + ((4 - as.numeric(qtr)) * 900),
           # Extract Down and Distance
           # down = str_extract(Situation, "^[1-4]"),
           # ydstogo = as.numeric(str_extract(Situation, "(?<=\\& )[0-9]+")),
           # Convert Field Position to Yards from the end zone
           yardline_100 = if_else(posteam == side_of_field, 100 - yardline_50, yardline_50),
           # Is it goal to go?
           goal_to_go = if_else(ydstogo == yardline_100, 1, 0),
           # Get Play Types
           play_type = case_when(str_detect(desc, "[1-3]pt attempt") ~ "extra_point",
                                str_detect(desc, "(rush)") ~ "run",
                                str_detect(desc, "(kneel)") ~ "qb_kneel",
                                str_detect(desc, "(spike)") ~ "qb_spike",
                                str_detect(desc, "(pass)|(scramble)|(sack)") ~ "pass",
                                str_detect(desc, "punt") ~ "punt",
                                str_detect(desc, "kickoff") ~ "kickoff",
                                str_detect(str_to_lower(desc), "field goal") ~ "field_goal",
                                TRUE ~ "no_play"),
           # Did the QB align in shotgun?
           shotgun = case_when(play_type %in% c("run", "pass") ~ if_else(str_detect(desc, "Shotgun"), 1, 0)),
           # Did the offense Huddle?
           no_huddle = case_when(play_type %in% c("run", "pass") ~ if_else(str_detect(desc, "No Huddle"), 1, 0)),
           # Was the pass a spike?
           qb_spike = case_when(play_type == "qb_spike" ~ if_else(str_detect(desc, "spike"), 1, 0)),
           # Was the play a QB Kneel?
           qb_kneel = case_when(play_type == "qb_kneel" ~ if_else(str_detect(desc, "kneel"), 1, 0)),
           # Was the QB sacked?
           sack = case_when(play_type == "pass" ~ if_else(str_detect(desc, "sack"), 1, 0)),
           # Did the QB Scramble?
           qb_scramble = case_when(play_type == "pass" ~ if_else(str_detect(desc, "scramble"), 1, 0)),
           # Was there a pass attempt?
           pass_attempt = case_when(play_type == "pass" ~ if_else(sack == 0 & qb_scramble == 0, 1, 0)),
           # Was the pass intercepted?
           interception = case_when(pass_attempt == 1 ~ 
                                        if_else(str_detect(str_to_lower(desc), "intercepted"), 1, 0)),
           # Was the pass completed?
           complete_pass = case_when(pass_attempt == 1 ~ 
                                    if_else(str_detect(desc, "incomplete") |
                                                !str_detect(desc, "[0-9]") |
                                                interception == 1, 0, 1)),
           # Was the pass thrown away?
           throwaway = case_when(pass_attempt == 1 ~ ifelse(str_detect(desc, "incomplete\\."), 1, 0)),
           # Pass Depth and Direction buckets
           pass_length = str_extract(desc, "(?<=pass (incomplete )?)(short)|(deep)"),
           pass_location = str_extract(desc, "(?<=pass (incomplete )?((short)|(deep)) )(left)|(middle)|(right)"),
           # Run Direction to get location and gap
           run_direction = str_extract(desc, "(?<=rush )([A-z]+\\s[A-z]+)+(?=\\s((to)|(out)|(for)))"),
           run_location = str_extract(run_direction, "(left)|(right)|(middle)"),
           run_gap = str_extract(run_direction, "(end)|(tackle)|(guard)|(middle)"),
           # Did the play result in a touchdown?
           touchdown = if_else(str_detect(str_to_lower(desc), "touchdown"), 1, 0),
           # Was the field goal made?	
           field_goal_made = case_when(play_type == "field_goal" ~ if_else(str_detect(tolower(desc),"is good"),1,0)),
           # Was there a fumble?
           fumble = if_else(str_detect(str_to_lower(desc), "fumble"), 1, 0),
           # Yards Gained from Scrimmage
           yards_gained = case_when(qb_spike == 1 | complete_pass == 0 ~ 0,
                                   play_type %in% c("run", "pass") & fumble == 1 & 
                                       !str_detect(desc, "Aborted") ~ yardline_100 - 
                                       yl_100(str_extract(desc, "(?<= to )[A-Z]{2,3}\\s[0-9]{1,2}"), posteam),
                                   play_type == "pass" & str_detect(desc, "Aborted") ~ yardline_100 - 
                                       yl_100(str_extract(desc, "(?<= to )[A-Z]{2,3}\\s[0-9]{1,2}"), posteam),
                                   play_type == "run" & str_detect(desc, "Aborted") ~ yardline_100 - 
                                       yl_100(str_extract(desc, "(?<= at the )[A-Z]{2,3}\\s[0-9]{1,2}"), posteam),
                                   play_type %in% c("run","pass") ~ 
                                       as.numeric(str_extract(desc, "\\-?[0-9]{1,2}(?= yards)"))),
           # Did the play result in a first down? # is.na() handles first downs before penalties, might cause false positives
           first_down = if_else((posteam == lead(posteam) | is.na(lead(posteam))) & game_id == lead(game_id) & 
                                 play_type %in% c("run", "pass") & (lead(down) == 1 | is.na(lead(down))) & 
                                 yards_gained >= ydstogo, 1, 0),
           # Did the offense turn the ball over?
           turnover = if_else(((posteam != lead(posteam) & game_id == lead(game_id) & 
                                    qtr == lead(qtr)) | interception == 1) &
                                  !(play_type %in% c("punt", "field_goal", "extra_point")), 1, 0),
           turnover_type = case_when(turnover == 1 & fumble == 1 ~ "Fumble",
                                    turnover == 1 & interception == 1 ~ "Interception",
                                    turnover == 1 & down == 4 ~ "Downs"),
           # Did the defense recover the fumble?
           fumble_lost = if_else(fumble == 1 & turnover == 1, 1, 0),
           # Did the Offense go for it on fourth down?
           fourth_down_decision = case_when(down == 4 ~ if_else(play_type %in% c("pass", "run"), 1, 0)),
           # What Type of Extra Point did the offense attempt?
           extra_point_type = as.numeric(if_else(play_type == "extra_point", 
                                                 str_extract(desc, "^[1-3]"), "NA")),
           # Extra point succesful?
           extra_point_conversion = case_when(play_type == "extra_point" ~ 
                                               if_else(str_detect(desc, " successful"), 1, 0)),
           # Is this a penalty?
           penalty = if_else(str_detect(desc, "PENALTY") | str_detect(desc, "penalty"),1,0),
           # Fix missing down, distance, yardline, and goal to go for penalties
           down = case_when(penalty == 1 & lag(first_down) == 1 ~ "1",
                            penalty == 1 & lag(first_down) != 1 & lag(down) != "4" ~ as.character(as.numeric(lag(down,1)) + 1), # need better fix to avoid 5th down
                            TRUE ~ down),
           ydstogo = case_when(penalty == 1 & lag(first_down) == 1 ~ 10,
                                penalty == 1 & lag(first_down) != 1 ~ lag(ydstogo) - lag(yards_gained),
                                TRUE ~ ydstogo),
           yardline_100 = case_when(penalty == 1 ~ lag(yardline_100) - lag(yards_gained),
                                    TRUE ~ yardline_100),
           goal_to_go = if_else(ydstogo == yardline_100, 1, 0),
           # Tackle information
           solo_tackle = case_when(str_detect(desc, "[A-Z]\\.[A-z]+(\\'|\\-)?[A-z]+\\)") ~
                                     if_else(str_detect(desc, "\\([A-Z]\\.[A-z]+(\\'|\\-)?[A-z]+\\)"), 1, 0)),
           assist_tackle = case_when(str_detect(desc, "[A-Z]\\.[A-z]+(\\'|\\-)?[A-z]+\\)") ~
                                       if_else(str_detect(desc, ";") |
                                                 str_detect(desc, ", [A-Z]\\.[A-z]+(\\'|\\-)?[A-z]+\\)"), 1, 0)),
           tackle_for_loss = case_when(solo_tackle == 1 | assist_tackle == 1 ~ if_else(yards_gained < 0, 1, 0)),
           # # On who is the penalty?
           # penalty_team = case_when(penalty == 1 ~ gsub(".*PENALTY","",desc) %>% str_split(" ") %>% sapply( "[[", 3) %>% 
           #                           gsub(pattern = "[.]", replacement = "")
           #                        ),
           # # What kind of penalty?
           # penalty_type = case_when(penalty == 1 ~ gsub(".*PENALTY","",desc) %>% stri_extract_first_regex(pattern=c("[.].*,")) %>%
           #                           str_split(",") %>% sapply( "[[", 1) %>% gsub(pattern="([.] )",replacement="")
           #                         ),
           # # How many yards lost/gained?
           # PenaltyYards = case_when(penalty == 1 & penalty_team == posteam ~ -as.numeric(stri_extract_first_regex(gsub(".*PENALTY","",desc),pattern=c("\\-*\\d+\\.*\\d*"))),
           #                          penalty == 1 & penalty_team != posteam ~ as.numeric(stri_extract_first_regex(gsub(".*PENALTY","",desc),pattern=c("\\-*\\d+\\.*\\d*")))
           # ),
           # Extract passer name
           passer_player_name = case_when(play_type == "pass" & qb_scramble != 1 ~ 
                                    str_extract(desc, "[A-Z]\\.[A-z]+(\\'|\\-)?[A-z]+")),
           # Extract receiver name
           receiver_player_name = case_when(play_type == "pass" & qb_scramble != 1 ~
                                      str_extract(desc, "(?<=((to)|(for)) )[A-Z]\\.[A-z]+(\\'|\\-)?[A-z]+")),
           # Extract rusher name
           rusher_player_name = case_when(play_type == "run" | qb_scramble == 1 ~ 
                                    str_extract(desc, "[A-Z]\\.[A-z]+(\\'|\\-)?[A-z]+")),
           # Extract interceptor name
           interception_player_name = case_when(interception == 1 ~
                                                  str_extract(desc, "(?<=INTERCEPTED by )[A-Z]\\.[A-z]+(\\'|\\-)?[A-z]+")),
           # Extract Solo Tackle player name
           solo_tackle_player_name = case_when(solo_tackle == 1 ~
                                                  str_extract(desc, "(?<=\\()[A-Z]\\.[A-z]+(\\'|\\-)?[A-z]+")),
           # Extract Assist Tackle player names
           assist_tackle_1_player_name = case_when(assist_tackle == 1 ~
                                                     str_extract(desc, "(?<=\\()[A-Z]\\.[A-z]+(\\'|\\-)?[A-z]+")),
           assist_tackle_2_player_name = case_when(assist_tackle == 1 ~
                                                     str_extract(desc, "(?<=(;|,) )[A-Z]\\.[A-z]+(\\'|\\-)?[A-z]+(?=\\))")))
  
  return(pbp1)
}


raw_pbp_df <- retrive_games(1:12)
clean_pbp <- clean_data(raw_pbp_df)

write.csv(clean_pbp, 'scraped PBP.csv', row.names = F)