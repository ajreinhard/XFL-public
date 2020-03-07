setwd('C:/Users/Owner/Documents/GitHub/XFL')
library(scales)
library(htmlTable)
library(xml2)
library(ggplot2)
library(webshot)
library(extrafont)
library(rvest)
library(RJSONIO)
library(glue)
library(tidyverse)
library(lubridate)
source('C:/Users/Owner/Documents/Other XFL/helpers.R')

#pbp_df <- read.csv('epa/scraped PBP.csv', stringsAsFactors=F)
#teams_df <- read.csv('teams.csv', stringsAsFactors=F)


#pbp_df <- read.csv('https://github.com/ajreinhard/xfl-public/raw/master/epa/scraped PBP.csv', stringsAsFactors=F)
#pass_mean <- mean(pbp_df$epa[which(pbp_df$play_type == 'pass')], na.rm = T)
#pass_sd <- sd(pbp_df$epa[which(pbp_df$play_type == 'pass')], na.rm = T)
#pnorm(8.11,pass_mean * 32, pass_sd * sqrt(32))

game_id <- 10
#game_df <- pbp_df[which(pbp_df$Game==game_id),]

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
pbp_df$PlayDescription <- gsub(" no gain ", " 0 yards ", pbp_df$PlayDescription)

keep_cols <- c('GameID','PlayID','Quarter','StartTime','ClubCode',
			'PlayDescription','DrivePlays','DriveNetYards',
			'home_team','away_team','Down','Distance','yardline_50', 'side_of_field',
			'PlayType','StartHomeScore','StartAwayScore','EndHomeScore',
			'EndAwayScore','ShortPlayDescription','playScoringTeamId','DriveID')  

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
			"EndAwayScore","ShortPlayDescription","td_team","drive_id")  
names(df) <- col_names

  pbp1 <- df %>%
    select(col_names) %>%
    mutate(# Fix Game 7 & 16 issue where one drive has the wrong Offense
           posteam = if_else(drive_id==6478, "DAL", posteam),
           posteam = if_else(drive_id==11688, "TB", posteam),
           # Add Defensive Team
           defteam = if_else(posteam == home_team, home_team, away_team),
           posteam_type = if_else(posteam==home_team, "home", "away"),
           defteam = if_else(posteam == home_team, away_team, home_team),
           week = ceiling(Game/4),
           # Score Columns
           # Sometimes end play score is zero to zero
           EndHomeScore = if_else(EndHomeScore <  total_home_score, total_home_score, EndHomeScore),
           EndAwayScore = if_else(EndAwayScore <  total_away_score, total_away_score, EndAwayScore),
           posteam_score = if_else(posteam == home_team, total_home_score, total_away_score),
           defteam_score = if_else(posteam == away_team, total_home_score, total_away_score),
           posteam_score_post = if_else(posteam == home_team, EndHomeScore, EndAwayScore),
           defteam_score_post = if_else(posteam == away_team, EndHomeScore, EndAwayScore),
           score_differential = posteam_score - defteam_score,
           score_differential_post = posteam_score_post - defteam_score_post,
           pos_score_change = score_differential_post - score_differential,
           pos_score_change = if_else(pos_score_change == 6, 6.5, pos_score_change),
           pos_score_change = if_else(pos_score_change == -6, -6.5, pos_score_change),

           # Create GameIDs in NFL's GSIS format
           game_id = if_else(Game%%4 %in% c(1, 2),
                            str_remove_all(glue("{ymd(20200208) + (week-1)*7}"), "-"),
                            str_remove_all(glue("{ymd(20200209) + (week-1)*7}"), "-")),
           game_id = if_else(Game%%2 == 1,
                            as.character(glue("{game_id}00")),
                            as.character(glue("{game_id}01"))),
           game_id = as.numeric(game_id),
           # GameTime
           quarter_seconds_remaining = as.numeric(str_extract(as.character(time), "[0-9]{2}")) * 60 +
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

           # Is this a kickoff?
           ydstogo = na_if(ydstogo, 0),

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

add_nflscrapR_epa <- function(df){
  library(nflscrapR)
  cat(glue("WARNING: this relies on the nflscrapR model built exclusively on NFL data, not XFL data. When using these \\
            numbers, keep in mind that it will fail to capture differences between the two leagues. This should only be \\
            used until a XFL-specific EPA model is available."))
  df_ep <- nflscrapR::calculate_expected_points(df, "half_seconds_remaining", "yardline_100", "down", "ydstogo", "goal_to_go",
                                                td_value = 6.5) %>%
    mutate(epa = case_when(pos_score_change != 0 ~ pos_score_change - ep,
                           TRUE ~ if_else(posteam == lead(posteam), lead(ep) - ep, -lead(ep) - ep)
                  )
           )
  return(df_ep)
}

raw_pbp_df <- retrive_games(game_id)
clean_pbp <- clean_data(raw_pbp_df)
game_df <- add_nflscrapR_epa(clean_pbp)


URL <- paste0('http://stats.xfl.com/',game_id)
webpage <- read_html(URL)
js_scrip_agg <- html_nodes(webpage, xpath = '//script[@type="text/javascript"]')[3] %>% html_text()
agg_json <- fromJSON(substring(js_scrip, 21, nchar(js_scrip)-6))

away_pts <- agg_json$awayStats$stats$PointsFor
home_pts <- agg_json$homeStats$stats$PointsFor
away_team <- agg_json$awayStats$name
home_team <- agg_json$homeStats$name

score <- htmlTable(data.frame(c(away_team,home_team),c(away_pts,home_pts)),rnames=F,header=c('',''), css.class = 'score')
score <- gsub("style='border-collapse: collapse; margin-top: 1em; margin-bottom: 1em;'","",score)
score <- gsub("style='border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: center;'","",score)
score <- gsub("style='border-bottom: 2px solid grey; text-align: center;'","",score)

total_sec_left <- mins_to_seconds(agg_json$homeStats$stats$TimeOfPossession) + mins_to_seconds(agg_json$awayStats$stats$TimeOfPossession)
curr_qtr <- ((total_sec_left-(total_sec_left %% 900))/900) + 1
curr_sec_qtr <- (curr_qtr * 900) - total_sec_left
curr_qtr_sec <- curr_sec_qtr %% 60
curr_qtr_min <- (curr_sec_qtr - curr_qtr_sec)/60

if (total_sec_left==3600) {
	time_frmt <- 'Final'
} else if (total_sec_left==1800) {
	time_frmt <- 'Halftime'
} else {
	time_frmt <- paste0(ordinal(curr_qtr),' Quarter, ',curr_qtr_min,':',sprintf("%02d", curr_qtr_sec))
}

game_now_html <- paste0(time_frmt, '<br>', score)

game_df$cum_home_epa <- cumsum(ifelse(game_df$posteam==game_df$home_team & game_df$play_type!='punt' & game_df$play_type!='field_goal' & !is.na(game_df$epa), game_df$epa, 0))
game_df$cum_away_epa <- cumsum(ifelse(game_df$posteam!=game_df$home_team & game_df$play_type!='punt' & game_df$play_type!='field_goal' & !is.na(game_df$epa), game_df$epa, 0))

game_df$big_play <- ifelse(abs(game_df$epa)>=2.5 & !is.na(game_df$epa),1,0)
game_df$Graph <- ifelse(game_df$big_play==1,cumsum(game_df$big_play),NA)
game_df$big_play_y_home <- ifelse(game_df$big_play==1 & game_df$posteam==game_df$home_team,game_df$cum_home_epa,NA)
game_df$big_play_y_away <- ifelse(game_df$big_play==1 & game_df$posteam!=game_df$home_team,game_df$cum_away_epa,NA)

tm_col <- c(teams_df$Color1[match(game_df$away_team[1],teams_df$Abbr)],teams_df$Color1[match(game_df$home_team[1],teams_df$Abbr)])
names(tm_col) <- c(game_df$away_team[1],game_df$home_team[1])

ggplot(data = game_df, aes(x = game_seconds_remaining)) +
  geom_hline(yintercept=0, color='grey75',size = 1) +
  geom_line(aes(y = cum_home_epa, color = home_team), size = 1) +
  geom_line(aes(y = cum_away_epa, color = away_team), size = 1) +
  geom_point(aes(y = big_play_y_home, color = home_team), fill = 'white', size = 4, shape = 21, stroke = 1.4, show.legend = F) +
  geom_point(aes(y = big_play_y_away, color = away_team), fill = 'white', size = 4, shape = 21, stroke = 1.4, show.legend = F) +
  geom_text(aes(y = big_play_y_home, color = home_team, label=Graph), show.legend = F, size = 3, family='HP Simplified') +
  geom_text(aes(y = big_play_y_away, color = away_team, label=Graph), show.legend = F, size = 3, family='HP Simplified') +
  scale_color_manual(values=tm_col,name = 'Team') +
  scale_x_reverse(limits = c(3600,0), breaks = seq(3150,0,-900), labels = paste0('Q',1:4), expand = c(0,0)) +
  labs(title='Cumulative Offensive EPA',
       subtitle = 'Excluding Special Teams and PATs',
       y = 'Cumulative EPA') +
  annotate("rect", xmin = 2700, xmax = 1800, ymin = -Inf, ymax = Inf, alpha = .05) +
  annotate("rect", xmin = 900, xmax = 0, ymin = -Inf, ymax = Inf, alpha = .05) +
  theme_bw() +
  theme(
    text = element_text(family='HP Simplified', color='darkblue'),
    plot.background = element_rect(fill = 'grey95', color = 'grey95'),
    panel.border = element_rect(color = 'darkblue'),
    axis.ticks = element_line(color = 'darkblue'),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 8, color = 'darkblue'),
    plot.title = element_text(size = 14),
    plot.subtitle = element_text(size = 8),
    plot.caption = element_text(size = 5)
  ) + 
  theme(
    axis.title.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.x = element_text(size = 10, color = 'darkblue'),
    legend.position = 'bottom',
    legend.title = element_blank(),
    legend.background = element_rect(fill = 'white',color = 'darkblue'),
    legend.key = element_blank()
  )

ggsave('epa-box/cummOff.png', width=5 * (16/9), height=5, dpi = 1000)
cummOff_html <- '<img src = "cummOff.png" class = cummOff>'
game_df$down <- as.numeric(game_df$down)
game_df$qtr <- as.numeric(game_df$qtr)
game_df$EPA <- paste0(ifelse(game_df$epa < 0, game_df$defteam, game_df$posteam), ' ', sprintf("%+.1f",round(abs(game_df$epa),1)))
game_df$Play <- trimws(gsub("\\s*\\([^\\)]+\\)","",game_df$desc))
game_df$DownDistYrd <- paste0(ordinal(game_df$down), ' & ',ifelse(game_df$goal_to_go==1,'GL',game_df$ydstogo), ' at ',ifelse(game_df$yardline_50==50,50,paste0(game_df$side_of_field, ' ', game_df$yardline_50)))
game_df$Situation <- paste0(ifelse(substring(game_df$time,1,1)=='0',substring(game_df$time,2,nchar(game_df$time)),game_df$time), ' ', ordinal(game_df$qtr), '<br>',game_df$DownDistYrd)
game_df$Ball <- paste0(game_df$posteam,ifelse(game_df$play_type=='field_goal',' (FG)', ifelse(game_df$play_type=='punt',' (Punt)','')))
game_df$Score <- paste0(game_df$away_team, ' ', game_df$total_away_score, '<br>',game_df$home_team, ' ', game_df$total_home_score)

big_play_html <- htmlTable(game_df[which(game_df$big_play==1),c('Graph','Situation','Score','Ball','Play','EPA')],rnames=F,css.class = 'big-play')


hm_tm <- ifelse(game_df$posteam_type[1]=='home',game_df$posteam,game_df$defteam)

for (colnm in which(sapply(game_df, typeof)=='integer')) game_df[which(is.na(game_df[,colnm])),colnm] <- 0

game_df$penalty <- ifelse(grepl('penalty',game_df$ShortPlayDescription) | game_df$ShortPlayDescription=='', 1, 0)
game_df$pass_yards_gained <- ifelse(game_df$pass_attempt==1, game_df$yards_gained, 0)
game_df$passer_player_name <- ifelse(game_df$play_type =='pass' & is.na(game_df$passer_player_name), game_df$rusher_player_name, game_df$passer_player_name)
game_df$rush_attempt <- ifelse(game_df$play_type =='run', 1, 0)
game_df$rush_yards_gained <- ifelse(game_df$play_type =='run' | game_df$qb_scramble==1, game_df$yards_gained, 0)
game_df$epa[which(is.na(game_df$epa))] <- 0
game_df$success <- ifelse(game_df$epa>0,1,0)
game_df$pass_touchdown <- ifelse(game_df$pass_attempt==1 & game_df$pos_score_change==6.5, 1, 0)
game_df$rush_touchdown <- ifelse(game_df$rush_attempt==1 & game_df$pos_score_change==6.5, 1, 0)

game_df$down_type <- ifelse(game_df$down < 3, 'Early', 'Late')

team_away_early <- aggregate(cbind('plays'=1,epa,success) ~ play_type, data = game_df, FUN = sum, subset = penalty == 0 & (play_type=='pass' | play_type=='run') & posteam_type=='away' & down_type=='Early', drop = F)
team_away_dwntype <- aggregate(cbind('plays'=1,epa,success) ~ down_type, data = game_df, FUN = sum, subset = penalty == 0 & (play_type=='pass' | play_type=='run') & posteam_type=='away', drop = F)
team_away_tot <- aggregate(cbind('plays'=1,epa,success) ~ posteam_type,data = game_df, FUN = sum, subset = penalty == 0 & (play_type=='pass' | play_type=='run') & posteam_type=='away', drop = F)

names(team_away_dwntype)[1] <- 'posteam_type'
names(team_away_early)[1] <- 'posteam_type'
all_tm_away <- rbind(team_away_early,team_away_dwntype,team_away_tot)

all_tm_away$epa_play <- sprintf("%+.2f",round(all_tm_away$epa/all_tm_away$plays,2))
all_tm_away$succ_rt <- percent(all_tm_away$success/all_tm_away$plays,accuracy = .1)

all_tm_away$posteam_type[which(all_tm_away$posteam_type=='Early')] <- '1st & 2nd Downs'
all_tm_away$posteam_type[which(all_tm_away$posteam_type=='Late')] <- '3rd & 4th Downs'
all_tm_away$posteam_type[which(all_tm_away$posteam_type=='pass')] <- 'Early Down Passes'
all_tm_away$posteam_type[which(all_tm_away$posteam_type=='run')] <- 'Early Down Runs'
all_tm_away$posteam_type[which(all_tm_away$posteam_type=='away')] <- 'All Plays'

team_home_early <- aggregate(cbind('plays'=1,epa,success) ~ play_type, data = game_df, FUN = sum, subset = penalty == 0 & (play_type=='pass' | play_type=='run') & posteam_type=='home' & down_type=='Early', drop = F)
team_home_dwntype <- aggregate(cbind('plays'=1,epa,success) ~ down_type, data = game_df, FUN = sum, subset = penalty == 0 & (play_type=='pass' | play_type=='run') & posteam_type=='home', drop = F)
team_home_tot <- aggregate(cbind('plays'=1,epa,success) ~ posteam_type,data = game_df, FUN = sum, subset = penalty == 0 & (play_type=='pass' | play_type=='run') & posteam_type=='home', drop = F)

names(team_home_dwntype)[1] <- 'posteam_type'
names(team_home_early)[1] <- 'posteam_type'
all_tm_home <- rbind(team_home_early,team_home_dwntype,team_home_tot)

all_tm_home$epa_play <- sprintf("%+.2f",round(all_tm_home$epa/all_tm_home$plays,2))
all_tm_home$succ_rt <- percent(all_tm_home$success/all_tm_home$plays,accuracy = .1)

all_tm_home$posteam_type[which(all_tm_home$posteam_type=='Early')] <- '1st & 2nd Downs'
all_tm_home$posteam_type[which(all_tm_home$posteam_type=='Late')] <- '3rd & 4th Downs'
all_tm_home$posteam_type[which(all_tm_home$posteam_type=='pass')] <- 'Early Down Passes'
all_tm_home$posteam_type[which(all_tm_home$posteam_type=='run')] <- 'Early Down Runs'
all_tm_home$posteam_type[which(all_tm_home$posteam_type=='home')] <- 'All Plays'

full_team_epa <- merge(all_tm_away,all_tm_home, by = 'posteam_type', all.x = T, suffixes = c('_away','_home'))
full_team_epa <- full_team_epa[c(4:5,1:3),c('posteam_type','plays_away','epa_play_away','succ_rt_away','plays_home','epa_play_home','succ_rt_home')]

full_team_epa_html <- htmlTable(full_team_epa, css.class = 'team', rnames = F, cgroup = c('',c(game_df$away_team[1],game_df$home_team[1])), n.cgroup = c(1,3,3), header = c('Play Group',rep(c('Plays','EPA/Play','Success<br>Rate'),2)))
##end team


pass_epa_box <- aggregate(cbind(complete_pass,pass_attempt,pass_yards_gained,pass_touchdown,interception,qb_scramble,rush_yards_gained,sack,epa,success) ~ passer_player_name + posteam, data = game_df, FUN = sum, subset = play_type == 'pass' & penalty == 0)
rush_epa_box <- aggregate(cbind(rush_attempt,rush_yards_gained,rush_touchdown,epa,success) ~ rusher_player_name + posteam, data = game_df, FUN = sum, subset = play_type == 'run' & penalty == 0)
rec_epa_box <- aggregate(cbind(pass_attempt,complete_pass,pass_yards_gained,pass_touchdown,epa,success) ~ receiver_player_name + posteam, data = game_df, FUN = sum, subset = penalty == 0)

pass_epa_box$qb_plays <- pass_epa_box$pass_attempt + pass_epa_box$qb_scramble + pass_epa_box$sack

pass_epa_box <- pass_epa_box[order(-pass_epa_box$qb_plays),]
rec_epa_box <- rec_epa_box[order(-rec_epa_box$pass_attempt),]
rush_epa_box <- rush_epa_box[order(-rush_epa_box$rush_attempt),]

pass_epa_box$'EPA/Ply' <- sprintf("%+.2f",round(pass_epa_box$epa/pass_epa_box$qb_plays,2))
rush_epa_box$'EPA/Ply' <- sprintf("%+.2f",round(rush_epa_box$epa/rush_epa_box$rush_attempt,2))
rec_epa_box$'EPA/Ply' <- sprintf("%+.2f",round(rec_epa_box$epa/rec_epa_box$pass_attempt,2))

pass_epa_box$'Succ%' <- percent(pass_epa_box$success/pass_epa_box$qb_plays,accuracy = 1)
rush_epa_box$'Succ%' <- percent(rush_epa_box$success/rush_epa_box$rush_attempt,accuracy = 1)
rec_epa_box$'Succ%' <- percent(rec_epa_box$success/rec_epa_box$pass_attempt,accuracy = 1)

pass_epa_box$'Cmp/Att' <- paste0(pass_epa_box$complete_pass,'/',pass_epa_box$pass_attempt)
pass_epa_box$'Scrb/Yrds' <- paste0(pass_epa_box$qb_scramble,'/',pass_epa_box$rush_yards_gained)
names(pass_epa_box)[which(names(pass_epa_box)=='passer_player_name')] <- 'Player'
names(pass_epa_box)[which(names(pass_epa_box)=='pass_yards_gained')] <- 'PassYrds'
names(pass_epa_box)[which(names(pass_epa_box)=='interception')] <- 'Int'
names(pass_epa_box)[which(names(pass_epa_box)=='sack')] <- 'Sack'
names(pass_epa_box)[which(names(pass_epa_box)=='pass_touchdown')] <- 'PassTD'

names(rec_epa_box)[which(names(rec_epa_box)=='receiver_player_name')] <- 'Player'
names(rec_epa_box)[which(names(rec_epa_box)=='pass_attempt')] <- 'Tar'
names(rec_epa_box)[which(names(rec_epa_box)=='complete_pass')] <- 'Rec'
names(rec_epa_box)[which(names(rec_epa_box)=='pass_yards_gained')] <- 'RecYrds'
names(rec_epa_box)[which(names(rec_epa_box)=='pass_touchdown')] <- 'RecTD'

names(rush_epa_box)[which(names(rush_epa_box)=='rusher_player_name')] <- 'Player'
names(rush_epa_box)[which(names(rush_epa_box)=='rush_attempt')] <- 'Runs'
names(rush_epa_box)[which(names(rush_epa_box)=='rush_yards_gained')] <- 'RunYrds'
names(rush_epa_box)[which(names(rush_epa_box)=='rush_touchdown')] <- 'RunTD'


pass_away <- htmlTable(pass_epa_box[which(pass_epa_box$posteam!=hm_tm),c('Player','Cmp/Att','PassYrds','PassTD','Scrb/Yrds','Sack','Int','Succ%','EPA/Ply')],rnames=F,css.class = 'sub')
pass_home <- htmlTable(pass_epa_box[which(pass_epa_box$posteam==hm_tm),c('Player','Cmp/Att','PassYrds','PassTD','Scrb/Yrds','Sack','Int','Succ%','EPA/Ply')],rnames=F,css.class = 'sub')

rec_away <- htmlTable(rec_epa_box[which(rec_epa_box$posteam!=hm_tm),c('Player','Tar','Rec','RecYrds','RecTD','Succ%','EPA/Ply')],rnames=F,css.class = 'sub')
rec_home <- htmlTable(rec_epa_box[which(rec_epa_box$posteam==hm_tm),c('Player','Tar','Rec','RecYrds','RecTD','Succ%','EPA/Ply')],rnames=F,css.class = 'sub')

run_away <- htmlTable(rush_epa_box[which(rush_epa_box$posteam!=hm_tm),c('Player','Runs','RunYrds','RunTD','Succ%','EPA/Ply')],rnames=F,css.class = 'sub')
run_home <- htmlTable(rush_epa_box[which(rush_epa_box$posteam==hm_tm),c('Player','Runs','RunYrds','RunTD','Succ%','EPA/Ply')],rnames=F,css.class = 'sub')

master_mx <- matrix(c(pass_away,rec_away,run_away,pass_home,rec_home,run_home),3)
colnames(master_mx) <- c('Away','Home')
master_tbl <- htmlTable(master_mx,rnames=F,css.class = 'master')


css_link <- '<head><link rel="stylesheet" type="text/css" href="box-theme.css?v=2"><link href="//fonts.googleapis.com/css?family=Roboto+Condensed" rel="stylesheet" type="text/css"></head>'

game_container <- paste0('<div class="team-cont">',game_now_html,full_team_epa_html,'</div>')

my_html <- paste0(css_link,game_container,cummOff_html,big_play_html,master_tbl)

write_html(read_html(my_html), 'epa-box/index.html')
webshot('epa-box/index.html', 'epa-box/game.png', useragent = "Mozilla/5.0 (Macintosh; Intel Mac OS X)", vwidth=1600, vheight=900)

