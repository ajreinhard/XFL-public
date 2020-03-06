library(scales)
library(htmlTable)
library(xml2)
setwd('C:/Users/Owner/Documents/GitHub/XFL')

pbp_df <- read.csv('epa/scraped PBP.csv', stringsAsFactors=F)

#pass_mean <- mean(pbp_df$epa[which(pbp_df$play_type == 'pass')], na.rm = T)
#pass_sd <- sd(pbp_df$epa[which(pbp_df$play_type == 'pass')], na.rm = T)
#pnorm(8.11,pass_mean * 32, pass_sd * sqrt(32))

game_id <- 6
game_df <- pbp_df[which(pbp_df$Game==game_id),]

hm_tm <- ifelse(game_df$posteam_type[1]=='home',game_df$posteam,game_df$defteam)

for (colnm in which(sapply(game_df, typeof)=='integer')) game_df[which(is.na(game_df[,colnm])),colnm] <- 0

game_df$penalty <- ifelse(grepl('penalty',game_df$ShortPlayDescription) | game_df$ShortPlayDescription=='', 1, 0)
game_df$pass_yards_gained <- ifelse(game_df$pass_attempt==1, game_df$yards_gained, 0)
game_df$passer_player_name <- ifelse(game_df$play_type =='pass' & is.na(game_df$passer_player_name), game_df$rusher_player_name, game_df$passer_player_name)
game_df$rush_attempt <- ifelse(game_df$play_type =='run', 1, 0)
game_df$rush_yards_gained <- ifelse(game_df$play_type =='run' | game_df$qb_scramble==1, game_df$yards_gained, 0)
game_df$epa[which(is.na(game_df$epa))] <- 0
game_df$success <- ifelse(game_df$epa>0,1,0)

pass_epa_box <- aggregate(cbind(complete_pass,pass_attempt,pass_yards_gained,interception,qb_scramble,rush_yards_gained,sack,epa,success) ~ passer_player_name + posteam, data = game_df, FUN = sum, subset = play_type == 'pass' & penalty == 0)
rush_epa_box <- aggregate(cbind(rush_attempt,rush_yards_gained,epa,success) ~ rusher_player_name + posteam, data = game_df, FUN = sum, subset = play_type == 'run' & penalty == 0)
rec_epa_box <- aggregate(cbind(pass_attempt,complete_pass,pass_yards_gained,epa,success) ~ receiver_player_name + posteam, data = game_df, FUN = sum, subset = penalty == 0)

pass_epa_box$qb_plays <- pass_epa_box$pass_attempt + pass_epa_box$qb_scramble + pass_epa_box$qb_scramble

pass_epa_box <- pass_epa_box[order(-pass_epa_box$qb_plays),]
rec_epa_box <- rec_epa_box[order(-rec_epa_box$pass_attempt),]
rush_epa_box <- rush_epa_box[order(-rush_epa_box$rush_attempt),]

pass_epa_box$'EPA/Pl' <- sprintf("%+.2f",round(pass_epa_box$epa/pass_epa_box$qb_plays,2))
rush_epa_box$'EPA/Pl' <- sprintf("%+.2f",round(rush_epa_box$epa/rush_epa_box$rush_attempt,2))
rec_epa_box$'EPA/Pl' <- sprintf("%+.2f",round(rec_epa_box$epa/rec_epa_box$pass_attempt,2))

pass_epa_box$'Succ%' <- percent(pass_epa_box$success/pass_epa_box$qb_plays,accuracy = 1)
rush_epa_box$'Succ%' <- percent(rush_epa_box$success/rush_epa_box$rush_attempt,accuracy = 1)
rec_epa_box$'Succ%' <- percent(rec_epa_box$success/rec_epa_box$pass_attempt,accuracy = 1)

pass_epa_box$'Cmp/Att' <- paste0(pass_epa_box$complete_pass,'/',pass_epa_box$pass_attempt)
pass_epa_box$'Scrb/Yrds' <- paste0(pass_epa_box$qb_scramble,'/',pass_epa_box$rush_yards_gained)
names(pass_epa_box)[which(names(pass_epa_box)=='passer_player_name')] <- 'Player'
names(pass_epa_box)[which(names(pass_epa_box)=='pass_yards_gained')] <- 'PassYrds'
names(pass_epa_box)[which(names(pass_epa_box)=='interception')] <- 'Int'
names(pass_epa_box)[which(names(pass_epa_box)=='sack')] <- 'Sack'

names(rec_epa_box)[which(names(rec_epa_box)=='receiver_player_name')] <- 'Player'
names(rec_epa_box)[which(names(rec_epa_box)=='pass_attempt')] <- 'Tar'
names(rec_epa_box)[which(names(rec_epa_box)=='complete_pass')] <- 'Rec'
names(rec_epa_box)[which(names(rec_epa_box)=='pass_yards_gained')] <- 'RecYrds'

names(rush_epa_box)[which(names(rush_epa_box)=='rusher_player_name')] <- 'Player'
names(rush_epa_box)[which(names(rush_epa_box)=='rush_attempt')] <- 'Runs'
names(rush_epa_box)[which(names(rush_epa_box)=='rush_yards_gained')] <- 'RunYrds'


pass_away <- htmlTable(pass_epa_box[which(pass_epa_box$posteam!=hm_tm),c('Player','Cmp/Att','PassYrds','Scrb/Yrds','Sack','Int','Succ%','EPA/Pl')],rnames=F,css.class = 'sub')
pass_home <- htmlTable(pass_epa_box[which(pass_epa_box$posteam==hm_tm),c('Player','Cmp/Att','PassYrds','Scrb/Yrds','Sack','Int','Succ%','EPA/Pl')],rnames=F,css.class = 'sub')

rec_away <- htmlTable(rec_epa_box[which(rec_epa_box$posteam!=hm_tm),c('Player','Tar','Rec','RecYrds','Succ%','EPA/Pl')],rnames=F,css.class = 'sub')
rec_home <- htmlTable(rec_epa_box[which(rec_epa_box$posteam==hm_tm),c('Player','Tar','Rec','RecYrds','Succ%','EPA/Pl')],rnames=F,css.class = 'sub')

run_away <- htmlTable(rush_epa_box[which(rush_epa_box$posteam!=hm_tm),c('Player','Runs','RunYrds','Succ%','EPA/Pl')],rnames=F,css.class = 'sub')
run_home <- htmlTable(rush_epa_box[which(rush_epa_box$posteam==hm_tm),c('Player','Runs','RunYrds','Succ%','EPA/Pl')],rnames=F,css.class = 'sub')

master_mx <- matrix(c(pass_away,rec_away,run_away,pass_home,rec_home,run_home),3)
colnames(master_mx) <- c('Away','Home')
master_tbl <- htmlTable(master_mx,rnames=F,css.class = 'master')


css_link <- '<head><link rel="stylesheet" type="text/css" href="box-theme.css?v=2"></head>'

#my_html <- paste0(css_link,pass_away,pass_home,rec_away,rec_home)
my_html <- paste0(css_link,master_tbl)


write_html(read_html(my_html), 'epa-box/index.html')

