library(rvest)
library(RJSONIO)
library(nflscrapR)

setwd('C:/Users/Owner/Documents/GitHub/XFL/epa')

#####PICK UP EACH GAME
#####CALCULATE EPA
for (i in 1:8) {

URL <- paste0('http://stats.xfl.com/',i)
webpage <- read_html(URL)

js_scrip <- html_nodes(webpage, xpath = '//script[@type="text/javascript"]')[6] %>% html_text()
pbp_json <- fromJSON(substring(js_scrip, 21, nchar(js_scrip)-6))

pbp_df <- read.csv(paste0('pre-epa/',i,'.csv'), stringsAsFactors=F)

pbp_df$DefTeam <- ifelse(pbp_df$ClubCode==unique(pbp_df$ClubCode)[2],unique(pbp_df$ClubCode)[1],unique(pbp_df$ClubCode)[2])

pbp_df$OffensePts <- ifelse(pbp_json$homeClubCode==pbp_df$ClubCode, (pbp_df$EndHomeScore - pbp_df$EndAwayScore) - (pbp_df$StartHomeScore - pbp_df$StartAwayScore), (pbp_df$EndAwayScore - pbp_df$EndHomeScore) - (pbp_df$StartAwayScore - pbp_df$StartHomeScore))
pbp_df$Yardline100 <- abs(ifelse(sapply(strsplit(pbp_df$Yardline,' '), function(x) x[1])==pbp_df$ClubCode,100,0) - sapply(strsplit(pbp_df$Yardline,' '), function(x) as.numeric(x[2])))
pbp_df$GoalToGo <- ifelse(pbp_df$Yardline100==pbp_df$Distance,1,0)
qtr_secs <- sapply(strsplit(pbp_df$StartTime,':'), function(x) as.numeric(x[1])) * 60 + sapply(strsplit(pbp_df$StartTime,':'), function(x) as.numeric(x[2]))
pbp_df$SecondsLeftInHalfStart <- ifelse(pbp_df$Quarter %% 2 == 0, qtr_secs, qtr_secs + 900)
pbp_df$YardsGained <- as.numeric(gsub("([0-9]+).*$", "\\1", pbp_df$ShortPlayDescription))
pbp_df$YardsGained <- ifelse(is.na(pbp_df$YardsGained), 0, pbp_df$YardsGained)
pbp_df$YardsGained[which(!pbp_df$PlayType %in% c('Pass','Rush','Penalty'))] <- 0


pbp_df$Yardline100[which(pbp_df$PlayType=='Kick off')] <- 68
pbp_df$Down[which(pbp_df$PlayType=='Kick off')] <- 1
pbp_df$Distance[which(pbp_df$PlayType=='Kick off')] <- 10

pbp_df$OffensePts[which(pbp_df$OffensePts==6)] <- 6.5
pbp_df$OffensePts[which(pbp_df$OffensePts==-6)] <- -6.5

pbp_df$RushAttempt <- ifelse(grepl('rush',pbp_df$ShortPlayDescription),1,0)
pbp_df$PassAttempt <- ifelse(grepl('pass',pbp_df$ShortPlayDescription),1,0)
pbp_df$CompletePass <- ifelse(grepl('incomplete',pbp_df$ShortPlayDescription, ignore.case = T) & pbp_df$PassAttempt==1,0,1)

pbp_df$Interception <- ifelse(grepl('Intercept',pbp_df$ShortPlayDescription),1,0)
pbp_df$PassAttempt <- pbp_df$PassAttempt + pbp_df$Interception
pbp_df$Penalty <- ifelse(grepl('Penalty',pbp_df$ShortPlayDescription, ignore.case = T),1,0)
pbp_df$Sack <- ifelse(grepl('sack',pbp_df$ShortPlayDescription),1,0)
pbp_df$Scramble <- ifelse(grepl('scramble',pbp_df$PlayDescription),1,0)
pbp_df$QBSpike <- ifelse(grepl(' spiked ',pbp_df$PlayDescription),1,0)
pbp_df$QBKneel <- ifelse(grepl(' kneels ',pbp_df$PlayDescription),1,0)
pbp_df$PAT <- as.numeric(ifelse(grepl('pt attempt',pbp_df$PlayDescription),substr(pbp_df$PlayDescription,1,1),0))
pbp_df$PATConverted <- ifelse(pbp_df$PAT==0,NA,ifelse(pbp_df$OffensePts==0,0,1))

first_parenth <- sapply(regmatches(pbp_df$PlayDescription, gregexpr("(?<=\\().*?(?=\\))", pbp_df$PlayDescription, perl=T)), function(x) x[1])
first_parenth[which(substring(pbp_df$PlayDescription,1,1)!='(')] <- NA
pbp_df$PlayFormation <- first_parenth

sec_parenth <- sapply(regmatches(pbp_df$PlayDescription, gregexpr("(?<=\\().*?(?=\\))", pbp_df$PlayDescription, perl=T)), function(x) rev(x)[1])
sec_parenth[which(sapply(regmatches(pbp_df$PlayDescription, gregexpr("(?<=\\().*?(?=\\))", pbp_df$PlayDescription, perl=T)), length)==1)] <- NA

pbp_df$TacklerName1 <- sapply(strsplit(gsub(', ','; ',sec_parenth), '; '), function(x) x[1])
pbp_df$TacklerName2 <- sapply(strsplit(gsub(', ','; ',sec_parenth), '; '), function(x) x[2])

begin_play_text <- ifelse(is.na(first_parenth),1,regexpr(')',pbp_df$PlayDescription)+2)
end_pass_text <- apply(cbind(regexpr(' pass ', pbp_df$PlayDescription),regexpr(' sacked ', pbp_df$PlayDescription)),1,max)
end_rush_text <- apply(cbind(regexpr(' rush ', pbp_df$PlayDescription),regexpr(' scrambles ', pbp_df$PlayDescription)),1,max)

pbp_df$PasserName <- substring(pbp_df$PlayDescription,begin_play_text, end_pass_text-1)
pbp_df$PasserName[which(pbp_df$PasserName=='')] <- NA
pbp_df$RusherName <- substring(pbp_df$PlayDescription,begin_play_text, end_rush_text-1)
pbp_df$RusherName [which(pbp_df$RusherName =='')] <- NA
TargetName_toEnd <- substring(pbp_df$PlayDescription,regexpr(' to ', pbp_df$PlayDescription)+4, nchar(pbp_df$PlayDescription)-1)
pbp_df$TargetName <- substring(TargetName_toEnd, 1, regexpr(' ',TargetName_toEnd)-1)
pbp_df$TargetName[which(pbp_df$PassAttempt==0)] <- NA
pbp_df$Distance[which(pbp_df$Distance==0)] <- NA

pbp_df <- calculate_expected_points(pbp_df
					,half_seconds_remaining='SecondsLeftInHalfStart'
					,yard_line_100='Yardline100'
					,down='Down'
					,yards_to_go='Distance'
					,goal_to_go='GoalToGo'
					,td_value = 6.5)

pbp_df$ep <- ifelse(pbp_json$homeClubCode==pbp_df$ClubCode, 1, -1) * pbp_df$ep
pbp_df$epa <- NA
pbp_df$epa[which(!is.na(pbp_df$ep))] <- diff(c(pbp_df$ep[which(!is.na(pbp_df$ep))],0))
pbp_df$epa <- ifelse(pbp_json$homeClubCode==pbp_df$ClubCode, 1, -1) * pbp_df$epa
pbp_df$ep <- ifelse(pbp_json$homeClubCode==pbp_df$ClubCode, 1, -1) * pbp_df$ep

pbp_df$epa <- ifelse(pbp_df$OffensePts==0,pbp_df$epa, pbp_df$OffensePts - pbp_df$ep)

write.csv(pbp_df, paste0('post-epa/',i,'.csv'), row.names = F)
}

all_pbp <- do.call(rbind, lapply(dir('post-epa',full=T), function(i) read.csv(i, stringsAsFactors=F)))
write.csv(all_pbp,'all_pbp.csv', row.names = F)








#aggregate(cbind(RushAttempt,PassAttempt,YardsGained,epa) ~ ClubCode + PlayType, data = pbp_df, FUN = sum)





11*60+54

temp_df <- data.frame('SecondsLeftInHalfStart'=743
					,'Yardline100'=49
					,'Down'=2
					,'Distance'=2
					,'GoalToGo'=0)

calculate_expected_points(temp_df
					,half_seconds_remaining='SecondsLeftInHalfStart'
					,yard_line_100='Yardline100'
					,down='Down'
					,yards_to_go='Distance'
					,goal_to_go='GoalToGo'
					,td_value = 6.5)






aggregate(cbind(RushAttempt,PassAttempt,YardsGained,epa) ~ ClubCode + PlayType, data = pbp_df, FUN = sum)



pbp_df$

?calculate_expected_points




str(pbp_df)



pbp_df$IsScoringPlay


sum(pbp_df$PassAttempt)

aggregate(cbind(RushAttempt,PassAttempt,YardsGained,epa) ~ ClubCode + PlayType, data = pbp_df, FUN = sum)

table(pbp_df$PlayType)




60*15

ifelse()


str(pbp_df)

