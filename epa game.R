
pbp2$passer_player_name
table(is.na(pbp2$qb_kneel))
pbp2$qb_spike
pbp2$success <- ifelse(pbp2$epa>0,1,0)
pbp2$passerrusher <- ifelse(is.na(pbp2$passer_player_name),pbp2$rusher_player_name,pbp2$passer_player_name)
pbp2$epa2 <- ifelse(pbp2$epa < -4.5,-4.5,pbp2$epa)


aggregate(epa ~ posteam,data = pbp2, FUN = mean, subset = (play_type=='dropback' | play_type=='run') & (is.na(qb_kneel) |  qb_kneel==0) & (is.na(qb_spike) |  qb_spike==0))
aggregate(epa2 ~ passerrusher,data = pbp2, FUN = mean, subset = (play_type=='dropback' | play_type=='run') & (is.na(qb_kneel) |  qb_kneel==0) & (is.na(qb_spike) |  qb_spike==0))



def_epa <- merge(aggregate(cbind('plays'=1,epa,success) ~ defteam, data = pbp2, FUN = sum, subset = play_type=='dropback')
      ,aggregate(cbind('plays'=1,epa,success) ~ defteam,data = pbp2, FUN = sum, subset = play_type=='run')
      , by = 'defteam', suffixes = c('Pass','Run'))


def_epa$'Defensive EPA / Play' <- (def_epa$epaPass + def_epa$epaRun) / (def_epa$playsPass + def_epa$playsRun)
def_epa$'Defensive Success Rate' <- (def_epa$successPass + def_epa$successRun) / (def_epa$playsPass + def_epa$playsRun)
def_epa$'Pass EPA / Play' <- def_epa$epaPass / def_epa$playsPass
def_epa$'Rush EPA / Play' <- def_epa$epaRun / def_epa$playsRun
def_epa$'Pass Success Rate' <- def_epa$successPass / def_epa$playsPass
def_epa$'Rush Success Rate' <- def_epa$successRun / def_epa$playsRun


off_epa <- merge(aggregate(cbind('plays'=1,epa,success) ~ posteam, data = pbp2, FUN = sum, subset = play_type=='dropback')
                 ,aggregate(cbind('plays'=1,epa,success) ~ posteam,data = pbp2, FUN = sum, subset = play_type=='run')
                 , by = 'posteam', suffixes = c('Pass','Run'))

off_epa$'Pass Ratio' <- off_epa$playsPass / (off_epa$playsPass + off_epa$playsRun)
off_epa$'Offensive EPA / Play' <- (off_epa$epaPass + off_epa$epaRun) / (off_epa$playsPass + off_epa$playsRun)
off_epa$'Offensive Success Rate' <- (off_epa$successPass + off_epa$successRun) / (off_epa$playsPass + off_epa$playsRun)
off_epa$'Pass EPA / Play' <- off_epa$epaPass / off_epa$playsPass
off_epa$'Rush EPA / Play' <- off_epa$epaRun / off_epa$playsRun
off_epa$'Pass Success Rate' <- off_epa$successPass / off_epa$playsPass
off_epa$'Rush Success Rate' <- off_epa$successRun / off_epa$playsRun

cbind(off_epa$posteam,off_epa$'Offensive EPA / Play',def_epa$'Defensive EPA / Play')
