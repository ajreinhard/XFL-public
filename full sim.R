setwd('C:/Users/Owner/Documents/GitHub/XFL')

#Bovada American Odds in Tm Order
#bovada_odds <- c('DAL'=300,'DC'=700,'HOU'=750,'LA'=550,'NY'=400,'SEA'=1100,'STL'=800,'TB'=500)

odds_df <- read.csv('preseason.csv', stringsAsFactors = F, row.names = 1)
games_df <- read.csv('sched.csv', stringsAsFactors = F)
teams_df <- read.csv('teams.csv', stringsAsFactors = F)

xfl_teams <- row.names(odds_df)

bovada_odds <- apply(odds_df, 1, mean)
bovada_implied_prob <- 100/(bovada_odds+100)
bovada_implied_prob <- bovada_implied_prob/sum(bovada_implied_prob)

K_fct <- 20
home_adv <- 65
reg_szn_gms <- 10
sim_cnt <- 10000

#implied_start <- 1500 + 25 * c('DAL'=1.9,'DC'=-0.5,'HOU'=-0.65,'LA'=0.3,'NY'=1.05,'SEA'=-1.65,'STL'=-0.95,'TB'=0.5)
#(bovada_implied_prob/old_bov)
implied_start <- 1500 + 25 * c('DAL'=1.4,'DC'=0.45,'HOU'=-0.6,'LA'=-0.45,'NY'=0.15,'SEA'=-1.5,'STL'=-0.65,'TB'=1.2)
(sum(implied_start) - 1500*8) / 25
names(implied_start) <- NULL

rate_start <- data.frame('Tm'=sort(teams_df$Abbr), Rate=implied_start, stringsAsFactors = F)
gms_ply_df <- games_df[which(!is.na(games_df$HomeScore)),]
reg_games_rem_df <- games_df[which(is.na(games_df$HomeScore) & games_df$Week <= reg_szn_gms),]


rate_curr <- rate_start
pred_gm_df <- c()
ply_gm_df <- c()

for (wk in unique(gms_ply_df$Week)) {
  this_wk <- gms_ply_df[which(gms_ply_df$Week==wk),]
  this_wk <- merge(merge(this_wk, rate_curr, by.x = 'Home', by.y = 'Tm'),rate_curr, by.x = 'Away', by.y = 'Tm', suffix = c('Home','Away'))
  this_wk$HomeWinProb <- 1/(1+10^(-(this_wk$RateHome - this_wk$RateAway + home_adv)/400))
  this_wk$HomeWin <- ifelse(this_wk$HomeScore > this_wk$AwayScore, 1, 0)
  this_wk$EloChng <-  (this_wk$HomeWin - this_wk$HomeWinProb) * 
    K_fct *
    log(abs(this_wk$HomeScore - this_wk$AwayScore) + 1) * 
    (2.2/(2.2 + (ifelse(this_wk$HomeWin==1,1/1000,-1/1000) * (this_wk$RateHome - this_wk$RateAway + home_adv))))
  this_wk$RateHomeNew <- this_wk$RateHome + this_wk$EloChng
  this_wk$RateAwayNew <- this_wk$RateAway - this_wk$EloChng
  rate_new <- data.frame('Tm'=c(this_wk$Away, this_wk$Home), 'NewRate'=c(this_wk$RateAwayNew, this_wk$RateHomeNew), stringsAsFactors = F)
  rate_curr <- merge(rate_curr, rate_new, by = 'Tm', all.x = T)
  rate_curr$Rate <- ifelse(is.na(rate_curr$NewRate), rate_curr$Rate, rate_curr$NewRate)
  rate_curr$NewRate <- NULL
  ply_gm_df <- rbind(ply_gm_df, this_wk)
}


curr_ratings <- rate_curr$Rate

sim_res <- sapply(1:sim_cnt, function(sm) {
  
  for (wk in unique(reg_games_rem_df$Week)) {
    this_wk <- reg_games_rem_df[which(reg_games_rem_df$Week==wk),]
    this_wk <- merge(merge(this_wk, rate_curr, by.x = 'Home', by.y = 'Tm'),rate_curr, by.x = 'Away', by.y = 'Tm', suffix = c('Home','Away'))
    this_wk$HomeWinProb <- 1/(1+10^(-(this_wk$RateHome - this_wk$RateAway + home_adv)/400))
    this_wk$HomeWin <- ifelse(runif(nrow(this_wk)) < this_wk$HomeWinProb, 1, 0)
    this_wk$EloChng <- (this_wk$HomeWin - this_wk$HomeWinProb) * K_fct
    this_wk$RateHomeNew <- this_wk$RateHome + this_wk$EloChng
    this_wk$RateAwayNew <- this_wk$RateAway - this_wk$EloChng
    rate_new <- data.frame('Tm'=c(this_wk$Away, this_wk$Home), 'NewRate'=c(this_wk$RateAwayNew, this_wk$RateHomeNew), stringsAsFactors = F)
    rate_curr <- merge(rate_curr, rate_new, by = 'Tm', all.x = T)
    rate_curr$Rate <- ifelse(is.na(rate_curr$NewRate), rate_curr$Rate, rate_curr$NewRate)
    rate_curr$NewRate <- NULL
    pred_gm_df <- rbind(pred_gm_df, this_wk)
  }
  
  
  all_gms <- rbind(ply_gm_df, pred_gm_df)
  
  all_gms <- merge(merge(all_gms, teams_df[,c('Abbr', 'Division')], by.x = 'Away', by.y = 'Abbr', all.x = T), teams_df[,c('Abbr', 'Division')], by.x = 'Home', by.y = 'Abbr', all.x = T, suffixes = c('Away','Home'))
  all_gms$DivGame <- ifelse(all_gms$DivisionAway==all_gms$DivisionHome, 'Div', 'Non')
  all_gms$TmWinner <- factor(ifelse(all_gms$HomeWin==1 & all_gms$Week <= 10, all_gms$Home, all_gms$Away), levels = rate_curr$Tm)
  
  rate_curr <- merge(rate_curr, teams_df[,c('Abbr', 'Division')], by.x = 'Tm', by.y = 'Abbr', all.x = T)
  rate_curr$Wins <- table(all_gms$TmWinner)
  rate_curr$DivWins <- table(all_gms$TmWinner[which(all_gms$DivGame=='Div')])
  rate_curr$WPCT <- rate_curr$Wins/reg_szn_gms
  
  all_gms <- merge(merge(all_gms, rate_curr[,c('Tm','WPCT')], by.x = 'Away', by.y = 'Tm', all.x = T), rate_curr[,c('Tm','WPCT')], by.x = 'Home', by.y = 'Tm', all.x = T, suffixes = c('Away','Home'))
  all_gms$LoserWPCT <- ifelse(all_gms$HomeWin==1, all_gms$WPCTAway, all_gms$WPCTHome)
  SOV_calc <- aggregate(LoserWPCT ~ TmWinner, data = all_gms, FUN = mean)
  rate_curr <- merge(rate_curr, SOV_calc, by.x = 'Tm', by.y = 'TmWinner', all.x = T)
  
  h2h_calc <- function(all_gms, tms_vec) {
    all_gms$concat <- paste0(all_gms$Away,'_',all_gms$Home)
    com_gms <- which(apply(sapply(tms_vec, function(x) ifelse(grepl(x, all_gms$concat),1,0)),1,sum)==2)
    com_wins <- table(all_gms$TmWinner[com_gms])
    return(ifelse(is.na(match(tms_vec,names(which(com_wins==max(com_wins))))),0,1))
  }
  
  div_split <- lapply(unique(rate_curr$Division), function(div) {
    div_df <- rate_curr[which(rate_curr$Division==div),]
    tied_wpct <- names(which(table(div_df$WPCT)>1))
    div_df$H2H <- 0
    for (x in tied_wpct) {
      div_df$H2H[which(div_df$WPCT==x)] <- h2h_calc(all_gms, div_df$Tm[which(div_df$WPCT==x)])
    }
    div_df$TB_ord <- div_df$WPCT + div_df$H2H/100 + div_df$DivWins/1000 + div_df$LoserWPCT/1000 + runif(4)/10000000
    div_df <- div_df[order(-div_df$TB_ord),]
    return(div_df)
  })
  
  #semi
  semi_win_prob <- sapply(div_split, function(div) 1/(1+10^(-(div$Rate[1] - div$Rate[2] + home_adv)/400)))
  playoff_hm_win <- ifelse(runif(2) < semi_win_prob, 1, 0)
  
  EloChng_PO <- (playoff_hm_win - semi_win_prob) * K_fct
  PO_chng <- data.frame('Tm'=c(div_split[[1]]$Tm[1:2],div_split[[2]]$Tm[1:2]),'Chng'=c(EloChng_PO[1],-EloChng_PO[1],EloChng_PO[2],-EloChng_PO[2]),stringsAsFactors = F)
  rate_curr <- merge(rate_curr, PO_chng, by = 'Tm', all.x = T)
  rate_curr$Chng[is.na(rate_curr$Chng)] <- 0
  rate_curr$Rate <- rate_curr$Rate + rate_curr$Chng
  rate_curr$Chng <- NULL
  
  #champ
  div1_chmp <- div_split[[1]]$Tm[ifelse(playoff_hm_win[1]==1,1,2)]
  div2_chmp <- div_split[[2]]$Tm[ifelse(playoff_hm_win[2]==1,1,2)]

  #houston home adv
  chmp_hm_adv <- ifelse(div1_chmp=='HOU',65,0)
  div1_chmp_win_prob <- 1/(1+10^(-(rate_curr$Rate[match(div1_chmp, rate_curr$Tm)] - rate_curr$Rate[match(div2_chmp, rate_curr$Tm)] + chmp_hm_adv)/400))
  div1_chmp_win <- ifelse(runif(1) < div1_chmp_win_prob, 1, 0)
  EloChng_chmp <- (div1_chmp_win - div1_chmp_win_prob) * K_fct
  
  chmp_chng <- data.frame('Tm'=c(div1_chmp,div2_chmp),'Chng'=c(EloChng_chmp,-EloChng_chmp),stringsAsFactors = F)
  rate_curr <- merge(rate_curr, chmp_chng, by = 'Tm', all.x = T)
  rate_curr$Chng[is.na(rate_curr$Chng)] <- 0
  rate_curr$Rate <- rate_curr$Rate + rate_curr$Chng
  rate_curr$Chng <- NULL
  
  div_chmp <- c(div_split[[1]]$Tm[1],div_split[[2]]$Tm[1])
  po_loss <- PO_chng$Tm[which(PO_chng$Chng<0)]
  runner_up <- ifelse(div1_chmp_win==0,div1_chmp,div2_chmp)
  champ <- ifelse(div1_chmp_win==1,div1_chmp,div2_chmp)
  
  return(c(rate_curr$Rate, rate_curr$Wins, div_chmp, po_loss, runner_up, champ))

})


sim_res_df <- data.frame(t(sim_res), stringsAsFactors = F)
for (i in 1:16) sim_res_df[,i] <- as.numeric(sim_res_df[,i])
xfl_tms <- sort(rate_start$Tm)
names(sim_res_df) <- c(paste0(xfl_tms,'_Rating'),paste0(xfl_tms,'_Wins'),'WEST_REG_CHAMP','EAST_REG_CHAMP','WEST_RU','EAST_RU','XFL_RU','XFL_CHAMP')

div_champ_odds <- table(factor(unlist(sim_res_df[,c('WEST_REG_CHAMP','EAST_REG_CHAMP')]), levels = xfl_tms))/sim_cnt
po_odds <- table(factor(unlist(sim_res_df[,c('WEST_RU','EAST_RU','XFL_RU','XFL_CHAMP')]), levels = xfl_tms))/sim_cnt
final_odds <- table(factor(unlist(sim_res_df[,c('XFL_RU','XFL_CHAMP')]), levels = xfl_tms))/sim_cnt
champ_odds <- table(factor(sim_res_df$XFL_CHAMP, levels = xfl_tms))/sim_cnt
wins_avg <- apply(sim_res_df[,paste0(xfl_tms,'_Wins')],2,mean)
rate_avg <- apply(sim_res_df[,paste0(xfl_tms,'_Rating')],2,mean)

sim_final_df <- data.frame(cbind(po_odds,div_champ_odds,final_odds,champ_odds,rate_avg,wins_avg),stringsAsFactors = F)

if (length(ply_gm_df)!=0) {
winners <- ifelse(ply_gm_df$HomeWin==1,ply_gm_df$Home,ply_gm_df$Away)
losers <- ifelse(ply_gm_df$HomeWin==0,ply_gm_df$Home,ply_gm_df$Away)
} else {
winners <- NA
losers <- NA
}

sim_final_df$vegas_implied <- bovada_implied_prob
sim_final_df$pre_season_ratings <- implied_start
sim_final_df$current_ratings <- curr_ratings
sim_final_df$wins <- table(factor(winners, xfl_teams))
sim_final_df$losses <- table(factor(losers, xfl_teams))

write.csv(sim_final_df, 'sim results.csv')
write.csv(sim_res_df,'sim_res.csv',row.names=F)


#plot(sim_final_df$vegas_implied, sim_final_df$champ_odds, xlim = c(0,.3), ylim = c(0,.3), type='n') +
#  lines(c(0,.3),c(0,.3)) +
#  text(sim_final_df$vegas_implied, sim_final_df$champ_odd, row.names(sim_final_df), cex = .5)


#all_gms[grepl('TB', all_gms$concat),]
#sample(c('aj','ty','dev','jake','seth','cory'),size=6, replace = FALSE)
