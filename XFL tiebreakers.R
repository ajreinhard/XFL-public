

all_gms$tot_pts_sim <- rnorm(40,41,12)
all_gms$tot_pts_sim <- ifelse(all_gms$tot_pts_sim < all_gms$WinnerMargin, all_gms$WinnerMargin, all_gms$tot_pts_sim)
all_gms$WinnerPF_sim <- ceiling(all_gms$WinnerMargin/2 + all_gms$tot_pts_sim/2)
all_gms$LoserPF_sim <- floor(all_gms$tot_pts_sim - all_gms$WinnerPF_sim)

all_gms$WinnerPF <- ifelse(is.na(all_gms$HomeScore), all_gms$WinnerPF_sim, ifelse(all_gms$HomeWin==1, all_gms$HomeScore, all_gms$AwayScore))
all_gms$LoserPF <- ifelse(is.na(all_gms$HomeScore), all_gms$LoserPF_sim, ifelse(all_gms$HomeWin==0, all_gms$HomeScore, all_gms$AwayScore))
       
#mean(all_gms$AwayScore + all_gms$HomeScore, na.rm = T)
#sd(all_gms$AwayScore + all_gms$HomeScore, na.rm = T)


h2h_tie <- function(all_gms, tms_vec) {
  all_gms$concat <- paste0(all_gms$Away,'_',all_gms$Home)
  com_gms <- which(apply(sapply(tms_vec, function(x) ifelse(grepl(x, all_gms$concat),1,0)),1,sum)==2)
  com_wins <- table(all_gms$TmWinner[com_gms])
  return(names(which(com_wins==max(com_wins))))
}

net_TD_mar_tie <- function(all_gms, tms_vec) {
  TD_mar_df <- data.frame('Team'=c(as.character(all_gms$TmWinner),as.character(all_gms$TmLoser)), 'TD_Margin'=c(all_gms$TD_margin,-all_gms$TD_margin), stringsAsFactors = F)
  TD_mar_agg_df <- aggregate(TD_Margin ~ Team, data = TD_mar_df, FUN = sum)
  tm_TD_mar <- TD_mar_agg_df$TD_Margin[match(tms_vec,TD_mar_agg_df$Team)]
  return(tms_vec[which(tm_TD_mar==max(tm_TD_mar))])
}

h2h_PF_tie <- function(all_gms, tms_vec) {
  all_gms$concat <- paste0(all_gms$Away,'_',all_gms$Home)
  tot_pts_df <- data.frame('concat'=rep(all_gms$concat,2),'Team'=c(as.character(all_gms$TmWinner),as.character(all_gms$TmLoser)), 'Point_For'=c(all_gms$WinnerPF,all_gms$LoserPF), stringsAsFactors = F)
  com_gms <- which(apply(sapply(tms_vec, function(x) ifelse(grepl(x, tot_pts_df$concat),1,0)),1,sum)==2)
  tot_pts_agg_df <- aggregate(Point_For ~ Team, data = tot_pts_df[com_gms,], FUN = sum)
  tm_tot_pts <- tot_pts_agg_df$Point_For[match(tms_vec,tot_pts_agg_df$Team)]
  return(tms_vec[which(tm_tot_pts==max(tm_tot_pts))])
}

div_win_tie <- function(all_gms, tms_vec) {
  div_wins <- table(all_gms$TmWinner[which(all_gms$DivGame=='Div')])
  div_wins_tm <- div_wins[which(names(div_wins) %in% tms_vec)]
  return(names(which(div_wins_tm==max(div_wins_tm))))
}

SOV_tie <- function(all_gms, tms_vec) {
  SOV_df <- aggregate(LoserWPCT ~ TmWinner, data = all_gms, FUN = mean)
  tm_SOV <- SOV_df$LoserWPCT[match(tms_vec,SOV_df$TmWinner)]
  return(tms_vec[which(tm_SOV==max(tm_SOV))])
}

SOV_tie <- function(all_gms, tms_vec) {
  SOV_df <- aggregate(LoserWPCT ~ TmWinner, data = all_gms, FUN = mean)
  tm_SOV <- SOV_df$LoserWPCT[match(tms_vec,SOV_df$TmWinner)]
  return(tms_vec[which(tm_SOV==max(tm_SOV))])
}

net_Pts_div_rnk_mar_tie <- function(all_gms, tms_vec) {
  tot_pts_df <- data.frame('Team'=c(as.character(all_gms$TmWinner),as.character(all_gms$TmLoser)), 'TD_Margin'=c(all_gms$WinnerMargin,-all_gms$WinnerMargin), stringsAsFactors = F)
  TD_mar_agg_df <- aggregate(TD_Margin ~ Team, data = tot_pts_df, FUN = sum)
  tm_TD_mar <- TD_mar_agg_df$TD_Margin[match(tms_vec,TD_mar_agg_df$Team)]
  return(tms_vec[which(tm_TD_mar==max(tm_TD_mar))])
}


all_gms



div_wins <- table(all_gms$TmWinner[which(all_gms$DivGame=='Div')])
div_wins[which(names(div_wins) %in% tms_vec)]




h2h_calc(all_gms, c('HOU','STL'))
net_TD_mar_tie(all_gms, c('NY','STL'))
tms_vec <- c('NY','STL')
SOV_tie(all_gms, c('NY','STL'))


tot_pts_df[com_gms,]
