setwd('C:/Users/Owner/Documents/GitHub/XFL')
sim_res_df <- read.csv('sim_res.csv',stringsAsFactors=F)

table(apply(sim_res_df[,grep('_Wins',names(sim_res_df))],1,max))/nrow(sim_res_df)
table(apply(sim_res_df[,grep('_Wins',names(sim_res_df))],1,min))/nrow(sim_res_df)

xfl_teams <- gsub('_Wins','',names(sim_res_df)[grep('_Wins',names(sim_res_df))])

west_ru_wins <- sapply(1:nrow(sim_res_df), function(x) sim_res_df[x,grep('_Wins',names(sim_res_df))[match(sim_res_df$WEST_RU, xfl_teams)[x]]])
east_ru_wins <- sapply(1:nrow(sim_res_df), function(x) sim_res_df[x,grep('_Wins',names(sim_res_df))[match(sim_res_df$EAST_RU, xfl_teams)[x]]])
xfl_ru_wins <- sapply(1:nrow(sim_res_df), function(x) sim_res_df[x,grep('_Wins',names(sim_res_df))[match(sim_res_df$XFL_RU, xfl_teams)[x]]])
xfl_champ_wins <- sapply(1:nrow(sim_res_df), function(x) sim_res_df[x,grep('_Wins',names(sim_res_df))[match(sim_res_df$XFL_CHAMP, xfl_teams)[x]]])

sim_res_df[2019,]
which(east_ru_wins==2)

table(apply(cbind(west_ru_wins,east_ru_wins,xfl_ru_wins,xfl_champ_wins),1,min))

head(sim_res_df)
west_chmp_wins[1]


made_po_mx <- t(apply(sim_res_df[,c('EAST_RU','WEST_RU','XFL_RU','XFL_CHAMP')],1,function(x) !is.na(match(xfl_teams, x))))
missed_po <- table(factor(unlist(ifelse(made_po_mx,NA,1)*sim_res_df[,grep('_Wins',names(sim_res_df))]),0:10))
made_po <- table(factor(unlist(ifelse(made_po_mx,1,NA)*sim_res_df[,grep('_Wins',names(sim_res_df))]),0:10))
cbind(made_po/(made_po + missed_po))


(summary(apply(sim_res_df[,grep('_Rating',names(sim_res_df))],1,max)) - 1500)/25
which(apply(sim_res_df[,grep('_Rating',names(sim_res_df))],1,max)>=1627)
sim_res_df[1430,]
plot(table(sim_res_df$TB_Wins))/nrow(sim_res_df)


145/(145+100)