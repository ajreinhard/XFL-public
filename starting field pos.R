library(ggplot2)
library(ggimage)
library(extrafont)
setwd('C:/Users/Owner/Documents/GitHub/XFL')

pbp_df <- read.csv('https://github.com/ajreinhard/xfl-public/raw/master/epa/all_pbp.csv', stringsAsFactors=F)
write.csv(pbp_df, 'reg_pbp_2020.csv', row.names = F)

#pbp_df <- do.call(rbind,lapply(1:8, function(x) read.csv(url(paste0('https://github.com/ajreinhard/xfl-public/raw/master/epa/post-epa/',x,'.csv')),stringsAsFactors = F)))


teams_df <- read.csv('https://github.com/ajreinhard/xfl-public/raw/master/teams.csv', stringsAsFactors=F)
teams_df$UseColor <- teams_df$Helm
teams_df$UseColor[4] <- teams_df$Color3[4]


drive_playID <- pbp_df$PlayID[which(pbp_df$PlayType!='Kick off')][match(unique(pbp_df$DriveID),pbp_df$DriveID[which(pbp_df$PlayType!='Kick off')])]

drive_begin_df <- pbp_df[which(pbp_df$PlayID %in% drive_playID),]
drive_begin_df <- drive_begin_df[order(-drive_begin_df$Yardline100),]
med_drive <- floor(table(drive_begin_df$ClubCode)/2)

drive_begin_df$TmDrvOrd <- sapply(1:nrow(drive_begin_df), function(x) length(which(drive_begin_df$ClubCode[1:x]==drive_begin_df$ClubCode[x])))

drive_begin_df$IsMedian <- 0
drive_begin_df$IsMedian[match(paste0(names(med_drive),med_drive),paste0(drive_begin_df$ClubCode,drive_begin_df$TmDrvOrd))] <- 1
#drive_begin_df[match(paste0(names(med_drive),med_drive),paste0(drive_begin_df$ClubCode,drive_begin_df$TmDrvOrd)),]

drive_begin_df$TeamLogoLink <- ifelse(drive_begin_df$IsMedian==1, paste0('https://github.com/ajreinhard/xfl-public/raw/master/logos/',tolower(drive_begin_df$ClubCode),'.png'),NA)
#drive_begin_df$TeamLogoLink <- ifelse(drive_begin_df$IsMedian==1, 'hou.png',NA)

tm_colors <- teams_df$UseColor
names(tm_colors) <- teams_df$Abbr

tm_ord <- drive_begin_df$ClubCode[which(drive_begin_df$IsMedian==1)]
drive_begin_df$ClubCode <- factor(drive_begin_df$ClubCode, tm_ord)

summary(drive_begin_df$Yardline100)

ggplot(data = drive_begin_df, aes(x=ClubCode, y=Yardline100)) +
  geom_jitter(aes(color = ClubCode), position=position_jitter(width=0.2), alpha=0.8) + 
  geom_image(aes(image = TeamLogoLink), size = 0.04, by="height") +
  scale_colour_manual(values=tm_colors) +
  scale_y_reverse(limits= c(100,0)) +
  labs(title='XFL Starting Field Position',
       caption = 'By Anthony Reinhard\nData from XFL.com',
       subtitle='Through Week 2 of 2020',
       y = 'Yards from End Zone to Start Drive') +
  theme_bw() +
  theme(
    text = element_text(family='HP Simplified', color='darkblue'),
    plot.background = element_rect(fill = 'grey95'),
    panel.border = element_rect(color = 'darkblue'),
    axis.ticks = element_line(color = 'darkblue'),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 8, color = 'darkblue'),
    plot.title = element_text(size = 14),
    plot.subtitle = element_text(size = 8),
    plot.caption = element_text(size = 5)
  ) +
  theme(legend.position = "none"
        ,axis.title.x=element_blank())

ggsave('XFL Field Pos.png', width=5, height=5)

#med_fld_pos <- aggregate(Yardline100 ~ ClubCode, data = drive_begin_df, FUN=median)

