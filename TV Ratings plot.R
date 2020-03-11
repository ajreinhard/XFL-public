library(ggplot2)
library(extrafont)
setwd('C:/Users/rei1740/Desktop/Anthony/XFL')

tv_color_df <- read.csv('network color.csv', stringsAsFactors = F)
tv_ratings_df <- read.csv('TVRatings.csv', stringsAsFactors = F)
tv_ratings_df$Week <- factor(tv_ratings_df$Week)
tv_ratings_df$League <- factor(tv_ratings_df$League, c('XFL (2001)','AAF','XFL (2020)'))

fill_col <- tv_color_df$color1
names(fill_col) <- tv_color_df$Network
stk_col <- tv_color_df$color2
names(stk_col) <- tv_color_df$Network


ggplot(data = tv_ratings_df, aes(x = Week, y = Rating, group = Network)) +
  geom_line(aes(color = Network), size = 1) +
  geom_point(stroke = 1.2, aes(color = Network, fill = Network), size = 2, shape = 21) +
  facet_wrap( ~ League, nrow=1, shrink= F) +
  scale_color_manual(values=fill_col) +
  scale_fill_manual(values=stk_col) +
  scale_x_discrete(drop = F) +
  scale_y_continuous(lim = c(0,10), breaks = seq(0,10,2), expand = c(0,0)) +
  labs(title='Spring Outdoor Football Nielsen Ratings',
       subtitle = 'Average Weekly Rating by Network',
       caption = 'By Anthony Reinhard  |  Data from showbuzzdaily.com and sportsmediawatch.com',
       x = 'Week',
       y = 'Nielsen Rating') +
  theme_bw() +
  theme(
    text = element_text(family='Bahnschrift', color='darkblue'),
    plot.background = element_rect(fill = 'grey95'),
    panel.border = element_rect(color = 'darkblue'),
    axis.ticks = element_line(color = 'darkblue'),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 8, color = 'darkblue'),
    plot.title = element_text(size = 14),
    plot.subtitle = element_text(size = 8),
    plot.caption = element_text(size = 5),
    strip.text.x = element_text(size=12, color='darkblue'),
    strip.background = element_rect(fill = 'grey90', color = 'darkblue'),
    legend.background = element_rect(fill = 'grey90',color = 'darkblue'),
    legend.key = element_blank()
  ) 

ggsave('TV Ratings.png', width = 5 * (16/9), height = 5, dpi = 700)