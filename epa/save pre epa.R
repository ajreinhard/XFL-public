library(rvest)
library(RJSONIO)

setwd('C:/Users/Owner/Documents/GitHub/XFL/epa')

#####SAVE RAW DATA
#####MAKE SOME MANUAL EDITS AFTER
for (i in 9:12) {
URL <- paste0('http://stats.xfl.com/',i)
webpage <- read_html(URL)

js_scrip <- html_nodes(webpage, xpath = '//script[@type="text/javascript"]')[6] %>% html_text()
pbp_json <- fromJSON(substring(js_scrip, 21, nchar(js_scrip)-6))
pbp_df <- data.frame(do.call(rbind,lapply(pbp_json$plays,function(x) sapply(x, function(y) ifelse(is.null(y), NA, y)))), stringsAsFactors = F)
pbp_df$Notes <- ''

write.csv(pbp_df,paste0('pre-epa/',i,'.csv'), row.names = F)
}
