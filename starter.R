library(vkR)
library(ggplot2)
library(tidyr)
library(dplyr)
library(data.table)
vkOAuth(6062514, 'friends,groups')
setAccessToken(access_token = 'eabb9943eabb9943eabb9943aeeae718f1eeabbeabb9943b38e59cedd5427f8d10fb7bc')


domain <- 'compbiol'
wall <- getWallExecute(domain = domain, count = 0, progress_bar = TRUE)
metrics <- jsonlite::flatten(wall$posts[c("date", "likes", "comments", "reposts")])
metrics$date <- as.POSIXct(metrics$date, origin="1970-01-01", tz='Europe/Moscow')
df <- metrics %>% 
  mutate(period = as.Date(cut(date, breaks='month'))) %>% 
  group_by(period) %>%
  summarise(likes = sum(likes.count), comments = sum(comments.count), reposts = sum(reposts.count), n = n())
ggplot(data=gather(df, 'type', 'count', 2:5), 
       aes(period, count)) + geom_line(aes(colour=type)) +
  labs(x='Date', y='Count')


ggplot(data=gather(df, 'type', 'count', 2:5), 
       aes(period, count)) + geom_line(aes(colour=type)) +
  labs(x='Date', y='Count') + ggtitle(paste0("General Activity of ",domain))
 
# title(main = "General Activity")