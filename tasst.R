# incrussiamedia
#  135045992
# thecalvertjournal
domain <- 'incrussiamedia'
wall <- getWallExecute(domain = domain, count = 0, progress_bar = TRUE)
metrics <- jsonlite::flatten(wall$posts[c("date", "likes", 
                                          "comments", "reposts")])
metrics$date <- as.POSIXct(metrics$date, origin="1970-01-01", 
                           tz='Europe/Moscow')
df <- metrics %>% 
  mutate(period = as.Date(cut(date, breaks='month'))) %>% 
  group_by(period) %>%
  summarise(likes = sum(likes.count), comments = sum(comments.count), 
            reposts = sum(reposts.count), n = n())
ggplot(data = gather(df, 'type', 'count', 2:5), 
       aes(period, count)) + geom_line(aes(colour=type)) +
  labs(x='Date', y='Count') + 
  ggtitle(paste0("General Activity of ",domain))

# output$Active_Users <- renderTable({

members <<- getGroupsMembersExecute(group_id = domain)
# network <<- getArbitraryNetwork(members)
Sys.sleep(2.5)
users <<- getUsersExecute(members)
  
wall <<- getWallExecute(domain = domain, count = 30, 
                          progress_bar = TRUE)
comments <<- wallGetCommentsList(wall,progress_bar = TRUE)
users_commented <<- sapply(comments,
                             function(comment) data.frame(from_id = comment$from_id))
users_commented <<- do.call(rbind, users_commented)
likes <<- sapply(wall$posts$id,
                   function(post_id) likesGetList(type = 'post', 
                                                  skip_own = 0, 
                                                  item_id = post_id))
users_liked <<- getUsersExecute(unlist(likes["items",]))
users_posted <<- data.frame(from_id = wall$posts$from_id)
active_users <<- getUsersExecute(unique(c(users_commented$from_id, 
                                            users_posted$from_id, users_liked$id)))
# nrow(active_users)
popul <<- unique(rbind(users, active_users))
# nrow(popul)
clear_popul <<- popul[is.na(popul$deactivated), ]
# nrow(clear_popul)
members_filtered <- intersect(clear_popul$id, users$id)
# length(members_filtered)
share_active_members <- length(intersect(active_users$id,users$id)) / nrow(users)
# round(share_active_members, 4)
clr_popul <<- getUsersExecute(users_ids = clear_popul$id, fields = 'sex')
female_share <- nrow(subset(clr_popul,clr_popul$sex == 1))/nrow(clr_popul)
# round(female_share, 4)
wrtrs <<- unique(c(users_posted$from_id,users_commented$from_id))
writer_share <- length(wrtrs)/nrow(clr_popul)
# round(writer_share, 4)
likers <- setdiff(users_liked$id,writers)
liker_share <- length(likers)/nrow(clr_popul)
# round(liker_share, 4)
passive <- setdiff(users$id,active_users$id)
passive_share <- length(passive)/nrow(users)
# round(passive_share, 4)
table_names <- c("total population","cleared popul.", "non-deactivated", 
                 "active users", "female share", "writers share",
                 "likers share", "passive members share")
comm_values <- c(nrow(popul), nrow(clear_popul), length(members_filtered), 
                 nrow(active_users), round(female_share, 4), 
                 round(writer_share, 4), round(liker_share, 4), 
                 round(passive_share, 4))
res_table <- data.frame(comm_values, table_names)








  