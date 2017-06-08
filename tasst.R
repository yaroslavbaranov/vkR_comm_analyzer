
domain <- 'compbiol'
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
ggplot(data=gather(df, 'type', 'count', 2:5), 
       aes(period, count)) + geom_line(aes(colour=type)) +
  labs(x='Date', y='Count') + 
  ggtitle(paste0("General Activity of ",input$text1))

# output$Active_Users <- renderTable({

members <<- getGroupsMembersExecute(group_id = domain)
network <<- getArbitraryNetwork(members)
Sys.sleep(2.5)
users <<- getUsersExecute(members)
  
wall <<- getWallExecute(domain = domain, count = 50, 
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
nrow(active_users)
  