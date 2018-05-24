#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(vkR)
library(igraph)
library(ggplot2)
library(tidyr)
library(dplyr)
library(data.table)
# Define server logic required to draw a histogram
shinyServer(
  function(input, output) {
    
    output$CommPlot <- renderPlot({
      
      domain <- input$text1
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
      
    })
   
    output$res_table <- renderTable({
      if(is.null(input$text1))     return(NULL)
      members <<- getGroupsMembersExecute(group_id = input$text1)
      # network <<- getArbitraryNetwork(members)
      Sys.sleep(2.5)
      users <<- getUsersExecute(members)
      
      wall <<- getWallExecute(domain = input$text1, count = 20, 
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
                                                users_posted$from_id, 
                                                users_liked$id)))
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
      res_table
    })
    # 
    # output$population <- renderTable({
    #   if(is.null(input$text1))     return(NULL)
    #   paste("You have selected", input$text1)
    #   population <<- unique(rbind(users, active_users))
    #   nrow(population)
    # })
    # 
    # output$clear_population <- renderTable({
    #   if(is.null(input$text1))     return(NULL)
    #   paste("You have selected", input$text1)
    #   clear_population <<- population[is.na(population$deactivated), ]
    #   nrow(clear_population)
    #   
    # })
    # 
    # output$members <- renderTable({
    #   if(is.null(input$text1))     return(NULL)
    #   paste("You have selected", input$text1)
    #   members_filtered <- intersect(clear_population$id, users$id)
    #   length(members_filtered)
    # })
    # 
    # output$share_active_members <- renderTable({
    #   if(is.null(input$text1))     return(NULL)
    #   paste("You have selected", input$text1)
    #   share_active_members <- length(intersect(active_users$id,users$id)) / nrow(users)
    #   share_active_members
    # })
    # 
    # 
    # 
    # output$femal_share <- renderTable({
    #   if(is.null(input$text1))     return(NULL)
    #   paste("You have selected", input$text1)
    #   clear_population <<- getUsersExecute(users_ids = clear_population$id, fields = 'sex')
    #   female_share <- nrow(subset(clear_population,clear_population$sex == 1))/nrow(clear_population)
    #   female_share
    # })
    # 
    # output$writer_share <- renderTable({
    #   if(is.null(input$text1))     return(NULL)
    #   paste("You have selected", input$text1)
    #   writers <<- unique(c(users_posted$from_id,users_commented$from_id))
    #   writer_share <- length(writers)/nrow(clear_population)
    #   writer_share
    # })
    # 
    # output$liker_share <- renderTable({
    #   if(is.null(input$text1))     return(NULL)
    #   paste("You have selected", input$text1)
    #   likers <- setdiff(users_liked$id,writers)
    #   liker_share <- length(likers)/nrow(clear_population)
    #   liker_share
    # })
    # 
    # output$passive_share <- renderTable({
    #   if(is.null(input$text1))     return(NULL)
    #   paste("You have selected", input$text1)
    #   passive <- setdiff(users$id,active_users$id)
    #   passive_share <- length(passive)/nrow(users)
    #   passive_share
    # })
    
    
    
    
  }
)
