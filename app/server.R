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


# Define server logic required to draw a histogram
shinyServer(
  function(input, output) {
    
   
    output$Active_Users <- renderTable({
      if(is.null(input$text1))     return(NULL)
      members <<- getGroupsMembersExecute(group_id = input$text1)
      network <<- getArbitraryNetwork(members)
      Sys.sleep(2.5)
      users <<- getUsersExecute(members)
      
      wall <<- getWallExecute(domain = input$text1, count = 10, 
                              progress_bar = TRUE)
      comments <<- wallGetCommentsList(wall,progress_bar = TRUE)
      users_commented <<- sapply(comments,
                                function(comment) data.frame(from_id = comment$from_id))
      users_commented <<- do.call(rbind, users_commented)
      likes <<- sapply(wall$posts$id,
                      function(post_id) likesGetList(type = 'post', 
                                                     owner_id = input$text2, 
                                                     item_id = post_id))
      users_liked <<- getUsersExecute(unlist(likes["items",]))
      users_posted <<- data.frame(from_id = wall$posts$from_id)
    
      active_users <<- getUsersExecute(unique(c(users_commented$from_id, 
                                                users_posted$from_id, users_liked$id)))
      nrow(active_users)
      
    })
    
    output$population <- renderTable({
      if(is.null(input$text1))     return(NULL)
      paste("You have selected", input$text1)
      population <<- unique(rbind(users, active_users))
      nrow(population)
    })
    
    output$clear_population <- renderTable({
      if(is.null(input$text1))     return(NULL)
      paste("You have selected", input$text1)
      clear_population <<- population[is.na(population$deactivated), ]
      nrow(clear_population)
      
    })
    
    output$members <- renderTable({
      if(is.null(input$text1))     return(NULL)
      paste("You have selected", input$text1)
      members_filtered <- intersect(clear_population$id, users$id)
      length(members_filtered)
    })
    
    output$share_active_members <- renderTable({
      if(is.null(input$text1))     return(NULL)
      paste("You have selected", input$text1)
      share_active_members <- length(intersect(active_users$id,users$id)) / nrow(users)
      share_active_members
    })
    

    
    output$femal_share <- renderTable({
      if(is.null(input$text1))     return(NULL)
      paste("You have selected", input$text1)
      clear_population <<- getUsersExecute(users_ids = clear_population$id, fields = 'sex')
      female_share <- nrow(subset(clear_population,clear_population$sex == 1))/nrow(clear_population)
      female_share
    })
    
    output$writer_share <- renderTable({
      if(is.null(input$text1))     return(NULL)
      paste("You have selected", input$text1)
      writers <<- unique(c(users_posted$from_id,users_commented$from_id))
      writer_share <- length(writers)/nrow(clear_population)
      writer_share
    })
    
    output$liker_share <- renderTable({
      if(is.null(input$text1))     return(NULL)
      paste("You have selected", input$text1)
      likers <- setdiff(users_liked$id,writers)
      liker_share <- length(likers)/nrow(clear_population)
      liker_share
    })
    
    output$passive_share <- renderTable({
      if(is.null(input$text1))     return(NULL)
      paste("You have selected", input$text1)
      passive <- setdiff(users$id,active_users$id)
      passive_share <- length(passive)/nrow(users)
      passive_share
    })
    
    
    
    
  }
)
