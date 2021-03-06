#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(vkR)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  titlePanel("SMM Dominator"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("The Owls are not what they seems"),

      textInput("text1", label = h5("Enter domain, for example: incrussiamedia"),
                value = "")
      # textInput("text2", label = h5("Enter owner_id"),
      #           value = "") 
    ),
    
    mainPanel(
      
      helpText("General Activity"),
      plotOutput("CommPlot"),
      
      helpText("Community stats "),
      tableOutput("res_table")
      
      # helpText("Population: "),
      # tableOutput("population"),
      # 
      # helpText("Clear Population: "),
      # tableOutput("clear_population"),
      # 
      # helpText("Members: "),
      # tableOutput("members"),
      # 
      # helpText("Share Active Members: "),
      # tableOutput("share_active_members"),
      # 
      # helpText("Femal Share: "),
      # tableOutput("femal_share"),
      # 
      # helpText("Writer Share: "),
      # tableOutput("writer_share"),
      # 
      # helpText("Liker Share: "),
      # tableOutput("liker_share"),
      # 
      # helpText("Passive Share: "),
      # tableOutput("passive_share")
      
    )
  )
))