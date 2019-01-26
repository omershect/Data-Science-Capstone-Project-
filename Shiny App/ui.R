#
# This is the user-interface definition of a Shiny web application. 
# Which get a Sentence from the user and predict the next Word 
#

#


library(shiny)
library(shinydashboard)
library(shinyjs)




#if (interactive()) {
  
  ui <- dashboardPage(skin = "red",
    dashboardHeader(
      title = "Predict Next Word",
      titleWidth = 450
    ),
    dashboardSidebar(
      # Custom CSS to hide the default logout panel
      tags$head(tags$style(HTML('.shiny-server-account { display: none; }'))),
      textInput("TextIn", "Input the Sentence"),
      verbatimTextOutput("value"),
      
      # The dynamically-generated user panel
      uiOutput("userpanel"),
      actionButton("do", "Predict")
    ),
    dashboardBody(
      
      
      conditionalPanel(condition = "output.setupComplete",    
      fluidRow(
          
          infoBox(
          title = "Top Probability word",
          width = 7,
          color = "blue",
          icon = icon("file-word"),
          textOutput("Words_Results1")

        )),
      
      
      fluidRow(
        
        infoBox(
          title = "Lower Probability words",
          width = 7,
          color = "green",
          icon = icon("file-word"),
          textOutput("Words_Results2")
          
        ))),
      conditionalPanel(condition = "!output.setupComplete",
                       
                       fluidRow(
                         
                         infoBox(
                           title = "Loading",
                           width = 7,
                           color = "blue",
                           icon = icon("spinner")
                           
                           
                         )))                      
      
    
    )

  ) 



