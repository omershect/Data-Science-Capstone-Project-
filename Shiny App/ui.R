#
# This is the user-interface definition of a Shiny web application. 
# Which get a Sentence from the user and predict the next Word 
#

#


library(shiny)
library(shinydashboard)




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
      
      
      
      fluidRow(
          
          infoBox(
          title = "Next Word Top Probabilty word",
          width = 7,
          color = "blue",
          icon = icon("file-word"),
          textOutput("Words_Results1")

        )),
      
      
      fluidRow(
        
        infoBox(
          title = "Next Word Lower Probabilty words",
          width = 7,
          color = "green",
          icon = icon("file-word"),
          textOutput("Words_Results2")
          
        ))
      
    )
  )

  



