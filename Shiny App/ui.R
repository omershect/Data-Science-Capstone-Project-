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
          title = "Next Word 1",
          icon = icon("file-word"),
          textOutput("Words_Results")

        ))
      
    )
  )

  



