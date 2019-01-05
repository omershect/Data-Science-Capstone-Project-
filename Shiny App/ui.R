#
# This is the user-interface definition of a Shiny web application. 
# Which get a Sentence from the user and predict the next Word 
#

#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  sidebarPanel(
    titlePanel("Preict Next Word"),
    textInput("TextIn", "Input the Sentence"),
    verbatimTextOutput("value"),
    #Submit button - control the changes       
    submitButton("Process Data")
  ),
  mainPanel(
    titlePanel("Next Word - Three Options:"),
    textOutput("Words_Results")
     
  )
  
  
  )
)
