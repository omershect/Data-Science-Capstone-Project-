#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)



# Define server logic required to draw a histogram
shinyServer(function(input, output,session) {

  source("E:/Elements/Coursera/Data Sciense/Course 10 Capstone project/Week 3/Model/Predict.R")
  
  TextIn<-reactive({
      input$TextIn
    })


output$Words_Results<-renderText({   
data <- TextIn()
  if (data !="") {
      res<-Predict_Words(data)
      res<-paste( unlist(res), collapse=' ')
      res
  }
    
}) 

})
  
  
