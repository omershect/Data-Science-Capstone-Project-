#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
source("E:/Elements/Coursera/Data Sciense/Course 10 Capstone project/Week 3/Model/Predict.R")


# Define server logic required to draw a histogram
shinyServer(function(input, output,session) {


  
  #observeEvent(input$button, {
  #  input$TextIn
  #})
  
  #observeEvent(input$do, {     
  res<-eventReactive(input$do,{
     str<-input$TextIn
     if (str!="")
      Predict_Words(str)
  })
 

   
  
  #output$Words_Results<-renderText({   
  #data <- res()
  #  if (data !="") {
  #    res<-Predict_Words(data)
  #data<-paste(unlist(data), collapse=' ')
  #data[1]
  
    
  #  }) 
  
  
  words<-reactive({
    words<-res()
  })
  
  output$Words_Results1<-renderText({   
    data<-words()
    #data<-paste(unlist(data), collapse=' ')
    unlist(data[1])
  }) 
  
  
  output$Words_Results2<-renderText({   
    data<-words()
    #data<-paste(unlist(data), collapse=' ')
    unlist(data[2:3])
  }) 
  
})
  
  

