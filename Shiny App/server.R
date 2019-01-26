#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyjs)



# Define server logic required to draw a histogram
shinyServer(function(input, output) {

  rv <- reactiveValues()
  rv$setupComplete <- FALSE 
  
  
  
  ## simulate data load
  {
    
     #Loading the Data - first Time 
     source("Predict.R")
      
     
      ## set my condition to TRUE
      rv$setupComplete <- TRUE
    
    
    ## the conditional panel reads this output
    output$setupComplete <- reactive({
      return(rv$setupComplete)
    })
    outputOptions(output, 'setupComplete', suspendWhenHidden=FALSE)
    
  }
  
  
  

  
  #observeEvent(input$do, {     
  res<-eventReactive(input$do,{
     str<-input$TextIn
     if (str!="")
      Predict_Words(str,5)
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
    unlist(data[2:5])
  }) 
  
})
  
  

