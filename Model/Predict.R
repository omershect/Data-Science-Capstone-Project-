#Load libraries 
library(dplyr)
library(tidytext)
library(ggplot2)
library(quanteda)
library(data.table)
library(stringi)

#Load the ngrams from the disk 
load("uni_words.rda")
load("bi_words.rda")
load("tri_words.rda")
load("four_words.rda")
load("five_words.rda")

#Function to predict the next word based on a string 
Predict_Simple_Back_of <- function(str){
  
  n<-sapply(strsplit(str, " "), length)
  
  tokens <- tokens(x = char_tolower(str))
  word1<-""
  word2<-""
  word3<-""
  word4<-""
  
  # Split to words according to the numbe of words 
  if (n>=4) {
    word1<-tokens$text1[n-3]
    word2<-tokens$text1[n-2]
    word3<-tokens$text1[n-1]
    word4<-tokens$text1[n]
  }
  
  if (n==3) {
    word2<-tokens$text1[n-2]
    word3<-tokens$text1[n-1]
    word4<-tokens$text1[n]
  }
  
  if (n==2) {
    word3<-tokens$text1[n-1]
    word4<-tokens$text1[n]
  }
  
  if (n==1) 
    word4<-tokens$text1[n]
  
  #Search ...
  Five_Gram_Search<- subset(five_words,word_1==word1 & word_2==word2 & word_3==word3 & word_4==word4)
  Four_Gram_Search<- subset(four_words,word_1==word2 & word_2==word3 & word_3==word4)
  Tree_Gram_Search<- subset(tri_words,word_1==word3 & word_2==word4)
  Bi_Gram_Search<- subset(bi_words,word_1==word4)
  
  Words<-select(Five_Gram_Search,word_5,Prob)
  names(Words)<-c("Word","Prob")
  
  tmp<-select(Four_Gram_Search,word_4,Prob)
  names(tmp)<-c("Word","Prob")
  tmp$Prob<-tmp$Prob*.4
  Words<-rbind(Words,tmp)
  
  tmp<-select(Tree_Gram_Search,word_3,Prob)
  names(tmp)<-c("Word","Prob")
  tmp$Prob<-tmp$Prob*.4*.4
  Words<-rbind(Words,tmp)
  
  tmp<-select(Bi_Gram_Search,word_2,Prob)
  names(tmp)<-c("Word","Prob")
  tmp$Prob<-tmp$Prob*.4*.4*.4
  Words<-rbind(Words,tmp)
  
  setorder(Words,colS=-Prob)
  return(Words)
  
  
}



Predict_Words <- function(str){
  
Words<-Predict_Simple_Back_of(str)
res<-select(Words,Word)[1:3,]
return (list(res))
  
}


