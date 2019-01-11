#Load libraries 
library(dplyr)
library(tidytext)
library(ggplot2)
library(quanteda)
library(data.table)
library(stringi)

setwd("E:/Elements/Coursera/Data Sciense/Course 10 Capstone project/Week 3/Model/")

#Load the ngrams from the disk 
load("uni_words.rda")
load("bi_words.rda")
load("tri_words.rda")
load("four_words.rda")
load("five_words.rda")

uni_words<-data.table(uni_words)
setkey(uni_words, word_1)
bi_words<-data.table(bi_words)
setkey(bi_words, word_1, word_2)
tri_words<-data.table(tri_words)
setkey(tri_words, word_1, word_2, word_3)
four_words<-data.table(four_words)
setkey(four_words, word_1, word_2, word_3,word_4)
five_words<-data.table(five_words)
setkey(five_words, word_1, word_2, word_3,word_4,word_5)

#Function to predict the next word based on a string 
Predict_Simple_Back_of <- function(str,Ngram = 5){
  
  
  n<-sapply(strsplit(str," "), length)
   
  tokens <- tokens(x = char_tolower(str),remove_punct = TRUE)
  
  word1<-""
  word2<-""
  word3<-""
  word4<-""
  
  # Split to words according to the numbe of words 
  if (n>=4 ) {
    word1<-tokens$text1[n-3]
    word2<-tokens$text1[n-2]
    word3<-tokens$text1[n-1]
    word4<-tokens$text1[n]
  }
  
  if (n>=3 ) {
    word2<-tokens$text1[n-2]
    word3<-tokens$text1[n-1]
    word4<-tokens$text1[n]
  }
  
  if (n>=2 ) {
    word3<-tokens$text1[n-1]
    word4<-tokens$text1[n]
  }
  
  if (n>=1 ) 
    word4<-tokens$text1[n]
  
#Search 5 Grams 
#And calculate the probabilty  

  Five_Gram_Search<-five_words[.(word1,word2,word3,word4)]
  Five_Gram_Search_NoNA<-na.omit (Five_Gram_Search, "count") 
  if (dim(Five_Gram_Search_NoNA)[1]>0){
       
        Count4Grams<-four_words[.(word1,word2,word3,word4)]$count
        Words5<-select(Five_Gram_Search,word_5,count)
        Words5<-mutate(Words5,Prob=count/Count4Grams)
        Words5<-select(Words5,word_5,Prob)
        names(Words5)<-c("word","Prob")
    } else {
      
       Words5<-select(Five_Gram_Search,word_5,count)
       Words5<-mutate(Words5,Prob=0)
       Words5<-select(Words5,word_5,Prob)
       names(Words5)<-c("word","Prob")
       Words5$Prob<-0
       Words5$word<-"dummy"
     }
  
  
#Four_Gram_Search<- subset(four_words,word_1==word2 & word_2==word3 & word_3==word4)
Four_Gram_Search<-four_words[.(word2,word3,word4)]
Four_Gram_Search_NoNA<-na.omit (Four_Gram_Search, "count") 
if (dim(Four_Gram_Search_NoNA)[1]>0){
      #Count3Grams<-subset(tri_words, word_1==word2 & word_2==word3 & word_3==word4)$count 
      Count3Grams<-tri_words[.(word2,word3,word4)]$count 
      Words4<-select(Four_Gram_Search,word_4,count)
      Words4<-mutate(Words4,Prob= ((0.4 * count)/Count3Grams))
      Words4<-select(Words4,word_4,Prob)
      names(Words4)<-c("word","Prob")
} else {
  
    Words4<-select(Four_Gram_Search,word_4,count)
    Words4<-mutate(Words4,Prob=0)
    Words4<-select(Words4,word_4,Prob)
    names(Words4)<-c("word","Prob")
    Words4$Prob<-0
    Words4$word<-"dummy"
}
  
#Tree_Gram_Search<- subset(tri_words,word_1==word3 & word_2==word4)
Tree_Gram_Search<-tri_words[.(word3,word4)]
Tree_Gram_Search_NoNA<-na.omit (Tree_Gram_Search, "count") 
if (dim(Tree_Gram_Search_NoNA)[1]>0){
      #Count2Grams<-subset(bi_words,word_1==word3 & word_2==word4)$count 
      Count2Grams<-bi_words[.(word3,word4)]$count
      Words3<-select(Tree_Gram_Search,word_3,count)
      Words3<-mutate(Words3,Prob= ((0.4 * 0.4 * count)/Count2Grams))
      Words3<-select(Words3,word_3,Prob)
      names(Words3)<-c("word","Prob")
} else {
  
    Words3<-select(Tree_Gram_Search,word_3,count)
    Words3<-mutate(Words3,Prob=0)
    Words3<-select(Words3,word_3,Prob)
    names(Words3)<-c("word","Prob")
    Words3$Prob<-0
    Words3$word<-"dummy"
}
  
  
#Bi_Gram_Search<- subset(bi_words,word_1==word4)
  Bi_Gram_Search<-bi_words[.(word4)]
  Bi_Gram_Search_NoNA<-na.omit (Bi_Gram_Search, "count") 
  if (dim(Bi_Gram_Search)[1]>0){
    #Count1Grams<-subset(uni_words,word_1==word4)$count 
    Count1Grams<-uni_words[.(word4)]$count
    Words2<-select(Bi_Gram_Search,word_2,count)
    Words2<-mutate(Words2,Prob= ((0.4 *0.4 * 0.4 * count)/Count1Grams))
    Words2<-select(Words2,word_2,Prob)
    names(Words2)<-c("word","Prob")
    
  }else {
    Words2<-select(Bi_Gram_Search,word_2,count)
    Words2<-mutate(Words2,Prob=0)
    Words2<-select(Words2,word_2,Prob)
    names(Words2)<-c("word","Prob")
    Words2$Prob<-0
    Words2$word<-"dummy"
    
  }
  
  
  words<-rbind(Words4,Words5,Words3,Words2)
  words<-words[!duplicated(words$word),]
  
  #in case the search find less then 5 words - complete with words with highest 
  # probabilty from Uni gram
  #if(dim(words)[1] < 5) {
  #  setorder(uni_words,colS=-Prob) 
  #  n_complete<- 5-(dim(words))[1]
  #}
    
  
  setorder(words,colS=-Prob)
  return(words)
  
  
}



Predict_Words <- function(str,Ngram=5){
  
Words<-Predict_Simple_Back_of(str,Ngram)
res<-select(Words,word)[1:3,]
return (list(res))





}


predict.baseline<-function(x){
  
  
  if (x=="" | x==" ") x<-"the"
  word<-Predict_Words(unname(x))
  return(word)
  
}


