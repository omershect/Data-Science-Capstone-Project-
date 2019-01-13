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


#########################################################
#                                                       #
#Function to predict the next word based on a string    #

#########################################################


Predict_Simple_Back_of <- function(str,NoOfWordtoReturn =5){
  
  
  uni_words<-data.table(uni_words)
  setkey(uni_words, word_1)
  
  str<-iconv(str, from = "UTF-8", to = "ASCII", sub = "")
  #Calculate the length of the sentence
  n<-sapply(strsplit(str," "), length)
   
  #Convert the sentence into tokens
  tokens <- tokens(x = char_tolower(str),remove_punct = TRUE,
    remove_twitter = TRUE,
    remove_numbers = TRUE,
    removeNumbers  = TRUE,
    remove_hyphens = TRUE,
    remove_symbols = TRUE,
    remove_separators = TRUE,
    remove_url = TRUE)
  
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
  if (n==0 )
    word4<-" "
  
  #Initiliaze the length parameters 
  PredictLength<-0
  Words5_length<-0
  Words4_length<-0
  Words3_length<-0
  Words2_length<-0
  
  #Search 5 Grams 
  #And calculate the probabilty 
  Five_Gram_Search<-five_words[.(word1,word2,word3,word4)]
  #If The last word is NA - No match - Remove the entire row
  Five_Gram_Search_NoNA<-na.omit (Five_Gram_Search, "count") 
  #If there is at least one occurrence calculate the probability 
  if (dim(Five_Gram_Search_NoNA)[1]>0){
       
        Count4Grams<-four_words[.(word1,word2,word3,word4)]$count
        Words5<-select(Five_Gram_Search,word_5,count)
        Words5<-mutate(Words5,Prob=count/Count4Grams)
        Words5<-select(Words5,word_5,Prob)
        names(Words5)<-c("word","Prob")
        Words5_length<-dim(Words5)[1]
  } 
#update the length   
PredictLength<-PredictLength+Words5_length

if (PredictLength<NoOfWordtoReturn) {  
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
      Words4_length<-dim(Words4)[1]
  } else {
    Words4_length<-0
  
  }
}
 
PredictLength<-PredictLength+Words4_length
  
if (PredictLength<NoOfWordtoReturn) {  
   
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
      Words3_length<-dim(Words3)[1]
  } else {
    Words3_length<-0
  
  }
}
  
PredictLength<-PredictLength+Words3_length  
if (PredictLength<NoOfWordtoReturn) {  
  #Bi_Gram_Search<- subset(bi_words,word_1==word4)
  Bi_Gram_Search<-bi_words[.(word4)]
  Bi_Gram_Search_NoNA<-na.omit (Bi_Gram_Search, "count") 
  if (dim(Bi_Gram_Search_NoNA)[1]>0){
    #Count1Grams<-subset(uni_words,word_1==word4)$count 
    Count1Grams<-uni_words[.(word4)]$count
    Words2<-select(Bi_Gram_Search,word_2,count)
    Words2<-mutate(Words2,Prob= ((0.4 *0.4 * 0.4 * count)/Count1Grams))
    Words2<-select(Words2,word_2,Prob)
    names(Words2)<-c("word","Prob")
    Words2_length<-dim(Words2)[1]
  } else {
    Words2_length<-0
    
  }
  
}







  Pwords<-""
  
  if(Words5_length>0)
    Pwords<-Words5
  if(Words4_length>0)
    Pwords<-rbind(Pwords,Words4)
  if(Words3_length>0)
    Pwords<-rbind(Pwords,Words3)
  if(Words2_length>0)
    Pwords<-rbind(Pwords,Words2)
  if(PredictLength>0)
    Pwords<-Pwords[!duplicated(Pwords$word),]
  
  
  #in case the search find less then 5 words - complete with words with highest 
  # probabilty from Uni gram
  if(PredictLength < NoOfWordtoReturn) {
    uni<-uni_words
    UniWordsHigh<-select(head(setorder(uni,colS=-Prob),5 ),word_1,count)
    names(UniWordsHigh)<-c("word","Prob")
    if (PredictLength == 0)
      Pwords<-UniWordsHigh
    if (PredictLength == 1)
      Pwords<-rbind(Pwords,head(UniWordsHigh,4))
    if (PredictLength == 2)
      Pwords<-rbind(Pwords,head(UniWordsHigh,3))
    if (PredictLength == 3)
      Pwords<-rbind(Pwords,head(UniWordsHigh,2))
    if (PredictLength == 4)
      Pwords<-rbind(Pwords,head(UniWordsHigh,1))
  }
  else
    setorder(Pwords,colS=-Prob)
  return(Pwords)
  
  
}



Predict_Words <- function(str){
  

Words<-Predict_Simple_Back_of(str)
res<-select(Words,word)[1:3,]
return ((res))





}


predict.baseline<-function(x){
  
  
  #if (x=="" | x==" ") x<-"the"
  print(unname(x))
  x<-unname(x)
  x<-iconv(x,"UTF-8","ASCII")
  word<-Predict_Words(unname(x))
  #print(word)
  return(iconv(word, "ASCII", "UTF-8"))
  
}


