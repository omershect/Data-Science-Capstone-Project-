

#Load libraries 
library(kableExtra)
library(dplyr)
library(tidytext)
library(ggplot2)
library(quanteda)
library(data.table)
library(stringi)


memory.limit(size = 100000) 


##############################################
# Load Data                                  #
#                                            #
##############################################

### Read the data and unzip it

#Link to the Location of the Text data file 
UrlLink<-"https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
#Create Data Directory (If not exist)
if(!file.exists("data")) {  
  dir.create("data")  
} 
#Download and Unzip the file (if not exist)
if(!file.exists("./data/Coursera-SwiftKey.zip")) { 
  download.file(UrlLink, destfile="./data/Coursera-SwiftKey.zip")  
  if(!file.exists("./data/final"))
    unzip("./data/Coursera-SwiftKey.zip",exdir="./data") 
}

#Read the three sources of data 
en_US.Blogs <- readLines(con = "./data/final/en_US/en_US.blogs.txt",encoding = "UTF-8")
en_US.news <- readLines(con = "./data/final/en_US/en_US.news.txt",encoding = "UTF-8")
en_US.twitter <- readLines(con = "./data/final/en_US/en_US.twitter.txt",encoding = "UTF-8")

#Load Bad Words list 

Bad_Words <- readLines(con = "./data/base-list-of-bad-words_text-file_2018_07_30.txt",encoding = "UTF-8")

#################################################
# Sample the data and split to train and test.  #
#                                               #
#################################################



sample_rate<-100/100
sample_tweet_rate<-100/100
set.seed(121)
Sample.Text<-c(en_US.Blogs,en_US.news,en_US.twitter)

#Clean memory 
rm(en_US.Blogs)
rm(en_US.news)
rm(en_US.twitter) 

print("Complete Loading data")



#Convert the Sample.txt vector to a Data frame 
DF <- data_frame(text = Sample.Text)
rm(Sample.Text)

#Split the data into train and test sets
n = nrow(DF)
set.seed(101)
#trainIndex = sample(1:n, size = round(0.9*n), replace=FALSE)
#train = DF[trainIndex ,]
#test = DF[-trainIndex ,]
train<-DF
#Clean memory 
rm(DF)



##############################################
# Clean the Data and create tokens           #
#                                            #
##############################################


print ("Create Corpus")
#Create Corpus
corp <- corpus(train)



rm(train)


#Remove all non English words 
corp<-corpus(iconv(texts(corp), from = "UTF-8", to = "ASCII", sub = ""))


#Remove punctuation, Twitter, Numbers, Hyphens, symbols, and URL  
Text.Sentences <- tokens(
  x = tolower(corp),
  remove_punct = TRUE,
  remove_twitter = TRUE,
  remove_numbers = TRUE,
  remove_hyphens = TRUE,
  remove_symbols = TRUE,
  remove_separators = TRUE,
  remove_url = TRUE)


print("Complete cleaning and Creating Corpus")

rm(corp)

#Create tokens 
#stemed_words <- tokens_wordstem(Text.Sentences, language = "english")
stemed_words<-Text.Sentences
rm(Text.Sentences)


##############################################
# Create Uni Gram                            #
#                                            #
##############################################
uni_DFM <- dfm(stemed_words,remove=Bad_Words)
print("Complete Creating Uni DFM")
#Trim to Words with Priority higher than 2
uni_DFM <- dfm_trim(uni_DFM, min_termfreq=3)
print("Trim Uni Gram")

#Calculate the Col Sum 
sums_U <- colSums(uni_DFM)
print("Complete ColSums Uni DFM")

#PAck in Data table 
uni_words <- data.table(word_1 = names(sums_U), count = sums_U)
print("Complete uni words ")


#Create hash 
setkey(uni_words, word_1)
#Calculate Probability
uni_words<-mutate(uni_words,Prob=count/sum(count))

#Remove long words 
uni_words<-(subset(uni_words,nchar(word_1)<12))
#Save the Results to RDA file 
save(uni_words, file="uni_words.rda")

print("Complete Saving uni words ")

print(paste0("Number of Uni Words terms: ", dim(uni_words)[1]))

#clear memory 
rm(uni_DFM)
rm(sums_U)
rm(uni_words)


##############################################
# Create Bi Gram                             #
#                                            #
##############################################

bi_gram <- tokens_ngrams(stemed_words, n = 2)
bi_DFM <- dfm(bi_gram,remove=Bad_Words)
rm(bi_gram)
print("Bi DFM ")
#Trim - Keep only items with frequancy above 2
bi_DFM <- dfm_trim(bi_DFM, min_termfreq=3)
print("Complete Trim")
# Create named vectors with counts of words 
sums_B <- colSums(bi_DFM)

rm(bi_DFM)

# Create data tables with individual words as columns
bi_words <- data.table(
  word_1 = sapply(strsplit(names(sums_B), "_", fixed = TRUE), '[[', 1),
  word_2 = sapply(strsplit(names(sums_B), "_", fixed = TRUE), '[[', 2),
  count = sums_B)

print("Complete bi words ")

rm(sums_B)
#Create hash 
setkey(bi_words, word_1, word_2)

#Remove Long words 
bi_words<-subset(bi_words,(nchar(word_1)<12) & nchar(word_2)<12)

#Save the Results to RDA file 
save(bi_words, file="bi_words.rda")

print(paste0("Number of Bi Words terms: ", dim(bi_words)[1]))

rm(bi_words)
print("Complete Saving bi words ")


##############################################
# Create Tri Gram                            #
#                                            #
##############################################


tri_gram <- tokens_ngrams(stemed_words, n = 3)
print("complete 3  ngrams")



tri_DFM <- dfm(tri_gram,remove=Bad_Words)
rm(tri_gram)
print("Tri DFM ")


tri_DFM <- dfm_trim(tri_DFM, min_termfreq=3)
print("Complete 3 Grams trim")

# Create named vectors with counts of words 
sums_T <- colSums(tri_DFM)
rm(tri_DFM)


# Create data tables with individual words as columns
tri_words <- data.table(
  word_1 = sapply(strsplit(names(sums_T), "_", fixed = TRUE), '[[', 1),
  word_2 = sapply(strsplit(names(sums_T), "_", fixed = TRUE), '[[', 2),
  word_3 = sapply(strsplit(names(sums_T), "_", fixed = TRUE), '[[', 3),
  count = sums_T)

rm(sums_T)

print("Complete data tables tri grams")
#Create hash 
setkey(tri_words, word_1, word_2, word_3)

#Remove Long words 
tri_words<-subset(tri_words,(nchar(word_1)<12) & nchar(word_2)<12 & nchar(word_3)<12)


save(tri_words, file="tri_words.rda")
print(paste0("Number of Tri Words terms: ", dim(tri_words)[1]))
rm(tri_words)
print("Complete Save tri words")




##############################################
# Create Four Gram                          #
#                                            #
##############################################



four_gram<- tokens_ngrams(stemed_words, n = 4)
print("complete 4  ngrams")

four_DFM <- dfm(four_gram,remove=Bad_Words)
print("four DFM ")

rm(four_gram)

four_DFM <- dfm_trim(four_DFM, min_termfreq=3)
print("Complete Trim 4 grams")

# Create named vectors with counts of words 
sums_F <- colSums(four_DFM)
rm(four_DFM)

# Create data tables with individual words as columns
four_words<-data.table(
  word_1 = sapply(strsplit(names(sums_F), "_", fixed = TRUE), '[[', 1),
  word_2 = sapply(strsplit(names(sums_F), "_", fixed = TRUE), '[[', 2),
  word_3 = sapply(strsplit(names(sums_F), "_", fixed = TRUE), '[[', 3),
  word_4 = sapply(strsplit(names(sums_F), "_", fixed = TRUE), '[[', 4),
  count = sums_F)

print("Complete data tables 4 grams")

rm(sums_F)
#Create hash 
setkey(four_words, word_1, word_2, word_3,word_4)

#Remove Long words 
four_words<-subset(four_words,(nchar(word_1)<12) & nchar(word_2)<12 & nchar(word_3)<12 & nchar(word_4)<12)

save(four_words, file="four_words.rda")
print("complete save 4 words")
print(paste0("Number of Four Words terms: ", dim(four_words)[1]))

rm(four_words)



##############################################
# Create Five Gram                           #
#                                            #
##############################################



five_gram<- tokens_ngrams(stemed_words, n = 5)
print("complete 5  ngrams")

five_DFM <- dfm(five_gram,remove=Bad_Words)
print("five DFM ")
rm(five_gram)

five_DFM <- dfm_trim(five_DFM, min_termfreq=3)
print("Complete trimming  5 ngrams")

# Create named vectors with counts of words 
sums_FF <- colSums(five_DFM)

rm(five_DFM)

# Create data tables with individual words as columns
five_words<-data.table(
  word_1 = sapply(strsplit(names(sums_FF), "_", fixed = TRUE), '[[', 1),
  word_2 = sapply(strsplit(names(sums_FF), "_", fixed = TRUE), '[[', 2),
  word_3 = sapply(strsplit(names(sums_FF), "_", fixed = TRUE), '[[', 3),
  word_4 = sapply(strsplit(names(sums_FF), "_", fixed = TRUE), '[[', 4),
  word_5 = sapply(strsplit(names(sums_FF), "_", fixed = TRUE), '[[', 5),
  count = sums_FF)

print("Complete data tables 5 grams")

rm(sums_FF)
#Create hash 
setkey(five_words, word_1, word_2, word_3,word_4,word_5)

#Remove Long words 
five_words<-subset(five_words,(nchar(word_1)<13) & nchar(word_2)<12 & nchar(word_3)<12 & nchar(word_4)<12 & nchar(word_5)<12)

save(five_words, file="five_words.rda")
print(paste0("Number of Five Words terms: ", dim(five_words)[1]))











