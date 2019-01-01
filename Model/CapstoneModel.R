

#Load libraries 
library(kableExtra)
library(dplyr)
library(tidytext)
library(ggplot2)
library(quanteda)
library(data.table)
library(stringi)




## Load the Data 

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


en_US.Blogs <- readLines(con = "./data/final/en_US/en_US.blogs.txt")
en_US.news <- readLines(con = "./data/final/en_US/en_US.news.txt")
en_US.twitter <- readLines(con = "./data/final/en_US/en_US.twitter.txt")

sample_rate<-30/100
set.seed(121)
Sample.Text<-c(sample(en_US.Blogs,length(en_US.Blogs) * (sample_rate),replace = FALSE),
               sample(en_US.news,length(en_US.news) * (sample_rate),replace = FALSE),
               sample(en_US.twitter,length(en_US.twitter) * (sample_rate),replace = FALSE))

rm(en_US.Blogs)
rm(en_US.news)
rm(en_US.twitter) 

print("Complete Loading data")


#Convert the Sample.txt vector to a Data frame 
DF <- data_frame(text = Sample.Text)
rm(Sample.Text)

n = nrow(DF)
set.seed(101)
trainIndex = sample(1:n, size = round(0.9*n), replace=FALSE)
train = DF[trainIndex ,]
test = DF[-trainIndex ,]

rm(DF)

print ("Create Corpus")
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
  removeNumbers  = TRUE,
  remove_hyphens = TRUE,
  remove_symbols = TRUE,
  remove_url = TRUE)


print("Complete cleaning and Creating Corpus")

rm(corp)
stemed_words <- tokens_wordstem(Text.Sentences, language = "english")

rm(Text.Sentences)
 

print("Create ngrams")
bi_gram <- tokens_ngrams(stemed_words, n = 2)
print("complete 2  ngrams")
tri_gram <- tokens_ngrams(stemed_words, n = 3)
print("complete 3  ngrams")
four_gram<- tokens_ngrams(stemed_words, n = 4)
print("complete 4  ngrams")
five_gram<- tokens_ngrams(stemed_words, n = 5)


print("Complete Creating 1:5 ngrams")

uni_DFM <- dfm(stemed_words)
rm(stemed_words)
print("Uni DFM ")
bi_DFM <- dfm(bi_gram)
rm(bi_gram)
print("Bi DFM ")
tri_DFM <- dfm(tri_gram)
rm(tri_gram)
print("Tri DFM ")
four_DFM <- dfm(four_gram)
print("four DFM ")

rm(four_gram)

five_DFM <- dfm(five_gram)
print("five DFM ")
rm(five_gram)

uni_DFM <- dfm_trim(uni_DFM, min_termfreq=4)
bi_DFM <- dfm_trim(bi_DFM, min_termfreq=4)
tri_DFM <- dfm_trim(tri_DFM, min_termfreq=4)
four_DFM <- dfm_trim(four_DFM, min_termfreq=4)
five_DFM <- dfm_trim(five_DFM, min_termfreq=4)

print("Complete trimming ngrams")



# Create named vectors with counts of words 
sums_U <- colSums(uni_DFM)
sums_B <- colSums(bi_DFM)
sums_T <- colSums(tri_DFM)
sums_F <- colSums(four_DFM)
sums_FF <- colSums(five_DFM)

rm(uni_DFM)
rm(bi_DFM)
rm(tri_DFM)
rm(four_DFM)
rm(five_DFM)

# Create data tables with individual words as columns
uni_words <- data.table(word_1 = names(sums_U), count = sums_U)

print("Complete uni words ")

bi_words <- data.table(
  word_1 = sapply(strsplit(names(sums_B), "_", fixed = TRUE), '[[', 1),
  word_2 = sapply(strsplit(names(sums_B), "_", fixed = TRUE), '[[', 2),
  count = sums_B)

print("Complete bi words ")

tri_words <- data.table(
  word_1 = sapply(strsplit(names(sums_T), "_", fixed = TRUE), '[[', 1),
  word_2 = sapply(strsplit(names(sums_T), "_", fixed = TRUE), '[[', 2),
  word_3 = sapply(strsplit(names(sums_T), "_", fixed = TRUE), '[[', 3),
  count = sums_T)

print("Complete tri words ")

four_words<-data.table(
  word_1 = sapply(strsplit(names(sums_F), "_", fixed = TRUE), '[[', 1),
  word_2 = sapply(strsplit(names(sums_F), "_", fixed = TRUE), '[[', 2),
  word_3 = sapply(strsplit(names(sums_F), "_", fixed = TRUE), '[[', 3),
  word_4 = sapply(strsplit(names(sums_F), "_", fixed = TRUE), '[[', 4),
  count = sums_F)


five_words<-data.table(
  word_1 = sapply(strsplit(names(sums_FF), "_", fixed = TRUE), '[[', 1),
  word_2 = sapply(strsplit(names(sums_FF), "_", fixed = TRUE), '[[', 2),
  word_3 = sapply(strsplit(names(sums_FF), "_", fixed = TRUE), '[[', 3),
  word_4 = sapply(strsplit(names(sums_FF), "_", fixed = TRUE), '[[', 4),
  word_5 = sapply(strsplit(names(sums_FF), "_", fixed = TRUE), '[[', 5),
  count = sums_F)

print("Complete data tables")

setkey(uni_words, word_1)
setkey(bi_words, word_1, word_2)
setkey(tri_words, word_1, word_2, word_3)
setkey(four_words, word_1, word_2, word_3,word_4)
setkey(five_words, word_1, word_2, word_3,word_4,word_5)

#Calculate regular probability 
uni_words<-mutate(uni_words,Prob=count/sum(count))
bi_words<-mutate(bi_words,Prob=count/sum(count))
tri_words<-mutate(tri_words,Prob=count/sum(count))
four_words<-mutate(four_words,Prob=count/sum(count))
five_words<-mutate(five_words,Prob=count/sum(count))


print("Results")
cat("Sample Rate is:",sample_rate)
cat("Number of Uni Words terms:",dim(uni_words)[1])
cat("Number of bi Words terms:",dim(bi_words)[1])
cat("Number of tri Words terms:",dim(tri_words)[1])
cat("Number of quad Words terms:",dim(four_words)[1])

save(uni_words, file="uni_words.rda")
save(bi_words, file="bi_words.rda")
save(tri_words, file="tri_words.rda")
save(four_words, file="four_words.rda")
save(five_words, file="five_words.rda")
