#Sentiment Analysis and wordcloud of your whatsapp chat
#download your chat from whatsapp and save it as a txt file.

rm(list=ls(all=TRUE))

#Load Required package 
install.packages("readtext")
library(readtext) 

#set working directory
setwd("C:/Users/G Amit/Desktop/Sentiment analysis") 

#put the chat text you want
Data <- readtext("chat.txt") 
TextData <- as.data.frame(Data)

library(tm)

#Define all the words which you do not want to be shown in the wordcloud, you can use names,special char. etc.
mystopwords <- c("Pizaa","e18682","tha","pm","am", "<", ">","amit","hai" ,stopwords("en")) 

#data cleaning
CleanData <- tolower(TextData$text) #Turn the data into lower case 
CleanData <- removeWords(CleanData, mystopwords) # removing stopwords defined ealier
CleanData <- removePunctuation(CleanData)#removing punctuations
CleanData <- removeNumbers(CleanData) #removing numbers

#CleanData <- stemmer(CleanData, rm.bracket = TRUE) 

# Creating a word-cloud based on max frequency word 

library(wordcloud) 
install.packages("qdap")
library(qdap)

library(tm)
library(methods)
library(RColorBrewer)
library(wordcloud)

make_word_cloud <- function(documents) {
  corpus = Corpus(VectorSource(tolower(documents)))
  corpus = tm_map(corpus, removePunctuation)
  corpus = tm_map(corpus, removeWords, stopwords("english"))
  
  frequencies = DocumentTermMatrix(corpus)
  word_frequencies = as.data.frame(as.matrix(frequencies))
  
  words <- colnames(word_frequencies)
  freq <- colSums(word_frequencies)
  wordcloud(words, freq,
            min.freq=sort(freq, decreasing=TRUE)[[100]], #defining no of words in wordcloud
            colors=brewer.pal(8, "Dark2"),
            random.color=TRUE)  
}

make_word_cloud(CleanData)

#sentiment analysis 
install.packages("syuzhet")
library(syuzhet)

Sentiments <- get_nrc_sentiment(CleanData)
Sentiments <- cbind("Words" = CleanData, Sentiments)
SentimentsScore <- data.frame("Score" = colSums(Sentiments[2:11])) 
TotalSentiments <- cbind("Sentiments" = rownames(SentimentsScore), SentimentsScore) 
rownames(TotalSentiments) <- NULL 

# Visualisation of the sentiments extracted from the texts 

library(ggplot2)
ggplot(data = TotalSentiments, aes(x = Sentiments, y = Score)) + geom_bar(stat = "identity", aes(fill = Sentiments))

