library(rvest)
library(XML)
library(magrittr)
library(tm)
library(wordcloud)
library(wordcloud2)
library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)
library(dplyr)


# Amazon Reviews #############################
aurl <- "https://www.amazon.in/Test-Exclusive-749/product-reviews/B07DJ8K2KT/ref=cm_cr_dp_d_show_all_btm?ie=UTF8&reviewerType=all_reviews"
amazon_reviews <- NULL
for (i in 1:10){
  murl <- read_html(as.character(paste(aurl,i,sep="=")))
  rev <- murl %>%
    html_nodes(".review-text") %>%
    html_text()
  amazon_reviews <- c(amazon_reviews,rev)
}
length(amazon_reviews)

setwd("E:\\Neeraj\\Exam and Careers\\DataScience\\Assignment")

write.table(amazon_reviews,"oneplus7t.txt",row.names = F)


oneplus7t_Lap <- read.delim('oneplus7t.txt')
str(oneplus7t_Lap)
View(oneplus7t_Lap)

# Build Corpus and DTM/TDM
library(tm)
corpus <- oneplus7t_Lap[-1,]
#corpus <- as.factor(corpus)
head(corpus)

class(corpus)
corpus <- Corpus(VectorSource(corpus))
inspect(corpus[1:5])

# Clean the text 
corpus <- tm_map(corpus,tolower)
inspect(corpus[1:5])
corpus <- tm_map(corpus,removePunctuation)
inspect(corpus[1:5])
corpus <- tm_map(corpus,removeNumbers)
inspect(corpus[1:5])
corpus_clean<-tm_map(corpus,stripWhitespace)
inspect(corpus[1:5])
cleanset<-tm_map(corpus,removeWords, stopwords('english'))
inspect(cleanset[1:5])

removeURL <- function(x) gsub('http[[:alnum:]]*','',x)
cleanset <- tm_map(cleanset, content_transformer(removeURL))
inspect(cleanset[1:5])

cleanset<-tm_map(cleanset,removeWords, c('laptop','can'))
# Since the word Laptop and can is used more, this can be removed as we are 
# mining the tweets for this laptop only.Also the word "Can" is common english word.
# we can pull back the word "can"  if needed.


cleanset <- tm_map(cleanset, gsub,pattern = 'computer', replacement = 'machine')
# the barplot pulls both Computer and Machine as separate words. this should be 
# counted as one as both holds the same synonym.

inspect(cleanset[1:5])
cleanset <- tm_map(cleanset,stripWhitespace)
inspect(cleanset[1:5])

#Term Document Matrix :
# Convert the unstructured data to structured data :
tdm <- TermDocumentMatrix(cleanset)
tdm <- removeSparseTerms(tdm,0.97)
tdm

# the terms indicate that there are 5973 words and 39262 documents(# of tweets) in this TDM
# Sparsity is 87% which indicates that there are lots of zero values.
tdm <- as.matrix(tdm)
tdm[1:10,1:20]
# Bar Plot 

w <- rowSums(tdm)  # provides the no of times a particular word has been used.
w <- subset(w, w>= 25) # Pull words that were used more than 25 times.
barplot(w, las = 2, col = rainbow(50))

# the word Machine as the highest frequency. This implies
# that MSI_Laptop has got more reviews about the Machine's performance or
# battery or about their specifications.

# Word Cloud :
library(wordcloud)
w <- sort(rowSums(tdm), decreasing = TRUE) # Sort words in decreasing order.
set.seed(123)
wordcloud(words = names(w), freq = w, 
          max.words = 250,random.order = F,
          min.freq =  3, 
          colors = brewer.pal(8, 'Dark2'),
          scale = c(5,0.3),
          rot.per = 0.6)

library(wordcloud2)
w <- data.frame(names(w),w)
colnames(w) <- c('word','freq')
wordcloud2(w,size = 0.5, shape = 'triangle', rotateRatio = 0.5, minSize = 1)



# lettercloud 

letterCloud(w,word = 'Am',frequency(5), size=1)

# Sentiment Analysis for tweets:
library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)
library(dplyr)
# Read File 
Amzn_reviews <- read.delim('MSIGTGN.TXT')
reviews <- as.character(Amzn_reviews[-1,])
class(reviews)

# Obtain Sentiment scores 
s <- get_nrc_sentiment(reviews)
head(s)
reviews[1]
# on tweet 4, you have 4 for anger, each one work for disgust, fear 
# and sadness, 3 for trust , 4 words for negative and 2 positive.
get_nrc_sentiment('Love')

# Love has one Joy and one positive 
get_nrc_sentiment('glaring') #1 Anger and 1 Negavite
# it has a bad battery life
# barplot 
barplot(colSums(s), las = 2.5, col = rainbow(10),
        ylab = 'Count',main= 'Sentiment scores for Amazon Reviews
        -MSI GT63 TITAN Gaming Laptop')
