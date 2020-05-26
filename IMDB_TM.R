library(httpuv)
library(rvest)
library(base64enc)
library(tm)
library(XML)
library(magrittr)
library(wordcloud)
library(wordcloud2)
library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)
library(dplyr)


# install.packages('devtools')
# devtools::install_github("lchiffon/wordcloud2")


# IMDBReviews #############################
aurl <- "https://www.imdb.com/title/tt6146586/reviews?ref_=tt_ql_3"
IMDB_reviews <- NULL
for (i in 1:10){
  murl <- read_html(as.character(paste(aurl,i,sep="=")))
  rev <- murl %>%
    html_nodes(".show-more__control") %>%
    html_text()
  IMDB_reviews <- c(IMDB_reviews,rev)
}
length(IMDB_reviews)

setwd("E:\\Neeraj\\Exam and Careers\\DataScience\\Assignment")
write.table(IMDB_reviews,"Aquaman.txt",row.names = F)
JWick <- read.delim('Aquaman.txt')
str(JWick)

View(JWick)

# Build Corpus and DTM/TDM
corpus <- JWick[-1,]
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

cleanset<-tm_map(cleanset,removeWords, c('can','film'))
# Since the word laptop and can were used, this can be removed as we are 
# mining the tweets for this film.Also the word "Can" is common english word.
# we can pull back the word "can"  if needed.
cleanset<-tm_map(cleanset,removeWords, c('movie','movies'))
# Removing the word movie and movies on similar grounds - as unnecessary.
cleanset <- tm_map(cleanset, gsub,pattern = 'character', replacement = 'characters')
# the barplot pulls both character and characters as separate words. this should be 
# counted as one as both holds the same synonym.
inspect(cleanset[1:5])

cleanset <- tm_map(cleanset,stripWhitespace)
inspect(cleanset[1:5])

#Term Document Matrix :
# Convert the unstructured data to structured data :
tdm <- TermDocumentMatrix(cleanset)
tdm <- removeSparseTerms(tdm,0.97)
tdm

# the terms indicate that there are 13649 words and 393036 documents(# of tweets) in this TDM
# Sparsity is 97% which indicates that there are lots of zero values.
tdm <- as.matrix(tdm)
tdm[1:10,1:20]

# Bar Plot 

w <- rowSums(tdm)  # provides the no of times a particular word has been used.
w <- subset(w, w>= 50) # Pull words that were used more than 25 times.
barplot(w, las = 2, col = rainbow(50))

# the word Aquaman,Like and James as the highest frequency. This implies
# that Movie Aquaman has got more reviews about the James and 
# most of them liked the movie.

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
wordcloud2(w,size = 0.5, shape = 'triangle', rotateRatio = 0.5, 
           minSize = 1)

# lettercloud
letterCloud(w,word = 'A',frequency(5), size=1)

# Read File 
IMDB_reviews <- read.delim('Aquaman.TXT')
reviews <- as.character(IMDB_reviews[-1,])
class(reviews)


# Obtain Sentiment scores 
s <- get_nrc_sentiment(reviews)
head(s)
reviews[3]
#"Hollow plot interspersed with the most boring fighting scenes ever. Dozens and dozens of inept villains, who are there just to be shoot. Gone are the fresh sequences of the first movie.\nSuper-uninteresting main villain. Very poor climax. Twists, just for the sake of twists. It seems like there's nobody out there capable of writing a movie anymore."
# on tweet 3, you have 3 for anger, 1 for anticipation, 1 for disgust, 2 for fear, 1 for joy
# and 2 for sadness, 1 for surprise,  1 for trust , 7 words for negative and 2 positive.
get_nrc_sentiment('splendid')

# Splendid has one Joy and one positive 
get_nrc_sentiment('no words') #1 joy , 1 for surprise and 1 positive

# barplot 
barplot(colSums(s), las = 2.5, col = rainbow(10),
        ylab = 'Count',main= 'Sentiment scores for IMDB Reviews
        for Jhon Wick Chapter 3')
