
# Inclusing the necassary libraries
#install.packages("ROAuth")
library("twitteR")
library("ROAuth")
library(base64enc)
library(tm)
library(wordcloud)
library(wordcloud2)
library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)
library(dplyr)

# Loading the data consisitng of Tweets of Facebook
cred <- OAuthFactory$new(consumerKey='rLdMjSRcc74TMebjgEJM3DNEn', # Consumer Key (API Key)
                         consumerSecret='72rcW81rb6vOrFihArMzgwsCbPhIWJ74s8B676FI6L5a0h4Ldn', #Consumer Secret (API Secret)
                         requestURL='https://api.twitter.com/oauth/request_token',accessURL='https://api.twitter.com/oauth/access_token',                 authURL='https://api.twitter.com/oauth/authorize')

save(cred, file="twitter authentication.Rdata")
load("twitter authentication.Rdata")

#Access Token Secret
setup_twitter_oauth("rLdMjSRcc74TMebjgEJM3DNEn", # Consumer Key (API Key)
                    "72rcW81rb6vOrFihArMzgwsCbPhIWJ74s8B676FI6L5a0h4Ldn", #Consumer Secret (API Secret)
                    "2572457845-GrCH29tg1yTTBWhNBJBsHW6oL2eJo0Fq139TBQy",  # Access Token
                    "yUlgdlFEV3CsnJctQcX940PTxbLa9dT2MR7Cm62kOpJqA")  #Access Token Secret

#registerTwitterOAuth(cred)
Tweets <- userTimeline('facebook', n = 1000,includeRts = T)
TweetsDF <- twListToDF(Tweets)
dim(TweetsDF)
View(TweetsDF)
setwd("E:\\Neeraj\\Exam and Careers\\DataScience\\Assignment")
write.csv(TweetsDF, "Tweets.csv",row.names = F)
getwd()


# Read file
facebook <- read.csv(file.choose())
str(facebook)


# Build Corpus and DTM/TDM
corpus <- facebook$text
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

cleanset<-tm_map(cleanset,removeWords, c('facebook','can'))
inspect(cleanset[1:5])
# Since the work Facebook and can is used more, this can be removed as we are 
# mining the tweets from Facebook only.Also the word "Can" is common english word.
# we can pull back the word "can"  if needed.
cleanset <- tm_map(cleanset, gsub,pattern = 'pages', replacement = 'page')
# the barplot pulls both page and pages as separate words. this should be 
# counted as one.
inspect(cleanset[1:5])

cleanset <- tm_map(cleanset,stripWhitespace)
inspect(cleanset[1:5])


#Term Document Matrix :
# Convert the unstructured data to structured data :
tdm <- TermDocumentMatrix(cleanset)
tdm <- removeSparseTerms(tdm,0.97)
str(tdm)
tdm

# the terms indicate that there are 2547 words and 1000 documents(# of tweets) in this TDM
# Sparsity is 100% which indicates that there are lots of zero values.
tdm <- as.matrix(tdm)
tdm[1:10,1:20]

w <- rowSums(tdm)  # provides the no of times a particular word has been used.
w <- subset(w, w>= 25) # Pull words that were used more than 25 times.
barplot(w, las = 2, col = rainbow(50))

# the word account as the highest frequency. This implies
# that facebook is more concerned about people's account

# Word Cloud :

w <- sort(rowSums(tdm), decreasing = TRUE) # Sort words in decreasing order.
set.seed(123)
wordcloud(words = names(w), freq = w, 
          max.words = 250,random.order = F,
          min.freq =  5, 
          colors = brewer.pal(8, 'Dark2'),
          scale = c(6,0.3),
          rot.per = 0.6)


w <- data.frame(names(w),w)
colnames(w) <- c('word','freq')
wordcloud2(w,size = 0.5, shape = 'triangle', rotateRatio = 0.5, minSize = 1)
letterCloud(w,word = "F",frequency(5), size=1)


# Read File 
fbdata <- read.csv(file.choose(), header = TRUE)
tweets <- as.character(fbdata$text)
class(tweets)

# Obtain Sentiment scores 
s <- get_nrc_sentiment(tweets)
head(s)

tweets[4]

# "@prpltnkr Hi there. You can learn how to report a #Page that's 
# pretending to be you in our Help Center: https://t.co/n1CJLpv30Z. -KN
# the above tweet has value 1 for anger, value 1 for Negative 
# and value 2 for positive which reinstates that it has a mixture of 
# all three emotions in the above statement.
get_nrc_sentiment('pretending')

# Pretend has one value of negative and one value for anger
get_nrc_sentiment('can learn') #1 for positive


# barplot 
barplot(colSums(s), las = 2.5, col = rainbow(10),
        ylab = 'Count',main= 'Sentiment scores for Facebook Tweets')
