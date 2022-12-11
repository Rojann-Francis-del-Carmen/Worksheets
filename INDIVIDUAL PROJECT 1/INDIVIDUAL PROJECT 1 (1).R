install.packages("tm")
install.packages("tidytext")
install.packages("plotly")
install.packages("wordcloud")
install.packages("RColorBrewer")
install.packages("dplyr")
install.packages("wordcloud2")
install.packages("syuzhet")
install.packages("magrittr")
install.packages("stringr")
install.packages("twitteR")

library(syuzhet)
library(wordcloud)
library(plotly)
library(tm)
library(tidytext)
library(dplyr)
library(RColorBrewer)
library(ggplot2)
library(magrittr)
library(stringr)
library(twitteR)

# SETUP CREDENTIALS.
CONSUMER_KEY <- "qvs442wnmCeRWBeMcsl54oVat"
CONSUMER_SECRET <- "16PDlKNLN14uXEAvWyczsDtKa3r7uR6bdjgSoSywAXRse8xTsg"
ACCESS_TOKEN <-  "1595396460484648960-103J1iDrS4Sirv8RRsQCZYr5cM7Z8a"
ACCESS_SECRET <-  "ejpl7RVDO6Ew6TAlO3AHqYyECmoWJtMV8EGJUUS5YEKZt"

setup_twitter_oauth(consumer_key = CONSUMER_KEY,
                    consumer_secret = CONSUMER_SECRET,
                    access_token = ACCESS_TOKEN,
                    access_secret = ACCESS_SECRET)


# EXTRACTING TWEETS.
trend_Tweets <- searchTwitter("#YOUTUBE -filter:retweets", n=10000, lang="en",
                              since="2022-11-25", 
                              until="2022-12-02", retryOnRateLimit = 120)
trend_Tweets

# CONVERTING LIST DATA TO DATA FRAME.
trendTweetstoDF <- twListToDF(trend_Tweets)
class(trendTweetstoDF)
names(trendTweetstoDF)
View(trendTweetstoDF)
head(trendTweetstoDF)[1:5]
head(trendTweetstoDF$text)[1:5]

# SAVE DATA FRAME FILE.
save(trendTweetstoDF,file = "trendingTweetstoDF.Rdata")

# LOAD DATA FRAME FILE.
load(file = "trendingTweetstoDF.Rdata")

# SAVING FILE AS RDATA.
save(trendTweetstoDF, file = "tweetstoDF.Rdata")

# CHECKING FOR MISSING VALUES IN A DATA FRAME.
misval <- sapply(trendTweetstoDF, function(x) sum(is.na(x)))
misval

# SUBSETTING USING THE dplyr() PACKAGE.
tweetsDP<- trendTweetstoDF %>%
  select(screenName,text,created,statusSource)
tweetsDP

# GROUPING THE DATA CREATED. 
tweetsDP %>%  
  group_by(1) %>%  
  summarise(max = max(created), min = min(created))

mte <- tweetsDP %>%  mutate(Created_At_Round = created %>% round
                            (units = 'hours') %>% as.POSIXct())
mte

tweetsDP %>% pull(created) %>% min() 
tweetsDP %>% pull(created) %>% max()

# Plot on tweets by time using the library(plotly) and ggplot().
plottweet <- mte %>% 
  dplyr::count(Created_At_Round) %>% 
  ggplot(mapping = aes(x = Created_At_Round, y = n)) +
  theme_light() +
  geom_line() +
  xlab(label = 'Date') +
  ylab(label = NULL) +
  ggtitle(label = 'Number of Tweets per Hour')

plottweet %>% ggplotly()

# ==============================================

ggplot(data = tweetsDP, aes(x = created)) +
  geom_histogram(aes(fill = ..count..)) +
  theme(legend.position = "upper right") +
  xlab("Time") + ylab("Number of tweets") + 
  scale_fill_gradient(low = "midnightblue", high = "aquamarine4")

# PLOTTING STATUS SOURCE.
encodeSource1
Stats_source<- function(x) {
  if(grepl(">Twitter for iPhone</a>", x)){
    "iphone"
  }else if(grepl(">Twitter for iPad</a>", x)){
    "ipad"
  }else if(grepl(">Twitter for Android</a>", x)){
    "android"
  } else if(grepl(">Twitter Web Client</a>", x)){
    "Web"
  } else if(grepl(">Twitter for Windows Phone</a>", x)){
    "windows phone"
  }else if(grepl(">dlvr.it</a>", x)){
    "dlvr.it"
  }else if(grepl(">IFTTT</a>", x)){
    "ifttt"
  }else if(grepl(">Facebook</a>", x)){  #This looks unreliable...
    "facebook"
  }else {
    "others"
  }
}


tweetsDP$tweetSource = sapply(tweetsDP$statusSource, 
                              Stats_source)

tweet_Source <- tweetsDP %>% 
  select(tweetSource) %>%
  group_by(tweetSource) %>%
  summarize(count=n()) %>%
  arrange(desc(count))

ggplot(tweetsDP[tweetsDP$tweetSource != 'others',], aes(tweetSource, fill = tweetSource)) +
  geom_bar() +
  theme(legend.position="none",
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylab("Number of tweets") +
  ggtitle("Tweets by Source")

# ACCOUNTS WHICH TWEET ABOUT YOUTUBE.
tweet_acc <- tweetsDP %>%
  select(screenName) %>%
  group_by(screenName) %>%
  summarize(count=n()) %>%
  arrange(desc(count)) 

#convert to Corpus
namestoCorpus <- Corpus(VectorSource(tweetsDP$screenName))  
#using ScreenName
class(tweetsDP$screenName)

clsdata <- class(VectorSource(tweetsDP$screenName))
clsdata

str(namestoCorpus)

class(namestoCorpus)

nms1 <- namestoCorpus
nms1

# WORDCLOUD FOR SCREEN_NAMES.
palet <- brewer.pal(8, "Dark2")
palet1 <- palet[-(1:4)]
set.seed(123)

par(mar = c(0,0,0,0), mfrow = c(1,1))

wordcloud(words = namestoCorpus, scale=c(3,0.5),
          max.words=10000,
          random.order=FALSE,
          rot.per=0.5,
          use.r.layout=TRUE,
          colors=palet)