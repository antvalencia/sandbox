# Example: Registering Your Application with Twitter
library("twitteR")
library("ROAuth")
t.api.key <- "PqyhPMEWFfdVtBRVCCH8uVksG"
t.api.secret <- "7s3XMYDA2tNdWv5B7LLKjctfSlhJmKDHLxFPUu5MG4Z7pf9GfB"
access.token <- "2170097350-tYO3ZWs4yYC4v08DMNkvQERdGug3czhAGFeFNIm"
access.secret <- "XO8yyKxQCFh39uqdy4TsvptwX8RQVHnxWljwiWTIZHk3z"

# version 1.1.8 [1] "Using browser based authentication"
# Waiting for authentication in browser... Press Esc/Ctrl + C to abort
setup_twitter_oauth(
  t.api.key,
  t.api.secret,
  access_token=access.token,
  access_secret=access.secret
  )

# To Test
start<-getUser("onesingleidea") # Users
start$location # You should get a description:


# 1) Pick two topics, “# RedSox ” and “#Yankees”, and search for the tweets associated with these two hashtags.
tweets.bos <- searchTwitter('#RedSox', n = 50)
tweets.bos
tweets.nyy <- searchTwitter('#Yankees', n = 50)
tweets.nyy


library(tm)
library(SnowballC)
# 2) Create separate (two) data corpora for the above two sets of tweets.
getCorpus <- function (tweets) {
  tweets.text <- lapply(tweets, function(t) {t$getText()})
  data.source <- VectorSource(tweets.text)
  data.corpus <- Corpus(data.source)
  return (data.corpus)
}

corpus.bos <- getCorpus(tweets.bos)
corpus.nyy <- getCorpus(tweets.nyy)


# 3) Use the pre-processing transformations described in the lecture.
removeURL <- function(x) {
  # Remove the URLs from the tweets
  gsub("(http[^ ]*)", "", x)
}
removeNumberWords <- function(x) {
  gsub("([[:digit:]]+)([[:alnum:]])*", "", x)
}

getTransCorpus <- function (data.corpus) {
  data.corpus <- tm_map(data.corpus, content_transformer(removeURL))
  data.corpus <- tm_map(data.corpus, content_transformer(removePunctuation))
  data.corpus <- tm_map(data.corpus, content_transformer(tolower))
  english.stopwords <- stopwords("en")
  data.corpus <- tm_map(data.corpus,content_transformer(removeWords),english.stopwords)
  data.corpus <- tm_map(data.corpus, content_transformer(removeNumberWords))
  data.corpus <- tm_map(data.corpus,content_transformer(stemDocument))
  data.corpus <- tm_map(data.corpus,content_transformer(stripWhitespace))
  return (data.corpus)
}
data.trans.bos <- getTransCorpus(corpus.bos)
data.trans.nyy <- getTransCorpus(corpus.nyy)


# 4) Create the term-document matrix for each hashtag.
tdm.bos <- TermDocumentMatrix(data.trans.bos)
tdm.nyy <- TermDocumentMatrix(data.trans.nyy)

# 5) Compare the frequent terms from each hashtag.
fft.bos <- findFreqTerms(tdm.bos, lowfreq=3)
fft.bos

fft.nyy <- findFreqTerms(tdm.nyy, lowfreq=3)
fft.nyy

# 6) Show word cloud for each hashtag.
library(wordcloud)
palette <- brewer.pal(8,"Dark2")
set.seed(137)

wf.bos <- rowSums(as.matrix(tdm.bos))
wordcloud(
  words=names(wf.bos),
  freq=wf.bos,
  min.freq=3,
  scale=c(2,0.5),
  rot.per=0.35,
  use.r.layout=FALSE,
  random.order=F,
  colors=palette
  )

wf.nyy <- rowSums(as.matrix(tdm.nyy))
wordcloud(
  words=names(wf.nyy),
  freq=wf.nyy,
  min.freq=3,
  scale=c(2,0.5),
  rot.per=0.35,
  use.r.layout=FALSE,
  random.order=F,
  colors=palette
  )

# 7) Using the positive and negative word lists, compute
#    the sentiment score (as described in the lecture)
#    for all the tweets for each hashtag.
pos.words <- scan(
  'positive-words.txt',
  what='character',
  comment.char = ';'
  )
neg.words <- scan(
  'negative-words.txt',
  what='character',
  comment.char = ';'
)
sentiment <- function(text, pos.words, neg.words) {
  text <- gsub('[[:punct:]]', '', text)
  text <- gsub('[[:cntrl:]]', '', text)
  text <- gsub('\\d+', '', text)
  text <- tolower(text)
  words <- strsplit(text, '\\s+')
  words <- unlist(words)
  pos.matches <- match(words, pos.words)
  pos.matches <- !is.na(pos.matches)
  neg.matches <- match(words, neg.words)
  neg.matches <- !is.na(neg.matches)
  score <- sum(pos.matches) - sum(neg.matches)
  cat (" Positive: ", words[pos.matches], "\n")
  cat (" Negative: ", words[neg.matches], "\n")
  return (score)
}
sentiment.na <- function(text, pos.words, neg.words) {
  text <- gsub('[[:punct:]]', '', text)
  text <- gsub('[[:cntrl:]]', '', text)
  text <- gsub('\\d+', '', text)
  text <- tolower(text)
  words <- strsplit(text, '\\s+')
  words <- unlist(words)
  pos.matches <- match(words, pos.words)
  pos.matches <- !is.na(pos.matches)
  neg.matches <- match(words, neg.words)
  neg.matches <- !is.na(neg.matches)
  p <- sum(pos.matches)
  n <- sum(neg.matches)
  if (p == 0 & n == 0)
    return (NA)
  else
    return (p - n)
}

tweets.bos <- searchTwitter('#RedSox', n = 500)
tweets.nyy <- searchTwitter('#Yankees', n = 500)
texts.bos <- 
  lapply(
    tweets.bos,
    function(t) {
      iconv(t$getText(),
            "latin1",
            "ASCII",
            sub=""
            )
      }
    )
texts.nyy <- 
  lapply(
    tweets.nyy,
    function(t) {
      iconv(t$getText(),
            "latin1",
            "ASCII",
            sub=""
      )
    }
  )
scores.bos <- sapply(
  texts.bos,
  sentiment.na,
  pos.words,
  neg.words
  )
table(scores.bos)
# scores.bos
# -3 -1  0  1  2  3 
# 3 22 21 50 27  7
scores.nyy <- sapply(
  texts.nyy,
  sentiment.na,
  pos.words,
  neg.words
)
table(scores.nyy)
# scores.nyy
# -4 -3 -2 -1  0  1  2  3 
# 3  2  7 44 19 95 50  5 

# 8) Create a bar chart and draw your inference.
barplot(
  table(scores.bos),
  main="#RedSox",
  xlab="Score",
  ylab="Count",
  ylim=c(0,40),
  col="cyan"
)
# pretty evenly distributed, but a spike at +1, so tweets are slightly positive
barplot(
  table(scores.nyy),
  main="#Yankees",
  xlab="Score",
  ylab="Count",
  ylim=c(0,40),
  col="cyan"
)
# spikes in -1, larger spikes around +1 & +2, so tweets are positive