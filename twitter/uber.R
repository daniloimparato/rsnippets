library(openssl)
library(httpuv)
library(twitteR)
library(tm)

options(httr_oauth_cache = T)

config <- here::here("twitter/secret.json") %>% jsonlite::read_json()

setup_twitter_oauth(
   consumer_key    = config$consumer_key
  ,consumer_secret = config$consumer_secret
  ,access_token    = config$access_token
  ,access_secret   = config$access_secret
)

tw = twitteR::searchTwitter('uber codigo OR desconto -2 -duas', n = 1000, retryOnRateLimit = 1, lang = "pt")
d = twitteR::twListToDF(tw)

d[,"text_ascii"] <- iconv(d[,"text"], from = "UTF-8", to = "ASCII//TRANSLIT",sub="")

remove_words <- c("codigo","uber","desconto","viagem","promocional","primeira","reais","pra","corrida")
remove_words <- c(remove_words, iconv(stopwords("pt"), from = "UTF-8", to = "ASCII//TRANSLIT", sub=""))

corpus <- Corpus(VectorSource(d[,"text_ascii"]))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, removeWords, remove_words)
#corpus <- tm_map(corpus, stemDocument)

tdm <- TermDocumentMatrix(corpus, control=list(wordLengths = c(4, Inf)))
m <- as.matrix(tdm)

v <- sort(rowSums(m),decreasing=TRUE)
rank <- data.frame(word = names(v),freq=v)

library(graph)
library(Rgraphviz)

plot(tdm, term = v, corThreshold = 0.1, weighting = T)
