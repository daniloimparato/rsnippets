library(twitteR)
library(tidyverse)
library(magrittr)

options(httr_oauth_cache = T)

config <- here::here("twitter/secret.json") %>% jsonlite::read_json()

setup_twitter_oauth(
   consumer_key    = config$consumer_key
  ,consumer_secret = config$consumer_secret
  ,access_token    = config$access_token
  ,access_secret   = config$access_secret
)

finalistas      <- c("Thelma", "Rafa", "Manu") %>% set_names(.,.)

hashtags_extra  <- c("#FinalBBB20EmCasa", "#RedeBBB", "#BBB20")
hashtags_campea <- paste0("#", finalistas, "Campeã")
hashtags_team   <- paste0("#Team", finalistas)

hashtags        <- c(hashtags_campea, hashtags_team, hashtags_extra) %>% paste(collapse = " OR ")

search_results <- searchTwitter(hashtags, n = 100, retryOnRateLimit = 1, lang = "pt") %>%
  twListToDF %>%
  pull(text)
  iconv("UTF-8", "ASCII//TRANSLIT", sub = "")

porcentagem <- finalistas %>%
  map(~ str_detect(search_results, fixed(., T))) %>%
  map_dbl(sum) %>%
  divide_by(sum(.)) %T>%
  pie
