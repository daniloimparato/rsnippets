library(tidyverse)
library(magrittr)
library(twitteR)
library(glue)

options(httr_oauth_cache = T)

config <- here::here("twitter/secret.json") %>% jsonlite::read_json()

setup_twitter_oauth(
   consumer_key    = config$consumer_key
  ,consumer_secret = config$consumer_secret
  ,access_token    = config$access_token
  ,access_secret   = config$access_secret
)

recipes <- tibble::tribble(
    ~recipe,  ~tipo,
  "#Fora%s", "fora",
  "#Fica%s", "fica",
  "#Team%s", "team"
)

participantes <- tibble::tribble(
  ~participante,    ~apelido,
        "Karol",     "Karol",
        "Karol",     "Kobra",
        "Karol",  "Mamacita",
       "Arthur",    "Arthur",
          "Gil",       "Gil",
)

df <- participantes %>%
  crossing(recipes) %>%
  mutate(hashtag = sprintf(recipe, apelido))

query <- df %$% paste(hashtag, collapse = " OR ")

search_results <- searchTwitter(query, n = 100, retryOnRateLimit = 1, lang = "pt") %>%
  twListToDF %>%
  pull(text) %>%
  iconv("UTF-8", "ASCII//TRANSLIT", sub = "")

df %<>%
  mutate(
     n = hashtag %>% fixed(T) %>% map_int(~ str_detect(search_results, .x) %>% sum)
    ,np = n / sum(n)
  )

ggplot(df, aes(x = "", y = n, fill = participante)) +
  geom_bar(stat = "identity", position = "fill") +
  coord_polar("y", start=0) +
  facet_wrap(tipo ~ .)
