library(tidyverse)
library(magrittr)
library(twitteR)

options(httr_oauth_cache = T)

config <- jsonlite::read_json("C:/Users/danilo/Downloads/rsnippets/twitter/secret.json")

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
  ~participante,  ~apelido,
    "Jessilane",   "Jessi",
      "Eliezer",     "Eli",
      "Eliezer", "Eliezer",
       "Arthur",  "Arthur",
      "Douglas",      "DG",
      "Douglas", "Douglas"
  )


df <- participantes %>%
  tidyr::crossing(recipes) %>%
  dplyr::mutate(hashtag = sprintf(recipe, apelido))

query <- df %$% paste(hashtag, collapse = " OR ")

results_df <- searchTwitter(
    searchString     = "{query} exclude:retweets" %>% str_glue,
    n                = 20000,
    retryOnRateLimit = 1,
    lang             = "pt",
    resultType       = "recent"
  ) %>%
  twListToDF

search_results <- results_df %>%
  pull(text) %>%
  iconv("UTF-8", "ASCII//TRANSLIT", sub = "")

results_df <- df %>%
  group_by(tipo) %>%
  mutate(
     n          = hashtag %>% fixed(T) %>% map_int(~ str_detect(search_results, .x) %>% sum)
    ,group_size = sum(n)
    ,prop       = ((n / group_size) * 100) %>% round(., 2)
  ) %>%
  group_by(tipo, participante) %>%
  summarise(
    n          = sum(n),
    prop       = sum(prop),
    group_size = unique(group_size)
  )

ggplot(results_df, aes(x = "", y = prop, fill = participante)) +
  geom_bar(stat = "identity", color = "#ffffff", size = 2, width = 1) +
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste0(prop, "%")), position = position_stack(vjust=0.5)) +
  facet_wrap("{tipo}\nn = {group_size}" %>% str_glue ~ .) +
  theme_void() + 
  theme(
     legend.title    = element_blank()
    ,legend.position = "bottom"
    ,legend.box      = "horizontal"
  ) +
  guides(color = guide_legend(nrow = 1))

ggsave("C:/Users/danilo/Downloads/bbb22_%s.jpg" %>% sprintf(Sys.time() %>% format("%Y%m%d_%H%M")))
