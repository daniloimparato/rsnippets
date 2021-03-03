library(tidyverse)
library(magrittr)
library(twitteR)

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
  # "Karol",     "Karol",
  # "Karol",     "Kobra",
  # "Karol",  "Mamacita",
  # "Arthur",    "Arthur",
  # "Gil",       "Gil",
  "Projota",     "Projota",
  "Arthur",    "Arthur",
  "Lumena",       "Lumena",
)

df <- participantes %>%
  crossing(recipes) %>%
  mutate(hashtag = sprintf(recipe, apelido))

query <- df %$% paste(hashtag, collapse = " OR ")

# query <- "#ForaLumena OR #ForaArthur OR #ForaProjota"

results_df <- searchTwitter(query, n = 20000, retryOnRateLimit = 1, lang = "pt") %>%
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
    ,ypos       = cumsum(prop) - (0.5 * prop)
  )

ggplot(results_df, aes(x = "", y = prop, fill = participante)) +
  geom_bar(stat = "identity", color = "#ffffff", size = 2, width = 1) +
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste0(prop, "%")), position = position_stack(vjust=0.5)) +
  facet_wrap(tipo ~ sprintf("n = %i", group_size)) +
  theme_void() + 
  theme(
     legend.title    = element_blank()
    ,legend.position = "bottom"
    ,legend.box      = "horizontal"
  ) +
  guides(color = guide_legend(nrow = 1))
