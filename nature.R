library(tidyverse)
library(magrittr)
library(rvest)
library(here)

setwd(here())

load_tsv <- TRUE
tsv <- "C:/Users/danilo/Downloads/nature.tsv"

min_wait = 5
max_wait = 10

page_urls <- sprintf("https://www.nature.com/opinion?page=%i", 1:50)

if(load_tsv) {
  df <- read_tsv(tsv)
} else {
  pages <- page_urls %>% map(~ {
    Sys.sleep(runif(1, min_wait, max_wait))
    print(.x)
    read_html(.x)
  })
  
  df <- map_df(pages, ~ {
    html_nodes(.x, 'a[data-track-label^="article card"][href^="/articles/"]') %>%
      {
        data.frame(
          link  = sprintf("https://www.nature.com%s", html_attr(., "href"))
          ,title = html_node(., ".c-article-item__title") %>% html_text
          ,type  = html_node(., ".c-article-item__article-type") %>% html_text
          ,date  = html_node(., ".c-article-item__date") %>% html_text
        )
      }
  })
  
  write_tsv(df, tsv)
}


relevant_patterns <- c(
   "?"
   ,"who"
   ,"why"
   ,"what"
   ,"which"
   ,"where"
   ,"should"
   ,"must"
   ,"can"
   ,"has"
   ,"have"
) %>% fixed(ignore_case = T)

df %<>% mutate(
  relevant = title %>% map(str_detect, relevant_patterns) %>% map_dbl(sum)
)
