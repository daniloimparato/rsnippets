library(tidyverse)
library(magrittr)
library(rvest)

wait = 5

page_urls <- sprintf("https://www.nature.com/opinion?page=%i", 1:50)

pages <- page_urls %>% map(~ {
  Sys.sleep(wait)
  read_html(.x)
})

df <- map_df(pages, ~ {
  html_nodes(.x, 'a[data-track-label^="article card"][href^="/articles/"]') %>%
  map_df(~ {
    data.frame(
       link  = html_attr(.x, "href")
      ,title = html_node(.x, ".c-article-item__title") %>% html_text
      ,type  = html_node(.x, ".c-article-item__article-type") %>% html_text
      ,date  = html_node(.x, ".c-article-item__date") %>% html_text
    )
  })
})
