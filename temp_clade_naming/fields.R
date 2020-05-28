library(XML)
library(rentrez)
library(tidyverse)
library(magrittr)

string_eukaryotes <- read_tsv(
   "https://raw.githubusercontent.com/daniloimparato/rsnippets/master/temp_clade_naming/string_eukaryotes.tsv"
  ,col_types = "cccci"
)

lineages_xml <- entrez_fetch(
  db      = "taxonomy",
  id      = string_eukaryotes[["new_taxid"]],
  rettype = "xml",
  retmode = "xml",
  parsed  = TRUE
)

string_eukaryotes %<>% mutate(
  lineage_txt = xpathSApply(lineages_xml, "//Lineage", xmlValue)
)

clade_names <- string_eukaryotes %>%
  # Long format lineages
  mutate(
     lineage_split = strsplit(lineage_txt, "; ")
  ) %>%
  unnest_longer(
     col        = lineage_split
    ,values_to  = "clade_name"
    ,indices_to = "clade_depth"
  ) %>%
  # Counting and dropping the last group
  group_by(root, clade_depth, clade_name) %>%
  tally(sort = TRUE) %>%
  # Collapsing lineages by clade depths
  summarise(
     diverging_rank = n_distinct(clade_name) > 1
    ,clade_name     = ifelse(diverging_rank, paste0(clade_name, " (", n,")", collapse = "; "), clade_name)
  ) %>%
  # Removing diverging ranks after the first one
  filter(cumsum(diverging_rank) <= 1) %>%
  # Removing irrelevant basal ranks (eg Eukaryota)
  group_by(clade_depth) %>%
  arrange(root) %>%
  filter(!(duplicated(clade_name) | duplicated(clade_name, fromLast = TRUE)) | diverging_rank) %>%
  # Choosing name
  group_by(root) %>%
  summarise(chosen_name = first(clade_name, order_by = clade_depth))
