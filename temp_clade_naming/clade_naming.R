lineages <- entrez_fetch(
  db      = "taxonomy",
  id      = string_eukaryotes[["new_taxid"]],
  rettype = "xml",
  retmode = "xml",
  parsed  = TRUE
)

string_eukaryotes %<>% mutate(
  root        = ogr@tree$tip.group[taxid],
  lineage_txt = xpathSApply(lineages, "//Lineage", XML::xmlValue)
)

# collapse_names(c("a","b","a")) == "a (2); b (1)"
collapse_names <- function(clade_names) {
  table(clade_names) %>% sort(TRUE) %>% paste0(names(.), " (", .,")", collapse = "; ")
}

protostomia <- string_eukaryotes %>%
  # Long format lineages
  mutate(
    lineage_split = strsplit(lineage_txt, "; ")
    ,root_rank     = as.numeric(as.factor(root))
  ) %>%
  unnest_longer(
    col             = lineage_split
    ,values_to       = "clade_name"
    ,indices_to      = "clade_depth"
  ) %>%
  # Collapsing lineages at clade ranks
  group_by(root, root_rank, clade_depth) %>%
  summarise(
    diverging_rank = n_distinct(clade_name) > 1
    ,clade_name     = ifelse(diverging_rank, collapse_names(clade_name), clade_name)
  ) %>%
  # Removing subsequent divergences
  filter(cumsum(diverging_rank) <= 1) %>%
  # Removing irrelevant ranks
  group_by(clade_depth) %>%
  arrange(root) %>%
  # filter(!duplicated(clade_name) | diverging_rank)
  mutate(
     still_unique1 = !duplicated(clade_name)
    ,still_unique2 = !duplicated(clade_name, fromLast = T)
    ,still_unique3 = n_distinct(clade_name) > 1
    ,result12      = (still_unique1 | still_unique2) | diverging_rank
    ,result3       =                 (still_unique3) | diverging_rank
  )



xxx <- string_eukaryotes %>%
  # Long format lineages
  mutate(
     lineage_split = strsplit(lineage_txt, "; ")
    ,root_rank     = as.numeric(as.factor(root))
  ) %>%
  unnest_longer(
     col             = lineage_split
    ,values_to       = "clade_name"
    ,indices_to      = "clade_depth"
  ) %>%
  # Collapsing lineages at clade ranks
  group_by(root, clade_depth) %>%
  mutate(
     diverging_rank = n_distinct(clade_name) > 1
  ) %>%
  # Removing subsequent divergences
  group_by(taxid) %>%
  arrange(clade_depth) %>%
  filter(cumsum(diverging_rank) <= 1) %>%
  arrange(-clade_depth) %>%
  filter(cumsum(!diverging_rank) <= 1 | all(!diverging_rank)) %>%
  # Removing irrelevant ranks
  group_by(clade_depth) %>%
  arrange(root) %>%
  # filter(!duplicated(clade_name) | diverging_rank) %>%
  filter(!(duplicated(clade_name) | duplicated(clade_name, fromLast = TRUE)) | diverging_rank) %>%
  # filter(n_distinct(clade_name) > 1 | diverging_rank) %>%
  # Removing irrelevant ranks for the top root
  # ungroup %>%
  # filter(root_rank != 1 | clade_depth >= max(clade_depth[root_rank == 2])) %>%
  # Choosing name
  group_by(root) %>%
  summarise(chosen_name = first(clade_name, order_by = clade_depth))







clade_names <- string_eukaryotes %>%
  # Long format lineages
  mutate(
     lineage_split = strsplit(lineage_txt, "; ")
    ,root_rank     = as.numeric(as.factor(root))
  ) %>%
  unnest_longer(
     col             = lineage_split
    ,values_to       = "clade_name"
    ,indices_to      = "clade_depth"
  ) %>%
  # Collapsing lineages at clade ranks
  group_by(root, root_rank, clade_depth) %>%
  summarise(
     diverging_rank = n_distinct(clade_name) > 1
    ,clade_name     = ifelse(diverging_rank, collapse_names(clade_name), clade_name)
  ) %>%
  # Removing subsequent divergences
  filter(cumsum(diverging_rank) <= 1) %>%
  # Removing irrelevant ranks
  group_by(clade_depth) %>%
  arrange(root) %>%
  filter(!(duplicated(clade_name) | duplicated(clade_name, fromLast = TRUE)) | diverging_rank) %>%
  # Choosing name
  group_by(root) %>%
  summarise(chosen_name = first(clade_name, order_by = clade_depth))
