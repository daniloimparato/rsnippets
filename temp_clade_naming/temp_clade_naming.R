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

collapse_names <- function(clade_names) {
  table(clade_names) %>% sort(TRUE) %>% paste0(names(.), " (", .,")", collapse = "; ")
}

yyy <- string_eukaryotes %>%
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
  filter(!duplicated(clade_name) | diverging_rank) %>%
  # Removing irrelevant ranks for the top root
  ungroup %>%
  filter(root_rank != 1 | clade_depth >= max(clade_depth[root_rank == 2])) %>%
  # Choosing name
  group_by(root) %>%
  summarise(chosen_name = first(clade_name, order_by = clade_depth))

  # remover tudo da primeira raiz que tiver o clade_depth menor que o clade_depth da segunda raiz
  # manter tudo a partir da primeira raiz onde o clade_depth for >= que o clade_depth da segunda raiz

xxx <- string_eukaryotes %>%
  mutate(lineage_split = strsplit(lineage_txt, "; ")) %>%
  unnest_longer(
    col             = lineage_split
    ,indices_include = TRUE
    ,values_to       = "clade_name"
    ,indices_to      = "clade_depth"
  ) %>%
  group_by(root, clade_depth) %>%
  mutate(
     diverging_rank = n_distinct(clade_name) > 1
    # ,clade_names    = clade_name %>% unique %>% list
  ) %>%
  group_by(clade_depth) %>%
  arrange(root) %>%
  filter(!duplicated(clade_name) | diverging_rank) %>%
  # Relative depth
  group_by(taxid) %>%
  arrange(clade_depth) %>%
  mutate(relative_depth = row_number()) %>%
  # Removing subsequent diverging ranks
  group_by(root, taxid) %>%
  arrange(clade_depth) %>%
  filter(cumsum(diverging_rank) <= 1) %>%
  # a
  group_by(root) %>%


ttt <- string_eukaryotes %>%
  mutate(lineage_split = strsplit(lineage_txt, "; ")) %>%
  unnest_longer(
     col             = lineage_split
    ,indices_include = TRUE
    ,values_to       = "clade_name"
    ,indices_to      = "clade_depth"
  ) %>%
  group_by(root, clade_depth) %>%
  summarise(
     diverging_rank = n_distinct(clade_name) > 1
    ,clade_name     = clade_name %>% unique %>% list
  ) %>%
  filter(cumsum(diverging_rank) <= 1) %>%
  # filter(root >= 28 & root <= 30) %>%
  group_by(root) %>%
  arrange(clade_depth, .by_group = TRUE) %>%
  summarise(
    lineage = list(clade_name)
  ) %>%
  mutate(
     downstream_diff = mapply(setdiff,         lineage, lead(lineage))
    ,upstream_diff   = mapply(setdiff, downstream_diff,  lag(lineage))
  )


roots_names <- string_eukaryotes %>%

  # Splitting lineage text
  mutate(lineage_split = strsplit(lineage_txt, "; ")) %>%
  group_by(root) %>%

  # For each root, get all lineage intersections
  # But also keep complete lineages for future use
  summarise(lineage = Reduce(intersect, lineage_split) %>% list,
            lineage_list = lineage_split %>% list) %>%

  # windowed lineage differences (window size = 3 -> current, next, prev)
  mutate(downstream_diff = mapply(setdiff,         lineage, lead(lineage))) %>%
  mutate(upstream_diff   = mapply(setdiff, downstream_diff,  lag(lineage))) %>%

  # Defaults to the furthest taxonomic rank (i.e. the 1st one)
  mutate(clade_name = map_chr(upstream_diff, 1, .default = NA)) %>%

  # Finding at what rank depth should mixed lineages be collapsed
  mutate(collapse_depth = lineage %>% map_int(length) + 1) %>%

  group_by(root) %>%
  # Fallback_name is the collapsed lineage ranks
  mutate(fallback_name = lineage_list %>%
           flatten %>%
           map2_chr(collapse_depth, `[`) %>%
           table %>%
           sort(TRUE) %>%
           paste0(names(.), " (", .,")", collapse="; ")) %>%
  mutate(clade_name = coalesce(clade_name, fallback_name))
