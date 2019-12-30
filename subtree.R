t <- make_tree(15, 2, "out")

V(t)$name <- LETTERS[1:vcount(t)]

plot(t, layout=layout_as_tree(t))

# retrieve traversed vertices from root vertex E to leaf vertices J and K
subvertices <- shortest_paths(t, V(t)["E"], V(t)[c("J","K")])$vpath %>% unlist %>% unique

V(t)[subvertices]$color <- "#ff0000"
plot(t, layout=layout_as_tree(t))
