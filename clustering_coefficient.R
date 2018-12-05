library(igraph)

g <- matrix(
  c(
   1,2,
   1,3,
   2,3,
   3,5,
   3,6,
   4,6,
   4,7,
   6,8,
   8,7
  )
  ,ncol = 2
  ,byrow = T
)

g <- graph_from_edgelist(g,directed = F)

cc <- transitivity(g, type="localundirected")

plot(g, vertex.label = cc)