subgraphs <- decompose.graph(graph)
lcc <- subgraphs[[which.max(sapply(subgraphs,vcount))]]
