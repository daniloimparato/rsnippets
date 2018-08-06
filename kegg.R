library(RCurl)

#todos as relacoes entre gene e pathway
all_links <- read.table(
  url("http://rest.kegg.jp/link/pathway/hsa")
  ,header = F
  ,stringsAsFactors = F
  ,col.names = c("gene", "pathway")
  ,sep="\t"
)

#todas as pathways de um gene (7157 = tp53)
gene_pathways <- read.table(
  url("http://rest.kegg.jp/link/pathway/hsa:7157")
  ,header = F
  ,stringsAsFactors = F
  ,sep="\t"
)

#nome da pathway
pathway_name <- read.table(
  url("http://rest.kegg.jp/list/path:hsa01522")
  ,header = F
  ,stringsAsFactors = F
  ,sep="\t"
)

#para baixar de 100 em 100
#ex http://rest.kegg.jp/list/path:hsa00010+path:hsa00030
pathway_name_100 <- read.table(
  url(paste("http://rest.kegg.jp/list/",paste(pathway_vector[1:100], collapse = "+"), sep = "", collapse = ""))
  ,header = F
  ,sep = "\t"
  ,comment.char = ""
  ,quote = ""
)



#############
#############

#o codigo a seguir separa as vias em blocos de 100 em 100 para consultar a API
vias <- c("path:hsa01522") #aqui um vetor com o identificador das vias

batch <- c(vias, rep("0", 100 - length(vias)%%100))
batch <- matrix(batch, ncol = 100, byrow = T)
batch <- apply(batch, MARGIN = 1, function(x) paste(x[x!="0"], collapse= "+", sep=""))

vias_nomes <- lapply(batch, function(x){
  Sys.sleep(1)
  
  read.table(
    url(paste("http://rest.kegg.jp/list/",x,sep="",collapse=""))
    ,header = F
    ,stringsAsFactors = F
    ,col.names = c("pathway", "name")
    ,sep="\t"
    ,comment.char = ""
    ,quote = ""
  )
})

vias_nomes <- do.call(rbind,vias_nomes)

vias_nomes