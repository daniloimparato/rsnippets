#####################################
## funcao calculo de escores
#####################################
combinescores<-function(dat, evidences="all", confLevel=0.4){
  if(evidences[1]=="all"){
    edat<-dat[,-c(1,2,ncol(dat))]
  } else {
    if(!all(evidences%in%colnames(dat))){
      stop("NOTE: one or more 'evidences' not listed in 'dat' colnames!")
    }
    edat<-dat[,evidences]
  }
  edat<-edat/1000
  edat<-1-edat
  sc<- apply(X = edat, MARGIN = 1, FUN = function(x) 1-prod(x))
  dat<-cbind(dat[,c(1,2)],combined_score=sc)
  idx<-dat$combined_score>=confLevel
  dat<-dat[idx,]
  return(dat)
}

######################################################
####              Loading data
######################################################
ids <- data.frame(gene = c("TP53","BRCA1"), whatever=c("lala","abcde"))

######################################################
####              STRINGdb
######################################################
library("STRINGdb")

string_db <- STRINGdb$new( version="10", species=9606, score_threshold=0, input_directory="" )
mapped <- string_db$map( ids, "gene", removeUnmappedRows = TRUE )
stringdat<-string_db$get_interactions(mapped$STRING_id)

#recalculando escore apenas com "experiments" e "database"
stringdat <- combinescores(stringdat, c("experiments", "database"), 0.7)



