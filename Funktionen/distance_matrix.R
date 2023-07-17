distance_matrix <- function(toks, method){
  dfm <- dfm(toks) 
  dtm.matrix <- as.matrix(dfm)
  if (method %in% c("euclidean", "chisquared", "kullback", "jeffreys", "jensen","manhattan", "maximum", "canberra", "minkowski", "hamming")){
    dist.matrix <-proxyC::dist(dtm.matrix, method = method) 
  }
  if (method %in% c("cosine", "correlation", "jaccard", "ejaccard","dice", "edice", "hamann", "faith", "simple matching")){
    dist.matrix <- proxyC::simil(dtm.matrix, method = method)
  }
  return(dist.matrix)
  
}