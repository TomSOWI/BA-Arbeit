distance_matrix <- function(df, method){
  toks <- tokens(df$context)
  dfm <- dfm(toks) 
  dtm.matrix <- as.matrix(dfm)
  dist.matrix <-proxyC::dist(dtm.matrix, method = method) #euclidean
}