


### Clean contextdata vector
clean_contextdata <- function(text){
  
  toks <- tokens(text, include_docvars = T) 
  ud_model <- udpipe_download_model("german")
  ud_model <- udpipe_load_model(ud_model)
  toks.character <- sapply(toks, paste, collapse = ' ')
  toks.df <- data.frame(doc.id = 1:length(toks), text = toks.character)
  x <- udpipe_annotate(ud_model, x = toks.df$text, doc_id = toks.df$doc.id)
  x <- as.data.frame(x)
  df.final <- aggregate(lemma ~ doc_id, data = x, FUN = paste, collapse = ' ')
  toks.final <- tokens(as.character(df.final$lemma)) %>%
                         tokens_remove(pattern = stopwords('german'))
  return(toks.final)
}


