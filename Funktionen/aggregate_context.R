aggregate_context <- function(tokens, dict_compound){
  df_kwic <- kwic(tokens, pattern = dict_compound, valuetype = "regex", window = 5) #window 
  #combine pre and after
  df_kwic$context <- paste(df_kwic$pre, df_kwic$post, sep = " ")
  df_kwic <- as.data.frame(df_kwic) %>% select(docname,pattern,context)
  
  party <- data.frame(docname = docnames(tokens), party = docvars(tokens)$party )
  #
  df_kwic <- merge(df_kwic, party, by = "docname") #prüfen ob das klappt
  #
  return(df_kwic)
}


