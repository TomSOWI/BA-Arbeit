corpus_to_poppulism_context_df <- function(corp, terms){
  df_kwic <- kwic(corp, pattern = terms, valuetype = "regex", window = 5) #window 
  #combine pre and after
  df_kwic$context <- paste(df_kwic$pre, df_kwic$post, sep = " ")
  df_kwic <- as.data.frame(df_kwic) %>% select(docname,pattern,context)
  docvars$corp <- NULL #aus corpus cleaning
  docvars$docname  <- as.character(docvars$docname)
  df_complete <- merge(df_kwic, docvars) #add party colum
}