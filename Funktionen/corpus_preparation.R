greeting_phrases <- c("liebe frau präsidentin","lieber herr präsident","herr präsident","frau präsidentin","sehr geehrter","sehr geehrte","liebe kolleginnen und kollegen","meine sehr verehrten damen und herren","verehrte kolleginnen und kollegen","geehrte kolleginnen und kollegen","herr parlamentspräsident","frau parlamentspräsidentin","herr minister","frau ministerin","herr staatssekretär","frau staatssekretärin","herr alterspräsident","frau altpräsidentin")


corpus_preparation <- function(dataframe){
  
  
  corp <- corpus(dataframe$speech_content, docvars = data.frame(docname = dataframe$id, party = dataframe$party))
  toks <- tokens(corp, remove_punct = T, remove_symbols = T, remove_numbers = T, remove_url = TRUE,
                 remove_separators = T, include_docvars = T, split_hyphens = F)
  toks <- tokens_tolower(toks, keep_acronyms = FALSE)
  toks <- tokens_remove(toks, stopwords("german"))
  toks <- tokens_remove(toks, greeting_phrases)
  toks_compund <- get_pop_tokens_updated(
    corp,
    create_compounds = T,
    compounds_at_level = "sentences",
    compounds_dict = popdictR::gruendl_terms,
    compounds_dict_glob = FALSE
  )
  corp <- vapply(toks_compund, paste, FUN.VALUE = character(1), collapse = " ") %>%
    corpus()
  df <- cbind(corp, docvars(toks)) 
  rownames(df) <- df$docname
  assign("docvars", df,  envir = globalenv()) # für corpus_to_poppulism_context_df
  corp <- corpus(docvars, text_field = "corp")
}

corpus_preparation <- function(corpus, dict){
  expertyears_cleaning(corpus,dict)
  
  
  
  toks <- tokens_remove(toks, greeting_phrases)
  toks_compund <- get_pop_tokens_updated(
    corp,
    create_compounds = T,
    compounds_at_level = "sentences",
    compounds_dict = popdictR::gruendl_terms,
    compounds_dict_glob = FALSE
 
}