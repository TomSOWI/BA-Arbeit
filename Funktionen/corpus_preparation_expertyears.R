
#############################################################
# Prepare corpus objects from 2014,2017 and 2019
#############################################################

corp_preparation_expertyears <- function(corp, pattern){
  toks <- tokens(corp, remove_punct = T, remove_symbols = T, remove_numbers = T,remove_url = TRUE,
                 remove_separators = T, include_docvars = T, split_hyphens = F) 
  toks <- tokens_tolower(toks, keep_acronyms = FALSE)
  #tokens remove greeting phrases
  toks <- get_pop_tokens_updated(corp,create_compounds = T,compounds_dict = pattern,compounds_at_level = "sentences")
  corp <- vapply(toks, paste, FUN.VALUE = character(1), collapse = " ") %>%
    corpus()
}