

#############################################################
# Prepare corpus objects from 2014,2017 and 2019
## Enter a corpus object and return a compound tokens object including important docvars
#############################################################


corpus_to_compound_tokens <- function(corp, pattern){
  toks <- get_pop_tokens_updated(corp,create_compounds = T,compounds_dict = pattern,compounds_at_level = "sentences")
  docnames(toks) <- docvars(corp)$id
  docvars(toks)$party <- docvars(corp)$party
  docvars(toks)$n_sentences <- quanteda::nsentence(corp)
  docvars(toks)$n_tokens <- quanteda::ntoken(toks)
  return(toks)
}




