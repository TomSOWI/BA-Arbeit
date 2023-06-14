


get_pop_tokens_updated <- function(
  text,
  create_compounds = FALSE,
  compounds_at_level = "sentences",
  compounds_dict = NULL,
  compounds_dict_glob = FALSE
) {
  
  if (create_compounds & is.null(compounds_dict)) {
    warning(paste0("You tried to create compounds for this corpus, but no ",
                   "dictionary for compound creation has been provided."))
  }
  
  # Create compounds if param is set
  if (create_compounds & !is.null(compounds_dict)) {
    # Compounds are created
    text <- make_compounds1(
      text = text,
      patterns = compounds_dict,
      at_level = compounds_at_level,
      glob = compounds_dict_glob,
      lazy = TRUE,
      ignore_case = TRUE,
      optimize_regex = TRUE)
  }
  
  ### Here the default settings for the tokenizer are made ###
  toks <- quanteda::tokens(
    text,
    what = "word",
    remove_punct = TRUE,
    remove_symbols = TRUE,
    remove_numbers = TRUE,
    remove_url = TRUE,
    remove_separators = TRUE,
    split_hyphens = FALSE,
    include_docvars = TRUE
  )
  toks <- quanteda::tokens_tolower(toks, keep_acronyms = FALSE)
  return(toks)
}
