


make_patterns_compound <- function(pattern){
  terms_optimized <- regexhelpeR::optimize_regex_patterns(pattern)
  terms_lazy <- regexhelpeR::make_all_regex_lazy(terms_optimized)
  terms_underscore <- stringi::stri_replace_all_fixed(terms_lazy," ", "_")
}

gruendl_patterns_to_compound <- function(){
  terms <- popdictR::gruendl_terms
  terms_optimized <- regexhelpeR::optimize_regex_patterns(terms)
  terms_lazy <- regexhelpeR::make_all_regex_lazy(terms_optimized)
  terms_underscore <- stringi::stri_replace_all_fixed(terms_lazy," ", "_")
  return(terms_underscore)
}

