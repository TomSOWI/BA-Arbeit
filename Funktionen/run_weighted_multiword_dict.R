

#############################################################
# Adjustment for identifier
#############################################################


run_weighted_multiword_dict <- function(
  data,
  dict_compound,
  dict_name = "original",
  dict_weight,
  dict_weight_name = "weighted",
  prepare_corp = FALSE,
  include_main_dict = TRUE,
  include_totals = TRUE
)
{
  if (prepare_corp == TRUE) {
    data <- expertyears_cleaning(data)
  }
  
  df_kwic <- kwic(data, pattern = dict_compound, valuetype = "regex", window = 1, case_insensitive = T)
  merged <- merge(dict_weight, as.data.frame(df_kwic), by = "pattern")
  party <- data.frame(docname = quanteda::docnames(data), party = docvars(data)$party)
  
  if (include_main_dict == TRUE) {
    merged$dict <- 1 
    merged <- merged %>%
      select(docname, dict, weight) %>%
      group_by(docname) %>%
      summarize(
        dict = sum(dict),
        weight = sum(weight)
      )
    result <- merge(merged, party, by = "docname")
    result <- result %>%
      select(party, dict, weight) %>%
      group_by(party) %>%
      summarize(
        dict = sum(dict),
        weight = sum(weight)
      )
    colnames(result) <- c("party", dict_name, dict_weight_name)
  } else {
    merged <- merged %>%
      select(docname, weight) %>%
      group_by(docname) %>%
      summarize(
        weight = sum(weight) 
      )
    result <- merge(merged, party, by = "docname")
    result <- result %>%
      select(party, weight) %>%
      group_by(party) %>%
      summarize(
        weight = sum(weight)
      )
    colnames(result) <- c("party", pattern_weight_name)
  }
  
  if (include_totals == TRUE) {
    n_sentences <- data.frame(n_sentences = docvars(data)$n_sentences, party = docvars(data)$party)
    n_sentences <- n_sentences %>%
      group_by(party) %>%
      summarize(
        n_sentences = sum(n_sentences)
      )
    result <- merge(result, n_sentences, by = "party")
  }
  
  return(result)
}


