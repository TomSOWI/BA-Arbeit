



#############################################################
# Run one weighted dictionary
#############################################################

run_weighted_multiword_dict2 <- function(
  corp,
  corp_compund,
  pattern,
  pattern_weight,
  pattern_name,
  prepare_corp = FALSE,
  include_main_dict = TRUE,
  include_totals = TRUE
  
  )
  {
  
  if(prepare_corp == TRUE){
    corp <- corp_expert_preparation(corp)
  }
  
  df_kwic <- kwic(corp_compund, pattern = pattern,valuetype = "regex", window = 1, case_insensitive = T)
  merged <- merge(pattern_weight,as.data.frame(df_kwic), by = "pattern")
  
  if (include_main_dict == TRUE){
    merged$no_weight <- 1 
    merged <- merged %>%
      select(docname,no_weight,weight) %>%
      group_by(docname) %>%
      reframe(
        dictionary = sum(no_weight),
        weight = sum(weight)
      )
    party <- data.frame(docname = quanteda::docid(corp),party = docvars(corp)$party)
    result <- merge(merged, party, by = "docname")
    result <- result %>%
      select(party,weight,dictionary) %>%
      group_by(party) %>%
      reframe(
        dictionary = sum(dictionary),
        weight = sum(weight)
      )
    
  } else{
    merged <- merged %>%
      select(docname,weight)%>%
      group_by(docname)%>%
      reframe(
       weight = sum(weight) 
      )
    party <- data.frame(docname = quanteda::docid(corp),party = docvars(corp)$party)
    result <- merge(merged, party, by = "docname")
    result <- result %>%
      select(party,weight) %>%
      group_by(party) %>%
      reframe(
        weight = sum(weight)
      )
  }
  
  if (include_totals == TRUE){
    n_sentences <- data.frame(n_sentences = as.numeric(quanteda::nsentence(corp)),
                              party = docvars(corp)$party)
    n_sentences <- n_sentences %>%
      group_by(party) %>%
      reframe(
        n_sentences = sum(n_sentences)
      )
      result <- merge(result, n_sentences, by = "party")
  } 
}



#############################################################
# Run several weighted dictionary
#############################################################

run_several_weighted_multiword_dict <- function(
  corp,
  corp_compund,
  pattern,
  pattern_weight_list,
  pattern_name,
  prepare_corp = FALSE,
  include_main_dict = TRUE,
  include_totals = TRUE
)
{
  
  
  dictionary_list <- #add all weights
    i = 0
  for (d in dictionary_list){
    result <- run_weighted_multiword_dict(corp = corp_2014, corp_compund = corp_2014_clean, pattern = pattern, pattern_weight = d)
    i = i + 1
    if (i)
  }