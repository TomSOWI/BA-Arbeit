textplot <- function(text, min_freq = 25){
  text <- gsub("dass","",text)
  
  dtm <- dfm(text) %>%
    dfm(remove = stopwords('german')) %>%
    dfm_wordstem() %>%
    dfm_trim(min_termfreq = min_freq , verbose = FALSE)
  
  set.seed(100)
  textplot_wordcloud(dtm)
  
}