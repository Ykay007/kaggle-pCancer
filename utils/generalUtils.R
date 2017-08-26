# load list of stop words -------------------
loadStopWords <- function() {
  
  # load predefined stop words
  data("stop_words")
  
  # added stop words from articles txt
  tibble(word = c(as.character(1:100),
                  "figur", "fig", "et", "al", "tabl", "patient", "data", "analysi", "analyz", 
                  "studi", "method", "result", "conclus", "author", "gene", "find", 
                  "found", "show", "perform", "cell", "demonstr", "evalu", "discuss", 
                  "conclud")) %>% 
    bind_rows(select(stop_words, word), .) %>% 
    distinct
}

