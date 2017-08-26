# read the text dumps ------------------------
readTxtData <- function(path = "") {
  
  # path = "data/trainingText" or "data/testText"
  txt_dump <- tibble(text = read_lines(path, skip = 1))
  
  # seperate and return txt data
  txt_dump %>%
    separate(text, into = c("ID", "txt"), sep = "\\|\\|") %>%
    mutate(ID = as.integer(ID))
}



# preprocess the text -------------------------------
textPreprocess <- function(df, bigram = FALSE) {
  
  
  # remove punctuation and convert to lower case
  cat("Removing punctuation and converting to lower case\n\n")
  df$txt <- unlist(lapply(tolower(enc2native(df$txt)),
                             tm::removePunctuation))
  
  # remove stop words
  SW <- loadStopWords() # predefined stop words
  cat("Removing stop words from the data\n\n")
  df %<>% mutate(txt = tm::removeWords(txt, SW$word))
  
  
  # parse the text
  if(bigram == TRUE) {
    
    cat("Parsing text into 2-grams\n\n")
    tidy <- df %>% 
      select(ID, txt) %>% 
      unnest_tokens(word, txt, token = "ngrams", n = 2)
    
  } else {
    
    cat("Parsing text into 1-grams\n\n")
    tidy <- df %>% 
      select(ID, txt) %>% 
      unnest_tokens(word, txt)
  }
  
  
  # stem words
  cat("Stemming the words\n\n")
  tidy  <- tidy %>% 
    mutate(word = wordStem(word))
  
  # all done - return
  tidy
}



# widify the matrix according to terms found in tf_idf -----------
wideTerms <- function(df, wordList) {
  
  # maintain only words within the word_list obtained in tf_idf
  cat("Maintaining only words from tf-idf\n\n")
  txt_subset <- df %>% 
    filter(word %in% wordList$word)
  
  # Count the top words for each ID
  cat("Counting the frequency of the remaining words\n\n")
  count_table <- 
    as_tibble((table(txt_subset$ID, txt_subset$word))) %>% 
    `colnames<-`(c("ID", "word", "count")) %>%
    group_by(ID, word) %>% 
    summarise(count = sum(count))
  
  # transform to wide format by word dummification
  cat("Converting to wide format\n\n")
  wide <- count_table %>% 
    spread(key = word, value = count)
  
  # all done - return
  wide
}



# TODO: does not work correctly ---------------------
dummifyThem <- function(toDummy) {
  
  # save desired data.frame
  df <- toDummy %>% 
    mutate_at(vars(Gene, Variation), funs(factor)) %>%
    select(Gene, Variation) %>% 
    as.data.frame()
  
  # dummify the data.frame
  dummied <- as_tibble(dummy.data.frame(toDummy))
  
  # return
  dummied
}
