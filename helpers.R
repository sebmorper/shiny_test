# Helpers


# Clean text --------------------------------------------------------------

clean.text <- function(x, language) {
  if(language == 'none') {
    y <- x %>% 
      str_to_lower %>% 
      str_remove_all(pattern = "[[:punct:]]") %>% 
      str_remove_all(pattern = "[[:digit:]]") %>% 
      str_remove_all(pattern = "http[a-z]+") %>% 
      str_remove_all('rt ') %>% 
      str_extract_all(boundary("word")) %>% 
      sapply(paste0, collapse = " ") %>% 
      str_trim() %>% 
      str_squish() %>% 
      str_replace_all('á', 'a') %>% 
      str_replace_all('é', 'e') %>%
      str_replace_all('í', 'i') %>%
      str_replace_all('ó', 'o') %>%
      str_replace_all('ú', 'u') %>% 
      str_replace_all('ñ', 'ni') %>% 
      str_replace_all('ò', 'o') %>%
      str_replace_all('à', 'a') %>%
      str_replace_all('è', 'e') %>%
      str_replace_all('ì', 'i') %>%
      str_replace_all('ù', 'u') %>%
      str_replace_all('ü', 'u')
    return(y)
  } else {
    y <- x %>% 
      str_to_lower %>% 
      str_remove_all(pattern = "[[:punct:]]") %>% 
      str_remove_all(pattern = "[[:digit:]]") %>% 
      str_remove_all(pattern = "http[a-z]+") %>% 
      str_remove_all('rt ') %>% 
      str_extract_all(boundary("word")) %>% 
      sapply(paste0, collapse = " ") %>% 
      tm::removeWords(stopwords('spanish')) %>% 
      str_trim() %>% 
      str_squish() %>% 
      str_replace_all('á', 'a') %>% 
      str_replace_all('é', 'e') %>%
      str_replace_all('í', 'i') %>%
      str_replace_all('ó', 'o') %>%
      str_replace_all('ú', 'u') %>% 
      str_replace_all('ñ', 'ni') %>% 
      str_replace_all('ò', 'o') %>%
      str_replace_all('à', 'a') %>%
      str_replace_all('è', 'e') %>%
      str_replace_all('ì', 'i') %>%
      str_replace_all('ù', 'u') %>%
      str_replace_all('ü', 'u')
    return(y)
  }
  
}

dtm.transform <- function(x) {
  y <- x %>% 
    VectorSource %>% 
    Corpus %>% 
    DocumentTermMatrix
  return(y)
}

# takes two character vectors, a "real" and a "predicted", counts the 
# frequency of each category and returns a melt data frame comparing 
# real vs predicted, ready to use it on ggplot

compare.class <- function(real.character, predicted.character) {
  
  new.data.real <- data.frame(paste(real.character), "real")
  new.data.pred <- data.frame(paste(predicted.character), "model")
  
  names(new.data.real) <- c("CL", "variable")
  names(new.data.pred) <- c("CL", "variable")
  
  new.data <- data.frame(table(rbind(new.data.real, new.data.pred)))
  print(new.data)
  return(new.data)
  
}