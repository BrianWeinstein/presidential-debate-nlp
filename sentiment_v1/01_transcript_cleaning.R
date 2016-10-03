

library(rvest)
library(dplyr)
library(stringr)
library(zoo)

setwd("~/Dropbox/debate-sentiment/")

transcript.raw <- read_html("https://www.washingtonpost.com/news/the-fix/wp/2016/09/26/the-first-trump-clinton-presidential-debate-transcript-annotated/") %>%
  html_nodes("#article-body p") %>%
  html_text()
transcript.raw <- data.frame(text=transcript.raw)



transcript <- transcript.raw

# exctract speaker
transcript <- transcript %>%
  mutate(speaker=str_extract(string = text, pattern = "^(LESTER HOLT|HOLT|CLINTON|TRUMP)"),
         speaker=str_to_title(speaker),
         text=str_replace(string = text, pattern = "^(LESTER HOLT|HOLT|CLINTON|TRUMP):", replacement = "")) %>%
  mutate(speaker=ifelse(speaker=="Lester Holt", "Holt", speaker))

# remove audience reaction (text starting with "(")
transcript <- transcript %>%
  filter(!str_detect(text, "^\\(")) 

# create consecutive response ids, carry forward the speaker name and response id
transcript <- transcript %>%
  mutate(speaker=na.locf(speaker, na.rm=FALSE)) %>%
  mutate(id=ifelse((lag(speaker) != speaker) | is.na(lag(speaker)), row_number(), NA)) %>%
  mutate(id=na.locf(id, na.rm=FALSE)) %>%
  filter(!is.na(speaker)) %>%
  mutate(id=dense_rank(id)) %>%
  mutate(id_line=row_number())


WordCount <- function(text){
  unlist(lapply(str_match_all(text, " "), length)) + 1
}

StopWordCount <- function(text){
  
  text_clean <- text %>%
    str_to_lower() %>%
    str_replace_all(pattern = "[^[:alnum:][:space:]']", replacement = "") %>%
    str_split(pattern = " ")
  
  num_stopwords <- list()
  
  for(i in 1:length(text_clean)){
    
    num_stopwords[[i]] <- sum(text_clean[[i]] %in% tm::stopwords())
    
  }
  
  num_stopwords <- unlist(num_stopwords)
  
  return(num_stopwords)
    
}

# trim text, count words in each line
transcript <- transcript %>%
  mutate(text=str_trim(text, side="both"),
         word_count=WordCount(text),
         stop_word_count=StopWordCount(text))

# reorder columns
transcript <- transcript %>%
  select(id, id_line, speaker, text, word_count, stop_word_count)

# group concatenate (most of the) consecutive responses from the same person
transcript.grouped <- transcript %>%
  group_by(id, speaker) %>%
  summarize(text = paste0(text, collapse=" "),
            word_count=sum(word_count),
            stop_word_count=sum(stop_word_count))





