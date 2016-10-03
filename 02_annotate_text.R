
# Initialize ############################################################################

# load packages
library(readr)
library(dplyr)
library(stringr)

# set working directory
setwd("~/Documents/presidential-debate-nlp/")

# load NLP functions
source("google_nlp_api.R")

# load english stopwords
source("stopwords_ref.R")

# define api key
source("keys.R") # google.api.key <- "YOUR_API_KEY"




# Load data ############################################################################

# load transcript
transcript <- read_csv("data/vox_transcript.csv")




# Annotate text using Google Cloud Natural Language API ############################################################################

# annotate text
annotations <- AnnotateText(text_body = transcript$text,
                            extract_syntax = TRUE, extract_entities = TRUE,
                            extract_document_sentiment = TRUE,
                            google_api_key = google.api.key)

# save workspace
save.image("02_annotate_text_raw.RData")

# extract each data frame, rename id column
annotations.sentences <- annotations$sentences %>% rename(text_id=document_id)
annotations.tokens <- annotations$tokens %>% rename(text_id=document_id)
annotations.entities <- annotations$entities %>% rename(text_id=document_id)
annotations.sentiment <- annotations$documentSentiment %>% rename(text_id=document_id)

# create a data frame of ngrams (1, 2, 3) using annotations.tokens
tokens.ngrams <- annotations.tokens %>%
  filter(partOfSpeech.tag %in% c("ADJ", "ADV", "NOUN", "PRON", "VERB") &
           !(lemma %in% stopwords) &
           lemma != "'s") %>%
  mutate(lemma=str_to_lower(lemma)) %>%
  group_by(text_id) %>%
  mutate(lemma_2=lead(lemma, 1),
         lemma_3=lead(lemma, 2),
         bigram=ifelse(!is.na(lemma_2), paste(lemma, lemma_2), as.character(NA)),
         trigram=ifelse(!is.na(lemma_3), paste(lemma, lemma_2, lemma_3), as.character(NA)),
         lemma_2=NULL,
         lemma_3=NULL)




# Output data ############################################################################

write.csv(annotations.sentences, file = "data/annotations_sentences.csv", row.names = FALSE)
write.csv(annotations.tokens, file = "data/annotations_tokens.csv", row.names = FALSE)
write.csv(annotations.entities, file = "data/annotations_entities.csv", row.names = FALSE)
write.csv(annotations.sentiment, file = "data/annotations_sentiment.csv", row.names = FALSE)
write.csv(tokens.ngrams, file = "data/tokens_ngrams.csv", row.names = FALSE)
