
# load packages
library(httr)
library(jsonlite)
library(dplyr)
library(progress)
library(data.table)

# set working directory
setwd("~/Documents/presidential-debate-nlp/")

# CREATE A PROJECT AND ENABLE BILLING:
#     https://cloud.google.com/natural-language/docs/getting-started
# DEFINE API KEY:
#     https://developers.google.com/places/web-service/get-api-key
source("keys.R") # google.api.key <- "YOUR_API_KEY"


ExtractEntities <- function(post_response) {
  
  # convert to data frame
  entities <- fromJSON(toJSON(content(post_response)$entities))
  
  # if it's a blank data frame (no entities in response), then return a blank data frame
  if(identical(entities, list())){
    
    blank.response <- data.frame(name = as.character(NA),
                                 type = as.character(NA),
                                 salience = as.numeric(NA),
                                 mentions = as.integer(NA),
                                 stringsAsFactors = FALSE)
    
    return(blank.response)
    
  }
  
  # flatten the data frame, count the mentions of each entity
  entities <- entities %>%
    flatten(recursive = TRUE) %>%
    rowwise() %>%
    mutate(mentions=nrow(mentions))
  
  # remove the wikipedia url (not always returned)
  entities$metadata.wikipedia_url <- NULL
  
  # unlist
  if(dim(entities)[1] >=2) {
    entities <- entities %>%
      apply(2, unlist)
  }
  
  # conver to data frame, recast column classes
  entities <- entities %>%
    as.data.frame(stringsAsFactors=FALSE) %>%
    mutate(name=as.character(name),
           type=as.character(type),
           salience=as.numeric(salience),
           mentions=as.integer(mentions))
  
  return(entities)
  
}


ExtractDocumentSentiment <- function(post_response) {
  
  # convert to data frame, recast column classes
  sentiment <- fromJSON(toJSON(content(post_response)$documentSentiment)) %>%
    as.data.frame(stringsAsFactors=FALSE) %>%
    mutate(polarity=as.numeric(polarity),
           magnitude=as.numeric(magnitude))
  
  return(sentiment)
  
}


ExtractSyntaxSentences <- function(post_response){
  
  # convert to data frame
  syntax.sentences <- fromJSON(toJSON(content(post_response)$sentences))
  
  # if it's a blank data frame (no entities in response), then return a blank data frame
  if(identical(syntax.sentences, list())){
    
    syntax.sentences <- data.frame(text.content = as.character(NA),
                                   text.beginOffset = as.integer(NA),
                                   stringsAsFactors = FALSE)
    
  } else {
    
    # flatten the data frame
    syntax.sentences <- syntax.sentences %>%
      flatten(recursive = TRUE)
    
    # unlist
    if(dim(syntax.sentences)[1] >=2) {
      syntax.sentences <- syntax.sentences %>%
        apply(2, unlist)
    }
    
    # recast column classes
    syntax.sentences <- syntax.sentences %>%
      as.data.frame(stringsAsFactors=FALSE) %>%
      mutate(text.content=as.character(text.content),
             text.beginOffset=as.integer(text.beginOffset))
    
  }
  
  return(syntax.sentences)
  
}


ExtractSyntaxTokens <- function(post_response){
  
  # convert to data frame
  syntax.tokens <- fromJSON(toJSON(content(post_response)$tokens))
  
  # if it's a blank data frame (no entities in response), then return a blank data frame
  if(identical(syntax.tokens, list())){
    
    syntax.tokens <- data.frame(lemma=as.character(NA),
                                text.content=as.character(NA),
                                text.beginOffset=as.integer(NA),
                                partOfSpeech.tag=as.character(NA),
                                dependencyEdge.headTokenIndex=as.integer(NA),
                                dependencyEdge.label=as.character(NA),
                                stringsAsFactors = FALSE)
    
  } else {
    
    # flatten the data frame
    syntax.tokens <- syntax.tokens %>%
      flatten(recursive = TRUE)
    
    # unlist
    if(dim(syntax.tokens)[1] >=2) {
      syntax.tokens <- syntax.tokens %>%
        apply(2, unlist)
    }
    
    # recast column classes
    syntax.tokens <- syntax.tokens %>%
      as.data.frame(stringsAsFactors=FALSE) %>%
      mutate(lemma=as.character(lemma),
             text.content=as.character(text.content),
             text.beginOffset=as.integer(text.beginOffset),
             partOfSpeech.tag=as.character(partOfSpeech.tag),
             dependencyEdge.headTokenIndex=as.integer(dependencyEdge.headTokenIndex),
             dependencyEdge.label=as.character(dependencyEdge.label))
  }
  
  return(syntax.tokens)
  
}


AnnotateSingleText <- function(text_body,
                               extract_syntax, extract_entities, extract_document_sentiment,
                               google_api_key) {
  
  # initialize an empty list to store output
  annotated_text <- list()
  
  # if the text_body is empty, or if nothing is requested, return a blank list
  if(is.na(text_body) |
     text_body=="" |
     ((extract_syntax + extract_entities + extract_document_sentiment) == 0)){
    
    annotated_text <- list(sentences=NULL, tokens=NULL,
                           entities=NULL, documentSentiment=NULL, language=NULL)
    
    return(annotated_text)
    
  }
  
  # POST the text body
  response.syntax <- POST(url = paste0("https://language.googleapis.com/v1beta1/documents:annotateText/?key=", google_api_key),
                          body = list(document=list(type="PLAIN_TEXT",
                                                    content=text_body),
                                      features=list(extractSyntax=extract_syntax,
                                                    extractEntities=extract_entities,
                                                    extractDocumentSentiment=extract_document_sentiment)),
                          encode = "json")
  
  
  if(extract_syntax == TRUE){
    
    # sentences
    annotated_text$sentences <- ExtractSyntaxSentences(post_response = response.syntax)
    
    # tokens
    annotated_text$tokens <- ExtractSyntaxTokens(post_response = response.syntax)
    
  }
  
  if(extract_entities == TRUE){
    
    # entities
    annotated_text$entities <- ExtractEntities(post_response = response.syntax)
    
  }
  
  if(extract_document_sentiment == TRUE) {
    
    # sentiment
    annotated_text$documentSentiment <- ExtractDocumentSentiment(post_response = response.syntax)
    
  }
  
  # language
  annotated_text$language <- content(response.syntax)$language
  
  return(annotated_text)
  
}


RbindDfId <- function(list_object, element){
  
  element.list <- sapply(list_object, "[[", element)
  
  rows.per.element <- sapply(element.list, nrow)
  rows.per.element[sapply(rows.per.element, is.null)] <- 0
  
  element.df <- rbindlist(element.list)
  
  element.df$document_id <- rep(1:length(element.list), rows.per.element)
  
  
  
  return(element.df)
  
}


AnnotateText <- function(text_body,
                         extract_syntax, extract_entities, extract_document_sentiment,
                         google_api_key) {
  
  # hacky fix to ensure output is a dataframe
  text_body <- c(text_body, NA)
  
  annotated_text_all <- list()
  
  pb <- progress_bar$new(
    format = "  Processing :current/:total (:percent) [:bar]  Elapsed: :elapsed, Remaining: :eta",
    total = length(text_body) - 1, clear = FALSE, width= 90)
  
  for(i in 1:(length(text_body))){
    
    if(i < length(text_body)){
      pb$tick()
    }
    
    annotated_text_all[[i]] <- AnnotateSingleText(text_body=text_body[i],
                                                  extract_syntax=extract_syntax,
                                                  extract_entities=extract_entities,
                                                  extract_document_sentiment=extract_document_sentiment,
                                                  google_api_key=google_api_key)
    
  }
  
  
  annotated_text_all_transpose <- list()
  
  
  if(extract_syntax == TRUE){
    
    # sentences
    annotated_text_all_transpose$sentences <- RbindDfId(list_object = annotated_text_all, element = "sentences")
    
    # tokens
    annotated_text_all_transpose$tokens <- RbindDfId(list_object = annotated_text_all, element = "tokens")
    
  }
  
  if(extract_entities == TRUE){
    
    # entities
    annotated_text_all_transpose$entities <- RbindDfId(list_object = annotated_text_all, element = "entities")
    
  }
  
  if(extract_document_sentiment == TRUE) {
    
    # sentiment
    annotated_text_all_transpose$documentSentiment <- RbindDfId(list_object = annotated_text_all, element = "documentSentiment")
    
  }
  
  if((extract_syntax + extract_entities + extract_document_sentiment) >= 1){
    
    # language
    language.list <- sapply(annotated_text_all, "[[", "language")
    language.list[sapply(language.list, is.null)] <- NA
    language.df <- data.frame(language=unlist(language.list), stringsAsFactors = FALSE) %>%
      mutate(document_id=row_number()) %>%
      na.omit()
    annotated_text_all_transpose$language <- language.df
    
  }
  
  
  return(annotated_text_all_transpose)
  
}

