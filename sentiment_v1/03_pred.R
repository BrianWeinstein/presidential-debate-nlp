
library(tm)
library(glmnet)
library(caret)


docs <- Corpus(VectorSource(ts$text))

# text processing
docs <- tm_map(docs, content_transformer(tolower)) # Convert all text to lowercase
docs <- tm_map(docs, content_transformer(removeNumbers)) # Remove all numbers
docs <- tm_map(docs, removeWords, stopwords("english")) # Delete all english stopwords. See list: stopwords("english")
docs <- tm_map(docs, content_transformer(removePunctuation)) # Remove all punctuation
docs <- tm_map(docs, stemDocument, language = "english") # Delete common word endings, like -s, -ed, -ing, etc.
docs <- tm_map(docs, content_transformer(stripWhitespace)) # Reduce any whitespace (spaces, newlines, etc) to single spaces

# build a document-term matrix
dtm <- DocumentTermMatrix(docs)

findFreqTerms(dtm, lowfreq = 50)

findAssocs(dtm, terms = "tax", corlimit = 0.4)




dtm.ns <- removeSparseTerms(dtm, 1.00-0.02)

dtm.df <- as.data.frame(as.matrix(dtm.ns))









dtm.data <- cbind(speaker=ts$speaker, dtm.df) %>%
  filter(speaker != "Lehrer") %>%
  mutate(speaker = ifelse(as.character(speaker)=="Obama", 1, 0))


# create train/text indices
set.seed(3)
ndx <- sample(nrow(dtm.data), floor(nrow(dtm.data) * 0.7))
dtm.data.train <- dtm.data[ndx,]
dtm.data.test <- dtm.data[-ndx,]



model.logistic <- cv.glmnet(as.matrix(dtm.data.train[ , -1]),
                            as.factor(dtm.data.train[ , 1]),
                            family="binomial", type.measure="class", alpha=0.5)


model.logistic$lambda.min
coef(model.logistic, s = "lambda.min")
plot(model.logistic)
pred.logistic <- predict(model.logistic,
                         newx=as.matrix(dtm.data.test[ , -1]),
                         s="lambda.min", type="class")

confusionMatrix(data=pred.logistic,
                reference=dtm.data.test[ , 1],
                dnn=c("Prediction", "True Value"),
                positive="1")



GetInformativeWords <- function(crossval) {
  coefs <- coef(crossval, s="lambda.min")
  coefs <- as.data.frame(as.matrix(coefs))
  names(coefs) <- "weight"
  coefs$word <- row.names(coefs)
  row.names(coefs) <- NULL
  filter(coefs, weight != 0)
}


GetInformativeWords(model.logistic) %>% View()



