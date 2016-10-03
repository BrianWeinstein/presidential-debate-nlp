
# Initialize ############################################################################

# load packages
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(broom)
library(forcats)
library(ggplot2); theme_set(theme_bw())
library(scales)


# formatting for scientific notation
options(scipen = 50, digits = 7)

# set working directory
setwd("~/Documents/presidential-debate-nlp/")

# load LogisticRegression helper function
source("logistic_helper.R")

# load colors definitions
source("colors.R")




# Load data ############################################################################

# load transcript and sentiment data
transcript <- read_csv("data/vox_transcript.csv")
annotations.tokens <- read_csv("data/annotations_tokens.csv")
tokens.ngrams <- read_csv("data/tokens_ngrams.csv")

# join dataframes
annotations.tokens <- left_join(transcript %>% mutate(text=NULL), annotations.tokens, by="text_id")
tokens.ngrams <- inner_join(transcript %>% mutate(text=NULL), tokens.ngrams, by="text_id")




# Does the % of time answering questions differ for each candidate ############################################################################

# pct of each candidate's words used to answer the questions
answer.pct <- annotations.tokens %>%
  filter(speaker != "Holt") %>%
  group_by(speaker, text_type) %>%
  summarize(num_words=n()) %>%
  ungroup() %>%
  mutate(speaker=fct_relevel(speaker, "Trump", "Clinton"),
         text_type=fct_relevel(text_type, "n", "y"),
         stt=paste(speaker, text_type, sep = "_"),
         stt=fct_relevel(stt, "Clinton_y", "Clinton_n", "Trump_y", "Trump_n"))

answer.pct.wide <- answer.pct %>%
  mutate(stt=NULL) %>%
  spread(key = text_type, value = num_words) %>%
  mutate(yes_pct=y/(y+n))

answer.pct.wide

# z test for equality of proportions
prop.test(x=as.matrix(answer.pct.wide[ c("y", "n")]))

# plot
ggplot(answer.pct %>% arrange(stt), aes(x=speaker, y=num_words, fill=stt)) +
  geom_bar(position="fill", stat="identity") +
  scale_fill_manual(values = c(colors$cp1, colors$cp2, colors$tp1, colors$tp2),
                    labels=c("Clinton answering", "Clinton not answering",
                             "Trump answering", "Trump not answering"),
                    name=NULL) +
  scale_y_continuous(name = "Percent of Words", labels = percent_format()) +
  labs(x="Speaker") +
  coord_flip() +
  theme(panel.border = element_blank())
ggsave(filename = "plots/speaker_vs_answering.png", width = 8, height = 3, units = "in")






# Compare nominal subjects ############################################################################

# Clinton: calculate the most common subjects of her sentences when answering / not answering
subjects.clinton <- annotations.tokens %>%
  filter(dependencyEdge.label=="NSUBJ") %>%
  filter(speaker=="Clinton") %>%
  mutate(lemma=str_to_lower(lemma)) %>%
  group_by(text_type, lemma) %>%
  summarize(num=n()) %>%
  spread(key=text_type, value=num, fill = 0) %>%
  mutate(n_sum=sum(n), y_sum=sum(y),
         n_pct=n/n_sum, n_pct_rank=min_rank(-n_pct),
         y_pct=y/y_sum, y_pct_rank=min_rank(-y_pct)) %>%
  arrange(n_pct_rank)

# Clinton: compare each the y vs n subject proportions
subjects.clinton.compare <- subjects.clinton %>%
  filter(n >= 10 & y >= 10) %>% # prop.test only accurate for the larger counts
  group_by(lemma) %>%
  do(tidy(prop.test(x=c(.$y, .$n),
                    n=c(.$y_sum, .$n_sum))))


subjects.clnton.plot <- subjects.clinton %>%
  filter(lemma %in% c("i", "we")) %>%
  mutate(lemma=ifelse(lemma=="i", "I", lemma)) %>%
  select(lemma, n, y) %>%
  gather(key = text_type, value = mentions, c(y, n)) %>%
  mutate(ttl=paste(text_type, lemma, sep="_"),
         ttl=fct_relevel(ttl, "y_I", "y_we", "n_I", "n_we"),
         text_type=fct_relevel(text_type, "y", "n"),
         lemma=fct_relevel(lemma, "I", "we"))

ggplot(subjects.clnton.plot, aes(x=text_type, y=mentions, fill=ttl)) +
  geom_bar(stat="identity", position=position_dodge(0.92)) +
  scale_fill_manual(values = c(colors$cp1, colors$cp2, colors$cs1, colors$cs2)) +
  theme(panel.border = element_blank())
ggsave(filename = "plots/yn_iwe_clinton.png", width = 5, height = 3, units = "in")


# Trump: calculate the most common subjects of his sentences when answering / not answering
subjects.trump <- annotations.tokens %>%
  filter(dependencyEdge.label=="NSUBJ") %>%
  filter(speaker=="Trump") %>%
  mutate(lemma=str_to_lower(lemma)) %>%
  group_by(text_type, lemma) %>%
  summarize(num=n()) %>%
  spread(key=text_type, value=num, fill = 0) %>%
  mutate(n_sum=sum(n), y_sum=sum(y),
         n_pct=n/n_sum, n_pct_rank=min_rank(-n_pct),
         y_pct=y/y_sum, y_pct_rank=min_rank(-y_pct)) %>%
  arrange(n_pct_rank)

# Trump: compare each the y vs n subject proportions
subjects.trump.compare <- subjects.trump %>%
  filter(n >= 10 & y >= 10) %>% # prop.test only accurate for the larger counts
  group_by(lemma) %>%
  do(tidy(prop.test(x=c(.$y, .$n),
                    n=c(.$y_sum, .$n_sum))))

subjects.trump.plot <- subjects.trump %>%
  filter(lemma %in% c("i", "we")) %>%
  mutate(lemma=ifelse(lemma=="i", "I", lemma)) %>%
  select(lemma, n, y) %>%
  gather(key = text_type, value = mentions, c(y, n)) %>%
  mutate(ttl=paste(text_type, lemma, sep="_"),
         ttl=fct_relevel(ttl, "y_I", "y_we", "n_I", "n_we"),
         text_type=fct_relevel(text_type, "y", "n"),
         lemma=fct_relevel(lemma, "I", "we"))

ggplot(subjects.trump.plot, aes(x=text_type, y=mentions, fill=ttl)) +
  geom_bar(stat="identity", position=position_dodge(0.92)) +
  scale_fill_manual(values = c(colors$tp1, colors$tp2, colors$ts1, colors$ts2)) +
  theme(panel.border = element_blank())
ggsave(filename = "plots/yn_iwe_trump.png", width = 5, height = 3, units = "in")


# Compare adjectives ############################################################################

# Clinton: calculate the most common adjectives for y vs n
adjectives.clinton <- annotations.tokens %>%
  filter(partOfSpeech.tag=="ADJ") %>%
  filter(speaker=="Clinton") %>%
  mutate(lemma=str_to_lower(lemma)) %>%
  group_by(text_type, lemma) %>%
  summarize(num=n()) %>%
  spread(key=text_type, value=num, fill = 0) %>%
  mutate(n_sum=sum(n), y_sum=sum(y),
         n_pct=n/n_sum, n_pct_rank=min_rank(-n_pct),
         y_pct=y/y_sum, y_pct_rank=min_rank(-y_pct)) %>%
  arrange(n_pct_rank)

# Clinton: compare each the y vs n adjectives proportions
adjectives.clinton.compare <- adjectives.clinton %>%
  filter(n >= 10 & y >= 10) %>% # prop.test only accurate for the larger counts
  group_by(lemma) %>%
  do(tidy(prop.test(x=c(.$y, .$n),
                    n=c(.$y_sum, .$n_sum))))

# Trump: calculate the most common adjectives for y vs n
adjectives.trump <- annotations.tokens %>%
  filter(partOfSpeech.tag=="ADJ") %>%
  filter(speaker=="Trump") %>%
  mutate(lemma=str_to_lower(lemma)) %>%
  group_by(text_type, lemma) %>%
  summarize(num=n()) %>%
  spread(key=text_type, value=num, fill = 0) %>%
  mutate(n_sum=sum(n), y_sum=sum(y),
         n_pct=n/n_sum, n_pct_rank=min_rank(-n_pct),
         y_pct=y/y_sum, y_pct_rank=min_rank(-y_pct)) %>%
  arrange(n_pct_rank)

# Trump: compare each the y vs n adjectives proportions
adjectives.trump.compare <- adjectives.trump %>%
  filter(n >= 10 & y >= 10) %>% # prop.test only accurate for the larger counts
  group_by(lemma) %>%
  do(tidy(prop.test(x=c(.$y, .$n),
                    n=c(.$y_sum, .$n_sum))))




# Common bigrams, trigrams ############################################################################

words.bigrams.clinton <- tokens.ngrams %>%
  filter(speaker == "Clinton" & !is.na(bigram)) %>%
  group_by(text_type, bigram) %>%
  summarize(count=n()) %>%
  spread(key = text_type, value=count) %>%
  arrange(-n)

words.trigrams.clinton <- tokens.ngrams %>%
  filter(speaker == "Clinton" & !is.na(trigram)) %>%
  group_by(text_type, trigram) %>%
  summarize(count=n()) %>%
  spread(key = text_type, value=count) %>%
  arrange(-n)

natopics.clinton <- data.frame(topic=c("middle class", "Barack Obama", "defeat ISIS",
                                       "Secretary of State", "american troops"),
                               mentions=c(6, 3, 3, 3, 2),
                               stringsAsFactors = FALSE)
ggplot(natopics.clinton,
       aes(x=fct_reorder(topic, mentions), y=mentions)) +
  geom_bar(stat = "identity", fill=colors$cp1) +
  labs(x=NULL, y="Frequency", title = "Clinton non-answer phrase frequency") +
  coord_flip() +
  # geom_text(aes(label=mentions), hjust=-0.0) +
  theme(panel.border = element_blank())
ggsave(filename = "plots/natopics_clinton.png", width = 5, height = 3, units = "in")



words.bigrams.trump <- tokens.ngrams %>%
  filter(speaker == "Trump" & !is.na(bigram)) %>%
  group_by(text_type, bigram) %>%
  summarize(count=n()) %>%
  spread(key = text_type, value=count) %>%
  arrange(-n)

words.trigrams.trump <- tokens.ngrams %>%
  filter(speaker == "Trump" & !is.na(trigram)) %>%
  group_by(text_type, trigram) %>%
  summarize(count=n()) %>%
  spread(key = text_type, value=count) %>%
  arrange(-n)

natopics.trump <- data.frame(topic=c("Secretary Clinton", "Sean Hannity", "Middle East",
                                     "trade deal", "bring money\n(back into the US)"),
                             mentions=c(17, 7, 6, 6, 4),
                             stringsAsFactors = FALSE)

ggplot(natopics.trump,
       aes(x=fct_reorder(topic, mentions), y=mentions)) +
  geom_bar(stat = "identity", fill=colors$tp1) +
  labs(x=NULL, y="Frequency", title = "Trump non-answer phrase frequency") +
  coord_flip() +
  # geom_text(aes(label=mentions), hjust=-0.0) +
  theme(panel.border = element_blank())
ggsave(filename = "plots/natopics_trump.png", width = 5, height = 3, units = "in")


# Clinton: Classify answer vs not answer ############################################################################

# create a document term matrix
dtm.df.clinton <- annotations.tokens %>%
  filter(partOfSpeech.tag %in% c("ADJ", "ADV", "NOUN", "PRON", "VERB")) %>%
  filter(speaker == "Clinton") %>%
  mutate(text_type = ifelse(text_type=="y", 1, 0)) %>%
  rename(answered=text_type) %>%
  mutate(lemma=str_to_lower(lemma)) %>%
  group_by(text_id, answered, lemma) %>%
  summarize(occurrences=n()) %>%
  ungroup() %>%
  spread(key = lemma, value = occurrences, fill = 0) %>%
  mutate(text_id=NULL)

# perform logistic regression with lasso
logistic.compare.clinton <- LogisticRegression(input.data = dtm.df.clinton,
                                               positive.class = "1",
                                               type.measure="class",
                                               sparsity = 0.95)

# get coefficients
logistic.compare.clinton$coefs %>% View

# get performance metrics
logistic.compare.clinton$confusion.matrix




# Clinton: Classify answer vs not answer, with bigrams ############################################################################

# create a document term matrix
dtm.df.clinton.bi <- tokens.ngrams %>%
  filter(speaker=="Clinton" & !is.na(bigram)) %>%
  mutate(text_type = ifelse(text_type=="y", 1, 0)) %>%
  rename(answered=text_type) %>%
  count(text_id, answered, bigram) %>%
  ungroup() %>%
  spread(key = bigram, value = n, fill = 0) %>%
  mutate(text_id=NULL)

# perform logistic regression with lasso
logistic.compare.clinton.bi <- LogisticRegression(input.data = dtm.df.clinton.bi,
                                                  positive.class = "1",
                                                  type.measure="class", 
                                                  sparsity = 0.985)

# get coefficients
logistic.compare.clinton.bi$coefs %>% View

# get performance metrics
logistic.compare.clinton.bi$confusion.matrix




# Clinton: Classify answer vs not answer, with trigrams ############################################################################

# create a document term matrix
dtm.df.clinton.tri <- tokens.ngrams %>%
  filter(speaker=="Clinton" & !is.na(trigram)) %>%
  mutate(text_type = ifelse(text_type=="y", 1, 0)) %>%
  rename(answered=text_type) %>%
  count(text_id, answered, trigram) %>%
  ungroup() %>%
  spread(key = trigram, value = n, fill = 0) %>%
  mutate(text_id=NULL)

# perform logistic regression with lasso
logistic.compare.clinton.tri <- LogisticRegression(input.data = dtm.df.clinton.tri,
                                                   positive.class = "1",
                                                   type.measure="class", 
                                                   sparsity = 0.99)

# get coefficients
logistic.compare.clinton.tri$coefs %>% View

# get performance metrics
logistic.compare.clinton.tri$confusion.matrix




# Trump: Classify answer vs not answer ############################################################################

# create a document term matrix
dtm.df.trump <- annotations.tokens %>%
  filter(partOfSpeech.tag %in% c("ADJ", "ADV", "NOUN", "PRON", "VERB")) %>%
  filter(speaker == "Trump") %>%
  mutate(text_type = ifelse(text_type=="y", 1, 0)) %>%
  rename(answered=text_type) %>%
  mutate(lemma=str_to_lower(lemma)) %>%
  group_by(text_id, answered, lemma) %>%
  summarize(occurrences=n()) %>%
  ungroup() %>%
  spread(key = lemma, value = occurrences, fill = 0) %>%
  mutate(text_id=NULL)

# perform logistic regression with lasso
logistic.compare.trump <- LogisticRegression(input.data = dtm.df.trump,
                                             positive.class = "1",
                                             type.measure="mae",
                                             sparsity = 0.95)

# get coefficients
logistic.compare.trump$coefs %>% View

# get performance metrics
logistic.compare.trump$confusion.matrix




# Trump: Classify answer vs not answer, with bigrams ############################################################################

# create a document term matrix
dtm.df.trump.bi <- tokens.ngrams %>%
  filter(speaker=="Trump" & !is.na(bigram)) %>%
  mutate(text_type = ifelse(text_type=="y", 1, 0)) %>%
  rename(answered=text_type) %>%
  count(text_id, answered, bigram) %>%
  ungroup() %>%
  spread(key = bigram, value = n, fill = 0) %>%
  mutate(text_id=NULL)

# perform logistic regression with lasso
logistic.compare.trump.bi <- LogisticRegression(input.data = dtm.df.trump.bi,
                                                positive.class = "1",
                                                type.measure="mae", 
                                                sparsity = 0.995)

# get coefficients
logistic.compare.trump.bi$coefs %>% View

# get performance metrics
logistic.compare.trump.bi$confusion.matrix




# Trump: Classify answer vs not answer, with trigrams ############################################################################

# create a document term matrix
dtm.df.trump.tri <- tokens.ngrams %>%
  filter(speaker=="Trump" & !is.na(trigram)) %>%
  mutate(text_type = ifelse(text_type=="y", 1, 0)) %>%
  rename(answered=text_type) %>%
  count(text_id, answered, trigram) %>%
  ungroup() %>%
  spread(key = trigram, value = n, fill = 0) %>%
  mutate(text_id=NULL)

# perform logistic regression with lasso
logistic.compare.trump.tri <- LogisticRegression(input.data = dtm.df.trump.tri,
                                                 positive.class = "1",
                                                 type.measure="mae", 
                                                 sparsity = 0.99)

# get coefficients
logistic.compare.trump.tri$coefs %>% View

# get performance metrics
logistic.compare.trump.tri$confusion.matrix
