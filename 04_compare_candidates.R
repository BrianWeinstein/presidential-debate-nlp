
# Initialize ############################################################################

# load packages
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(broom)
library(forcats)
library(ggplot2); theme_set(theme_bw())

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

# examine lemma replacements
annotations.tokens %>%
  group_by(lemma) %>%
  summarize(words=paste0(unique(text.content), collapse=", "),
            num_words=length(unique(text.content))) %>%
  arrange(-num_words)



# Compare nominal subjects ############################################################################

# calculate the most common subjects of each candidate's sentences
subjects <- annotations.tokens %>%
  filter(dependencyEdge.label=="NSUBJ") %>%
  mutate(lemma=str_to_lower(lemma)) %>%
  group_by(speaker, lemma) %>%
  summarize(num=n()) %>%
  spread(key=speaker, value=num, fill = 0) %>%
  mutate(Trump_pct=Trump/sum(Trump),
         Trump_pct_rank=min_rank(-Trump_pct),
         Clinton_pct=Clinton/sum(Clinton),
         Clinton_pct_rank=min_rank(-Clinton_pct),
         Holt=NULL) %>%
  mutate(Clinton_sum=sum(Clinton),
         Trump_sum=sum(Trump)) %>%
  arrange(Trump_pct_rank)


# # z test for equality of candidates' subject proportions # http://stats.stackexchange.com/questions/2391/what-is-the-relationship-between-a-chi-squared-test-and-test-of-equal-proportion
subjects.compare <- subjects %>%
  filter(Clinton >= 10 & Trump >= 10) %>% # prop.test only accurate for the larger counts
  group_by(lemma) %>%
  do(tidy(prop.test(x=c(.$Clinton, .$Trump),
                    n=c(.$Clinton_sum, .$Trump_sum))))

# fisher.test more accurate for smaller counts # http://stats.stackexchange.com/questions/123609/exact-two-sample-proportions-binomial-test-in-r-and-some-strange-p-values
# fisher.test(
#   matrix(c(subjects$Clinton[15],
#            subjects$Clinton_sum[15]-subjects$Clinton[15],
#            subjects$Trump[15],
#            subjects$Trump_sum[15]-subjects$Trump[15]),
#          ncol=2))

# calculate the most common adjectives for each candidate
adjectives <- annotations.tokens %>%
  filter(partOfSpeech.tag=="ADJ") %>%
  mutate(lemma=str_to_lower(lemma)) %>%
  group_by(speaker, lemma) %>%
  summarize(num=n()) %>%
  spread(key=speaker, value=num, fill = 0) %>%
  mutate(Trump_pct=Trump/sum(Trump),
         Trump_pct_rank=min_rank(-Trump_pct),
         Clinton_pct=Clinton/sum(Clinton),
         Clinton_pct_rank=min_rank(-Clinton_pct),
         Holt=NULL) %>%
  mutate(Clinton_sum=sum(Clinton),
         Trump_sum=sum(Trump)) %>%
  arrange(Trump_pct_rank)

# Compare each candidates' adjectives # http://stats.stackexchange.com/questions/2391/what-is-the-relationship-between-a-chi-squared-test-and-test-of-equal-proportion
adjectives.compare <- adjectives %>%
  filter(Clinton >= 10 & Trump >= 10) %>% # prop.test only accurate for the larger counts
  group_by(lemma) %>%
  do(tidy(prop.test(x=c(.$Clinton, .$Trump),
                    n=c(.$Clinton_sum, .$Trump_sum))))

# plot
adjectives.plot <- adjectives %>%
  select(lemma, Clinton, Trump) %>%
  gather(key = speaker, value = mentions, c(Clinton, Trump)) %>%
  group_by(speaker) %>%
  filter(row_number(-mentions) <= 10) %>%
  rbind(data.frame(lemma="braggadocios", speaker="Trump", mentions=1)) %>%
  as.data.frame() %>%
  group_by(speaker) %>%
  arrange(-mentions)

ggplot(adjectives.plot %>% filter(speaker=="Clinton"),
       aes(x=fct_reorder(lemma, mentions), y=mentions, fill=speaker)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values=colors$cp1, guide=FALSE) +
  labs(x=NULL, y="Frequency", title = "Clinton Adjective Frequency") +
  coord_flip() +
  # geom_text(aes(label=mentions), hjust=-0.0) +
  theme(panel.border = element_blank())
ggsave(filename = "plots/adjectives_clinton.png", width = 5, height = 3, units = "in")

ggplot(adjectives.plot %>% filter(speaker=="Trump"),
       aes(x=fct_reorder(lemma, mentions), y=mentions, fill=speaker)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values=colors$tp1, guide=FALSE) +
  labs(x=NULL, y="Frequency", title = "Trump Adjective Frequency") +
  coord_flip() +
  # geom_text(aes(label=mentions), hjust=-0.0) +
  theme(panel.border = element_blank())
ggsave(filename = "plots/adjectives_trump.png", width = 5, height = 3, units = "in")




# Common bigrams, trigrams ############################################################################

words.bigrams <- tokens.ngrams %>%
  filter(speaker != "Holt" & !is.na(bigram)) %>%
  group_by(speaker, bigram) %>%
  summarize(count=n()) %>%
  spread(key = speaker, value=count) %>%
  arrange(-Clinton)

words.trigrams <- tokens.ngrams %>%
  filter(speaker != "Holt" & !is.na(trigram)) %>%
  group_by(speaker, trigram) %>%
  summarize(count=n()) %>%
  spread(key = speaker, value=count) %>%
  arrange(-Clinton)




# Classify Clinton vs Trump ############################################################################

# create a document term matrix
dtm.df <- annotations.tokens %>%
  filter(partOfSpeech.tag %in% c("ADJ", "ADV", "NOUN", "PRON", "VERB")) %>%
  filter(speaker != "Holt") %>%
  mutate(speaker = ifelse(speaker=="Clinton", 1, 0)) %>%
  rename(speaker_clinton=speaker) %>%
  mutate(lemma=str_to_lower(lemma)) %>%
  group_by(text_id, speaker_clinton, lemma) %>%
  summarize(occurrences=n()) %>%
  ungroup() %>%
  spread(key = lemma, value = occurrences, fill = 0) %>%
  mutate(text_id=NULL)

# perform logistic regression with lasso
logistic.compare.candidates <- LogisticRegression(input.data = dtm.df,
                                                  positive.class = "1",
                                                  type.measure="class",
                                                  sparsity = 0.98)

# get coefficients
logistic.compare.candidates$coefs %>% View

# get performance metrics
logistic.compare.candidates$confusion.matrix




# Classify Clinton vs Trump, with bigrams ############################################################################

# create a document term matrix
dtm.df.bi <- tokens.ngrams %>%
  filter(speaker!="Holt" & !is.na(bigram)) %>%
  count(text_id, speaker, bigram) %>%
  ungroup() %>%
  mutate(speaker = ifelse(speaker=="Clinton", 1, 0)) %>%
  rename(speaker_clinton=speaker) %>%
  spread(key = bigram, value = n, fill = 0) %>%
  mutate(text_id=NULL)

# perform logistic regression with lasso
logistic.compare.candidates.bi <- LogisticRegression(input.data = dtm.df.bi,
                                                     positive.class = "1",
                                                     type.measure="class", 
                                                     sparsity = 0.994)

# get coefficients
logistic.compare.candidates.bi$coefs %>% View

# get performance metrics
logistic.compare.candidates.bi$confusion.matrix




# Classify Clinton vs Trump, with trigrams ############################################################################

# create a document term matrix
dtm.df.tri <- tokens.ngrams %>%
  filter(speaker!="Holt" & !is.na(trigram)) %>%
  count(text_id, speaker, trigram) %>%
  ungroup() %>%
  mutate(speaker = ifelse(speaker=="Clinton", 1, 0)) %>%
  rename(speaker_clinton=speaker) %>%
  spread(key = trigram, value = n, fill = 0) %>%
  mutate(text_id=NULL)

# perform logistic regression with lasso
logistic.compare.candidates.tri <- LogisticRegression(input.data = dtm.df.tri,
                                           positive.class = "1",
                                           type.measure="class", 
                                           sparsity = 0.996)

# get coefficients
logistic.compare.candidates.tri$coefs %>% View

# get performance metrics
logistic.compare.candidates.tri$confusion.matrix
