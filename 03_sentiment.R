
# Initialize ############################################################################

# load packages
library(readr)
library(dplyr)
library(ggplot2); theme_set(theme_bw())
library(scales)
library(forcats)
library(zoo)

# set working directory
setwd("~/Documents/presidential-debate-nlp/")

# load colors definitions
source("colors.R")




# Load data ############################################################################

# load transcript and sentiment data
transcript <- read_csv("data/vox_transcript.csv")
annotations.sentiment <- read_csv("data/annotations_sentiment.csv")




# Clean data ############################################################################

# join dataframes
transcript <- left_join(transcript, annotations.sentiment, by="text_id")

# create an approximate time, linearly interploated between 9pm and 10:30pm
transcript$time <- NA
transcript$time[1] <- "2016-09-26 21:00:00"
transcript$time[nrow(transcript)] <- "2016-09-26 22:30:00"
transcript <- transcript %>%
  mutate(time=as.POSIXct(time, tz = "America/New_York"),
         time=na.approx(time),
         time=as.POSIXct(time, tz = "America/New_York", origin = "1970-01-01 00:00:00"))

# categorize the sentiment polarity
transcript <- transcript %>%
  mutate(pnn=ifelse(polarity <= -0.75, -1,
                    ifelse(polarity >= 0.25, 1,
                           0)))




# Plots and statistical tests ############################################################################

# Polarity vs time, weighted by magnitude ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

ggplot(transcript %>% filter(speaker!="Holt"),
       aes(x=time, y=polarity, weight=magnitude,
           color=fct_relevel(speaker, "Clinton", "Trump"),
           label=text_id)) +
  geom_line(alpha=0.1) +
  geom_point(alpha=0.2, size=0.2) +
  geom_smooth(se=FALSE) + 
  scale_color_manual(values = c(colors$cp1, colors$tp1), guide = guide_legend(title = "Speaker")) +
  scale_y_continuous(name="Sentiment Polarity",
                     breaks = c(-1, 0, 1),
                     labels = c("Negative", "Neutral", "Positive")) +
  scale_x_datetime(name="Time (EDT)", labels=date_format("%I:%M %p", tz = "America/New_York")) +
  theme(panel.border = element_blank())
ggsave(filename = "plots/sentiment_polarity_vs_time.png", width = 8, height = 3, units = "in")


# Magnitude vs time ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

ggplot(transcript %>% filter(speaker!="Holt"),
       aes(x=time, y=magnitude,
           color=fct_relevel(speaker, "Clinton", "Trump"))) +
  geom_line(alpha=0.1) +
  geom_point(alpha=0.2, size=0.2) +
  geom_smooth(se=FALSE) + 
  scale_color_manual(values = c("blue", "red"), guide = guide_legend(title = "Speaker")) +
  scale_y_continuous(name="Sentiment Strength",
                     breaks = c(0, 5.7),
                     labels = c("Less   \nEmotion", "More   \nEmotion")) +
  scale_x_datetime(name="Time (EDT)", labels=date_format("%I:%M %p", tz = "America/New_York"))
ggsave(filename = "plots/sentiment_magnitude_vs_time.png", width = 8, height = 3, units = "in")



# Magnitude vs speaker ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

ggplot(transcript %>% filter(speaker!="Holt"),
       aes(x=speaker, y=magnitude)) +
  geom_boxplot(aes(fill=speaker,  color=speaker)) +
  scale_fill_manual(values = c("blue", "red"), guide = FALSE) +
  scale_color_manual(values = c("darkblue", "darkred"), guide = FALSE) +
  labs(y="Sentiment Strength", x=NULL)
ggsave(filename = "plots/sentiment_magnitude_vs_speaker.png", width = 4, height = 3, units = "in")

# rank sum test
wilcox.test(formula=magnitude~speaker, paired = FALSE,
            data=transcript %>% filter(speaker!="Holt" & pnn==1))



# Magnitude vs speaker and polarity ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 

ggplot(transcript %>% filter(speaker!="Holt"),
       aes(x=factor(pnn), y=magnitude)) +
  geom_boxplot(aes(fill=speaker,  color=speaker), position = position_dodge(0.82)) +
  scale_fill_manual(values = c("blue", "red"), guide = guide_legend(title = "Speaker")) +
  scale_color_manual(values = c("darkblue", "darkred"), guide = FALSE) +
  scale_x_discrete(name="Sentiment Polarity", labels = c("Negative", "Neutral", "Positive")) +
  labs(y="Sentiment Strength")
ggsave(filename = "plots/sentiment_magnitude_vs_speaker_polarity.png", width = 8, height = 3, units = "in")

# rank sum tests
wilcox.test(formula=magnitude~speaker, paired = FALSE,
            data=transcript %>% filter(speaker!="Holt" & pnn==-1))
wilcox.test(formula=magnitude~speaker, paired = FALSE,
            data=transcript %>% filter(speaker!="Holt" & pnn==0))
wilcox.test(formula=magnitude~speaker, paired = FALSE,
            data=transcript %>% filter(speaker!="Holt" & pnn==1))

# kruskal wallis test
kruskal.test(formula=magnitude~speaker_pnn,
             data=transcript %>% filter(speaker!="Holt" & pnn==-1) %>%
               mutate(speaker_pnn=factor(paste0(speaker,pnn))))



# Magnitude vs speaker while and answer/non-answer ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 

ggplot(transcript %>% filter(speaker!="Holt") %>%
         mutate(speaker_tt=paste(speaker, text_type, sep="_")),
       aes(x=factor(speaker), y=magnitude)) +
  geom_boxplot(aes(fill=speaker_tt,  color=speaker_tt), position = position_dodge(0.82)) +
  scale_fill_manual(values = c("#0000ff", "#6666ff", "#ff0000", "#ff6666"),
                    guide = FALSE, #guide_legend(title = "Speaker"),
                    labels = c("cy", "cn", "ty", "tn")) +
  scale_color_manual(values = c("darkblue", "#3d3d99", "darkred", "#b24747"), guide = FALSE) +
  scale_x_discrete(name=NULL, labels = c("Clinton\nAnswered / Didn't Answer", "Trump\nAnswered / Didn't Answer")) +
  labs(y="Sentiment Strength")
ggsave(filename = "plots/sentiment_magnitude_vs_speaker_answering.png", width = 8, height = 3, units = "in")

ggplot(transcript %>% filter(speaker!="Holt") %>%
         mutate(speaker_tt=paste(speaker, text_type, sep="_")),
       aes(x=factor(text_type), y=magnitude)) +
  geom_boxplot(aes(fill=speaker,  color=speaker), position = position_dodge(0.82)) +
  scale_fill_manual(values = c("blue", "red"), guide = guide_legend(title = "Speaker")) +
  scale_color_manual(values = c("darkblue", "darkred"), guide = FALSE) +
  scale_x_discrete(name="Sentiment", labels = c("Answering", "Not Answering")) +
  labs(y="Sentiment Strength")
ggsave(filename = "plots/sentiment_magnitude_vs_speaker_answering_v2.png", width = 8, height = 3, units = "in")

