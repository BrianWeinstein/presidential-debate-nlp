

library(httr)
library(jsonlite)
library(stringr)
library(ggplot2); theme_set(theme_bw())
library(forcats)



Sentiment <- function(text){
  
  if(is.na(text) | str_trim(text, side = "both")=="") {
    
    blank.sentiment <- data.frame(probability.neg = as.numeric(NA),
                                  probability.neutral = as.numeric(NA),
                                  probability.pos = as.numeric(NA),
                                  label = as.character(NA))
    
    return(blank.sentiment)
    
  } else {
    
    response <- POST(url = "http://text-processing.com/api/sentiment/",
                     body = paste0("text=", text),
                     encode = "form")
    
    if(response$status_code == 200){
      
      sentiment <- fromJSON(toJSON(httr::content(response), digits=5), flatten=TRUE)
      sentiment <- as.data.frame(sentiment, stringsAsFactors = FALSE)
      
      return(sentiment)
      
    } else {
      
      blank.sentiment <- data.frame(probability.neg = as.numeric(NA),
                                    probability.neutral = as.numeric(NA),
                                    probability.pos = as.numeric(NA),
                                    label = as.character(NA))
      
      return(blank.sentiment)      
    }
    
  }
  
}



GetSentiment <- function(text){
  
  sentiment.list <- lapply(text, Sentiment)
  
  sentiment.df <- do.call(rbind, sentiment.list)
  
  return(sentiment.df)
  
}



# ex <- GetSentiment(c("this is a test", "other work", "  ", NA, "this"))
# 
# GetSentiment("asdf")
# 
# rm(ex)



# get sentiment by id / id_line

sentiments <- GetSentiment(transcript$text)
ts <- cbind(transcript, sentiments)

ts$time <- NA
ts$time[1] <- "2016-09-26 21:00:00 ET"
ts$time[508] <- "2016-09-26 22:30:00 ET"
ts$time <- as.POSIXct(ts$time)
ts$time <- na.approx(ts$time)
ts$time <- as.POSIXct(ts$time, origin = "1969-12-31 20:00:00")

sentiments.grouped <- GetSentiment(transcript.grouped$text)
ts.grouped <- cbind(transcript.grouped, sentiments.grouped)


# plot sentiment vs id/id_line

ggplot(ts %>% filter(speaker!="Holt"), aes(x=time, y=probability.pos, color=fct_relevel(speaker, "Trump", "Holt", "Clinton"))) +
  geom_line(alpha=0.2) + geom_point(alpha=0.2, size=0.2) +
  geom_smooth(se=FALSE) + 
  scale_color_discrete(guide = guide_legend(title = "Speaker")) +
  scale_y_continuous(name="Probability of Positive Sentiment", labels = scales::percent) +
  scale_x_datetime(name="Time", labels=scales::date_format("%I:%M %p"))
# scale_x_continuous(name="Time", labels = c("9:00 PM ET", "10:30 PM ET"), breaks = c(0, 508))
ggsave(filename = "prob_pos.png", width = 8, height = 3, units = "in")
plotly::ggplotly()

ggplot(ts.grouped, aes(x=id, y=probability.pos, color=fct_relevel(speaker, "Trump", "Holt", "Clinton"))) +
  geom_line(alpha=0.2) + geom_point(alpha=0.2, size=0.2) +
  geom_smooth(se=FALSE) +
  scale_color_discrete(guide = guide_legend(title = "Speaker")) +
  scale_y_continuous(name="Probability of Positive Sentiment", labels = scales::percent) +
  scale_x_continuous(name="Response Number")



save.image("02_sentiment.RData")

transcript %>%
  group_by(speaker) %>%
  summarize(wc=sum(word_count),
            swc=sum(stop_word_count)) %>%
  mutate(pct=swc/wc)


ts %>%
  group_by(speaker) %>%
  summarize(min(probability.pos), median(probability.pos),
            mean(probability.pos), sd(probability.pos), max(probability.pos))

