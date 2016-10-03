
# Initialize ############################################################################

# load packages
library(rvest)
library(dplyr)
library(jsonlite)
library(stringr)
library(zoo)

# set working directory
setwd("~/Documents/presidential-debate-nlp/")




# Get data ############################################################################

# read in transcript html
transcript.raw <- read_html("https://apps.voxmedia.com/graphics/vox-debate-category-2016-1/?initialWidth=750&childId=this-is-every-time-clinton-and-trump-did-not-answer-the-question__graphic&parentUrl=http%3A%2F%2Fwww.vox.com%2Fdebates%2F2016%2F9%2F27%2F13070616%2Fdebate-clinton-trump-not-answers%2Fin%2F12771101")
transcript.raw <- transcript.raw %>%
  html_nodes("script") %>%
  html_text()
transcript.raw <- transcript.raw[4]

# copy to a second dataframe
transcript <- transcript.raw

# remove the non-json parts of the string
transcript <- transcript %>%
  str_replace(pattern = "^\\n\\tvar data = ", replacement = "") %>%
  str_replace(pattern = ";\n$", replacement = "")

# convert json to dataframe
transcript <- fromJSON(toJSON(fromJSON(transcript))) %>%
  flatten(recursive = TRUE) %>%
  as.data.frame()

# rename columns
transcript <- transcript %>%
  rename(speaker=s, text=l, text_type=t)

# correct a few data issues
transcript <- transcript %>%
  mutate(text_type=ifelse(speaker=="HOLT" & text_type=="n", "q", text_type)) %>% # fixes "Please just take 30..." and "OK, you are unpacking a lot here..."
  mutate(text_type=ifelse(speaker=="TRUMP" & text_type=="q", "y", text_type)) # fixes "The other day, we were deporting..."

# convert speaker names to title case
transcript <- transcript %>%
  mutate(speaker=str_to_title(speaker))

# create line and question numbers
transcript <- transcript %>%
  mutate(question_id=ifelse(text_type=="qq", row_number(), NA),
         question_id=dense_rank(question_id),
         question_id=na.locf(question_id, na.rm=FALSE),
         question_id=ifelse(is.na(question_id), 0, question_id),
         question_id=question_id+1,
         text_id=row_number())

# reorder columns
transcript <- transcript %>%
  select(text_id, question_id, speaker, text, text_type)




# Output data ############################################################################

# write to csv
write.csv(transcript, file = "data/vox_transcript.csv", row.names = FALSE)
