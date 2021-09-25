library(tidyverse)
library(data.table)
library(here)
library(stm)
library(lubridate)
library(tidytext)
library(ggthemes)
library(scales)
library(knitr)
library(dplyr)

# load the preselected migration articles
# mig_texts_raw <- fread(here('data/_migration_articles_new.csv'), encoding = 'UTF-8') %>% as_tibble()
mig_texts_raw <- fread(here('data/_migration_articles_BERT.csv'), encoding = 'UTF-8') %>% as_tibble()


# use subsample for now
# mig_texts_raw <- mig_texts_raw[sample(1:nrow(mig_texts_raw), 1000),]

# fix dates
mig_texts_raw$paper[mig_texts_raw$paper == "weltonline"] <- "welt"
mig_texts_raw$date_new <- NA_Date_
mig_texts_raw$date_new[mig_texts_raw$paper == "bild"] <- mig_texts_raw$date[mig_texts_raw$paper == "bild"] %>% as.Date()
mig_texts_raw$date_new[mig_texts_raw$paper == "faz"] <- mig_texts_raw$date[mig_texts_raw$paper=="faz"] %>% as.numeric() %>% as.Date(origin = "1970-01-01")
mig_texts_raw$date_new[mig_texts_raw$paper == "spon"] <- mig_texts_raw$date[mig_texts_raw$paper=="spon"] %>% as.Date(format = "%d.%m.%Y")
mig_texts_raw$date_new[mig_texts_raw$paper == "sz"] <- mig_texts_raw$date[mig_texts_raw$paper=="sz"] %>% as.integer() %>% as.Date(origin = "1970-01-01")
mig_texts_raw$date_new[mig_texts_raw$paper == "taz"] <- mig_texts_raw$date[mig_texts_raw$paper=="taz"] %>% as.integer() %>% as.Date(origin = "1970-01-01")
mig_texts_raw$date_new[mig_texts_raw$paper == "welt"] <- mig_texts_raw$date[mig_texts_raw$paper=="welt"] %>% as.integer() %>% as.Date(origin = "1970-01-01")
mig_texts_raw <- mig_texts_raw[mig_texts_raw$date_new > as.Date('2010-01-01'),]

## load german stopwords
german_stopwords <- data.frame(word = c(stopwords::stopwords("de"), 'dass', 'sagte', 'sagt', 'sei'), stringsAsFactors = F)


## preprocess
processed <- textProcessor(
  documents = mig_texts_raw$text,
  metadata = mig_texts_raw,
  language = 'german',
  customstopwords = german_stopwords$word,
  stem = F
)

out <- prepDocuments(
  documents = processed$documents,
  vocab = processed$vocab,
  meta = processed$meta
);rm(processed)
save(out, file = here('data/out_docs.Rdata'))

# set.seed(42)
topic_model <- stm(out$documents, out$vocab, K = 60, init.type = "Spectral", prevalence =~ s(date_new) + paper, data = out$meta)
save(topic_model, file = here('topic_model_K60.Rdata'))


