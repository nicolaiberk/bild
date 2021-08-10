library(dplyr)
library(tidyverse)
library(data.table)
library(here)
library(stm)
library(lubridate)
library(tidytext)

# load the preselected migration articles
mig_texts_raw <- fread(here('data/_migration_articles_new.csv'), encoding = 'UTF-8') %>% as_tibble()


# use subsample for now
# mig_texts_raw <- mig_texts_raw[sample(1:nrow(mig_texts_raw), 10000),]

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

## check uniqueness of articles
# length(unique(mig_texts_raw$url)) # some duplicates

## load german stopwords
# german_stopwords <- data.frame(word = c(stopwords("de"), 'dass', 'sagte', 'sagt', 'sei'), stringsAsFactors = F)

# ## preprocess: sort out non-unique cases, tokenize, remove stopwords
# mig_texts_tidy <- mig_texts_raw %>%
#   group_by(url) %>% 
#   summarise(date_new = first(date_new),
#             paper = first(paper),
#             title = first(title),
#             text = first(text)
#             ) %>%
#   mutate(article = row_number()) %>% 
#   unnest_tokens(word, text) %>% 
#   anti_join(german_stopwords) # missing in stopwords
# 
# mig_dfm <- mig_texts_tidy %>% 
#   count(article, word, sort = T) %>% 
#   cast_dfm(article, word, n)
# 
# 
# topic_model <- stm(mig_dfm, K = 60, init.type = 'Spectral') # k = 0 chooses optimal number of topics 
# summary(topic_model)


# alternative approach: use stmprinter to find right k of topics
library(stmprinter)

mig_texts_raw <- mig_texts_raw %>% 
  mutate(month = floor_date(date_new, unit = 'months'))

## preprocess
processed <- textProcessor(
  documents = mig_texts_raw$text,
  metadata = mig_texts_raw, 
  language = 'german'
)

out <- prepDocuments(
  documents = processed$documents,
  vocab = processed$vocab,
  meta = processed$meta
)

set.seed(42)

stm_models <- many_models(
  K = seq(10, 100, 10),
  documents = out$documents,
  vocab = out$vocab,
  prevalence = ~ paper + month, 
  data = out$meta,
  N = 5,
  runs = 100,
  cores = 1 # bc f windows
)

print_models(
  stm_models, mig_texts_raw$text,
  file = here('_sc/_framing/mig_stm_runs.pdf'),
  title = "Migration frames"
)
