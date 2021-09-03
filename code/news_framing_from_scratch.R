library(dplyr)
library(tidyverse)
library(data.table)
library(here)
library(stm)
library(lubridate)
library(tidytext)

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

## check uniqueness of articles
# length(unique(mig_texts_raw$url)) # some duplicates

## load german stopwords
german_stopwords <- data.frame(word = c(stopwords::stopwords("de"), 'dass', 'sagte', 'sagt', 'sei'), stringsAsFactors = F)

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
# library(stmprinter)
# 
# mig_texts_raw <- mig_texts_raw %>% 
#   mutate(quarter = floor_date(date_new, unit = 'quarters'))
# 
# ## preprocess
# processed <- textProcessor(
#   documents = mig_texts_raw$text,
#   metadata = mig_texts_raw, 
#   language = 'german'
# )
# 
# out <- prepDocuments(
#   documents = processed$documents,
#   vocab = processed$vocab,
#   meta = processed$meta
# )
# 
# set.seed(42)
# 
# print(paste('Started at:', Sys.time()))
# 
# stm_models <- many_models(
#   K = c(40, 60, 80, 110, 150),
#   documents = out$documents,
#   vocab = out$vocab, 
#   data = out$meta,
#   N = 1,
#   runs = 10,
#   cores = 3
# )
# 
# print_models(
#   stm_models, mig_texts_raw$text,
#   file = here('code/mig_stm_runs.pdf'),
#   title = "Migration frames"
# )
# 
# print(paste('Finished at:', Sys.time()))


## alternative 2: use stm package to decide on K
# mig_texts_raw <- mig_texts_raw %>%
#   mutate(quarter = floor_date(date_new, unit = 'quarters'))

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
)

set.seed(42)

print(paste('Started at:', Sys.time()))

## Search K
print("Searching right number of topics...")
kResult <- searchK(out$documents, out$vocab, K = c(100, 60, 30, 10), init.type = "Spectral", prevalence =~ date_new + paper, data = out$meta)

# plot disgnostics
jpeg("paper/vis/plot1_kSearch_broad.jpeg")
plot(kResult)
dev.off()

# plot coherence-exclusivity
jpeg("paper/vis/plot2_kSearch_broad.jpeg")
plot(kResult$results$semcoh, kResult$results$exclus, xlab = 'Semantic Coherence', ylab = 'Exclusivity')
text(kResult$results$semcoh, kResult$results$exclus, labels = paste("K", kResult$results$K), pos = 1)
dev.off()

# coherence-exclusivity table
save(kResult$results, file = 'paper/vis/table_ksearch_broad.RData')


print(paste('Finished at:', Sys.time()))
