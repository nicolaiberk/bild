library(dplyr)
library(tidyverse)
library(data.table)
library(here)
library(stm)
library(lubridate)
library(tidytext)
library(ggthemes)
library(scales)
library(knitr)

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

# set.seed(42)
# topic_model <- stm(out$documents, out$vocab, K = 60, init.type = "Spectral", prevalence =~ s(date_new) + paper, data = out$meta)
# save(topic_model, file = 'topic_model_K60.Rdata')


# explore following Julia Silge's blogpost: https://juliasilge.com/blog/evaluating-stm/
load(here('data/topic_model_K60.Rdata'))
td_beta <- tidy(topic_model)
td_gamma <- tidy(topic_model, matrix = "gamma",
                 document_names = rownames(out$documents))


top_terms <- td_beta %>%
  arrange(beta) %>%
  group_by(topic) %>%
  top_n(7, beta) %>%
  arrange(-beta) %>%
  select(topic, term) %>%
  summarise(terms = list(term)) %>%
  mutate(terms = map(terms, paste, collapse = ", ")) %>%
  unnest(cols = c(terms))

gamma_terms <- td_gamma %>%
  group_by(topic) %>%
  summarise(gamma = mean(gamma)) %>%
  arrange(desc(gamma)) %>%
  left_join(top_terms, by = "topic") %>%
  mutate(topic = paste0("Topic ", topic),
         topic = reorder(topic, gamma))

gamma_terms %>%
  top_n(20, gamma) %>%
  ggplot(aes(topic, gamma, label = terms, fill = topic)) +
  geom_col(show.legend = FALSE) +
  geom_text(hjust = 0, nudge_y = 0.0005, size = 3,
            family = "IBMPlexSans") +
  coord_flip() +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0, 0.09),
                     labels = percent_format()) +
  theme_tufte(base_family = "IBMPlexSans", ticks = FALSE) +
  theme(plot.title = element_text(size = 16,
                                  family="IBMPlexSans-Bold"),
        plot.subtitle = element_text(size = 13)) +
  labs(x = NULL, y = expression(gamma),
       title = "Top 20 topics by prevalence in the Hacker News corpus",
       subtitle = "With the top words that contribute to each topic")

gamma_terms %>%
  select(topic, gamma, terms) %>%
  kable(digits = 3,
        col.names = c("Topic", "Expected topic proportion", "Top 7 terms"))

## label with stm function
labelTopics(topic_model)
sageLabels(topic_model)

## associated documents of selected topics
findThoughts(topic_model, texts = out$meta$text, n = 10, topics = )$docs[[1]]

## look into selected topic prevalence across time & newspapers

### crime 1: police & refugees
crime <- topic_model$theta[,25]
ggplot(mapping = aes(x=out$meta$date_new, y = crime))+
  geom_smooth()
ggplot(mapping = aes(x=out$meta$paper, y = log(crime)))+
  geom_boxplot()

### mediterranean route & drowning of refugees
medit <- topic_model$theta[,49] 
ggplot(mapping = aes(x=out$meta$date_new, y = medit))+
  geom_smooth()

### number of asylum seekers arriving
refnums <- topic_model$theta[,32] 
ggplot(mapping = aes(x=out$meta$date_new, y = refnums))+
  geom_smooth()

### deportation (Afghanistan)
deport <- topic_model$theta[,44] 
ggplot(mapping = aes(x=out$meta$date_new, y = deport))+
  geom_smooth()

### local refugee accommodation
accomm <- topic_model$theta[,58] 
ggplot(mapping = aes(x=out$meta$date_new, y = accomm))+
  geom_smooth()

### greek internment camps
camps <- topic_model$theta[,27] 
ggplot(mapping = aes(x=out$meta$date_new, y = camps))+
  geom_smooth()

### labour market integration
labmar <- topic_model$theta[,11] 
ggplot(mapping = aes(x=out$meta$date_new, y = labmar))+
  geom_smooth()

### crime 2: capital crimes (rape, murder, cologne)
capcrime <- topic_model$theta[,48] 
ggplot(mapping = aes(x=out$meta$date_new, y = capcrime))+
  geom_smooth()
ggplot(mapping = aes(x=out$meta$paper, fill = out$meta$paper, y = log(capcrime)))+
  geom_violin()

ggplot(mapping = aes(x=out$meta$date_new, y = capcrime+crime))+
  geom_smooth()
ggplot(mapping = aes(x=out$meta$paper, fill = out$meta$paper, y = log(capcrime+crime)))+
  geom_violin()


### generate output file with daily prevalence
prevalence <- 
  data.frame(
    crime, medit, deport, refnums, camps, labmar, capcrime
  ) %>% 
  cbind(paper = out$meta$paper) %>% 
  cbind(date_new = out$meta$date_new) %>% 
  group_by(paper, date_new) %>% 
  summarise(
    crime_m    = mean(crime),
    medit_m    = mean(medit),
    deport_m   = mean(deport),
    refnums_m  = mean(refnums),
    camps_m    = mean(camps),
    labmar_m   = mean(labmar),
    capcrime_m = mean(capcrime),
    crime_s    = sum(crime),
    medit_s    = sum(medit),
    deport_s   = sum(deport),
    refnums_s  = sum(refnums),
    camps_s    = sum(camps),
    labmar_s   = sum(labmar),
    capcrime_s = sum(capcrime),
    crime_sd   = sd(crime),
    medit_sd   = sd(medit),
    deport_sd  = sd(deport),
    refnums_sd = sd(refnums),
    camps_sd   = sd(camps),
    labmar_sd  = sd(labmar),
    capcrime_sd = sd(capcrime)
    )

save(prevalence, file = here('data/daily_mig_topics.Rdata'))

# save meta with topic attention
topic_attention <- 
  data.frame(
    crime, medit, deport, refnums, camps, labmar, capcrime
    )

output <- cbind(out$meta, topic_attention)
save(output, file = here('data/mig_articles_topics.Rdata'))
