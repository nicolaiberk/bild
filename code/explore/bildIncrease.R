# ______________________________________________
# Bild
# Goal: Figure out drivers behind 3-fold increase in #Articles in Bild
# Procedure: load data, define vars, analyse text
# ______________________________________________
# Date:  Mon Jul 18 12:22:50 2022
# Author: Nicolai Berk
#  R version 4.1.1 (2021-08-10)
# ______________________________________________


# Setup ####
library(tidyverse)
library(dplyr)
library(here)
library(data.table)

## params
plot_permission <- F
runmodel <- F

# load data ####
data.table::fread(here("data/raw/newspapers/_bild_articles.csv"), encoding = "UTF-8") -> bild


# define vars ####
# 
# bild <-
#   bild %>% 
#   mutate(
#     period = (date > as.Date("2016-05-23") &
#                 date < as.Date("2017-06-16"))
#   )


# analyse text ####

## dpa share ####
bild <- 
  bild %>% 
  mutate(
    dpa = str_detect(text, "(dpa)")
  ) %>% 
  mutate(month = lubridate::floor_date(date, "month"))

bild %>% 
  group_by(month) %>% 
  summarise(
    n_dpa = sum(dpa),
    n_tot = n()
  ) %>% 
  mutate(dpa_share = n_dpa/n_tot) %>% 
  ggplot(aes(month, dpa_share)) +
  geom_col(fill = "#dd0000") +
  theme_minimal() +
  ggtitle("Share of DPA News in Bild coverage", "2013-2019")

bild %>% 
  ggplot(aes(x = month, fill = dpa)) +
  geom_bar() +
  theme_minimal() +
  ggtitle("Monthly number of articles, Bild online", "By DPA authorship, 2013-2019")

# DPA is driver!

## did slant change independently or is DPA just more "objective"?


## dpa across newspapers
mig_coverage <- 
  data.table::fread(here("data/processed/bert_crime_clean.csv")) %>% 
  mutate(
    dpa = str_detect(text, "(dpa)")
  ) %>% 
  mutate(month = lubridate::floor_date(date_clean, "month")) %>% 
  group_by(paper, month) %>% 
  summarise(dpa_share = mean(dpa))

mig_coverage %>% 
  ggplot(aes(month, dpa_share, col = paper)) +
  geom_line() +
  theme_minimal() +
  scale_color_manual(values = rev(MetBrewer::met.brewer("Nizami", 5)))
  

## crime share w dpa ####
bild <- data.table::fread(here("data/processed/bert_crime_clean.csv")) %>% 
  filter(paper == "Bild") %>% 
  mutate(
    dpa = str_detect(text, "(dpa)")
  ) %>% 
  mutate(month = lubridate::floor_date(date_clean, "month"))
  
bild %>% 
  group_by(month) %>% 
  summarise(
    n_dpa = sum(dpa),
    n_crime = sum(crime_label),
    n_tot = n()
  ) %>% 
  mutate(dpa_share = n_dpa/n_tot,
         crime_share = n_crime/n_tot) %>% 
  ggplot(aes(month)) +
  geom_line(aes(y = dpa_share), col = "green") +
  geom_line(aes(y = crime_share), col = "red") +
  theme_minimal() +
  ggtitle("Share of DPA and crime News on Migration in Bild coverage", "2013-2019")

crime_share_w_dpa <- 
  bild %>% 
  group_by(month) %>% 
  summarise(
    n_dpa = sum(dpa),
    n_crime = sum(crime_label),
    n_tot = n()
  ) %>% 
  mutate(dpa_share = n_dpa/n_tot,
         crime_share = n_crime/n_tot) %>% 
  ggplot(aes(month)) +
  geom_col(aes(y = dpa_share), col = "lightgray", alpha = 0.5) +
  geom_col(aes(y = crime_share), col = "red") +
  theme_minimal() +
  ylim (0, 1) +
  ggtitle("Share of Crime News on Migration in Bild coverage", "2013-2019")

crime_share_no_dpa <- 
  bild %>% 
  filter(!dpa) %>% 
  group_by(month) %>% 
  summarise(
    n_crime = sum(crime_label),
    n_tot = n()
  ) %>% 
  mutate(crime_share = n_crime/n_tot) %>% 
  ggplot(aes(month)) +
  geom_col(aes(y = crime_share), col = "red") +
  theme_minimal() +
  ylim (0, 1) +
  ggtitle("Share of Crime News on Migration in Bild coverage", "excluding DPA content, 2013-2019")


crime_share_w_dpa / crime_share_no_dpa

bild %>% 
  mutate(crime_label = crime_label== 1) %>%
  group_by(dpa) %>%
  summarise(crime_share = mean(crime_label)) %>% 
  ggplot(aes(x = dpa, y = crime_share)) +
  geom_col(position = "dodge2", fill = MetBrewer::met.brewer("Nizami", 2)) +
  theme_minimal() +
  scale_y_continuous("Count") + 
  scale_x_discrete("Content", labels = c("Original Bild", "DPA")) +
  ggtitle("Share of crime content in migration coverage by authorship")


## dpa-corrected DiD ####
mig_coverage <- 
  data.table::fread(here("data/processed/bert_crime_clean.csv")) %>% 
  mutate(
    dpa = str_detect(text, "(dpa)")
  ) %>% 
  filter(paper != "Bild" | !dpa) %>% # filter dpa from Bild coverage
  mutate(
    month = lubridate::floor_date(date_clean, "quarter"),
    post = date_clean > as.Date("2017-02-01"),
    treat = paper == "Bild"
    )

mig_coverage %>% 
  mutate(paper = paper == "Bild") %>% 
  group_by(paper, month) %>% 
  summarise(crime_share = mean(crime_label)) %>% 
  ggplot(aes(month, crime_share, col = paper)) +
  geom_line() +
  geom_vline(xintercept = as.Date("2017-02-01"), col = "red", lty = 2) +
  scale_color_manual(values = rev(MetBrewer::met.brewer("Nizami", 2)))


## DiD without DPA content (3.5pp increase)
fixest::feglm(crime_label ~ post*treat | paper + month,
     data = mig_coverage) %>% 
  summary()


## DiD controlling for DPA (3pp increase)
mig_coverage <- 
  data.table::fread(here("data/processed/bert_crime_clean.csv")) %>% 
  mutate(
    dpa = str_detect(text, "(dpa)"),
    month = lubridate::floor_date(date_clean, "quarter"),
    post = date_clean > as.Date("2017-02-01"),
    treat = paper == "Bild"
  )

fixest::feglm(crime_label ~ post*treat + dpa | paper + month,
   data = mig_coverage) %>% 
  summary()

## NA's not driver ####

if(plot_permission){
  ## problem definition: treatment confounded
  
  bild %>% 
    mutate(month = lubridate::floor_date(date, "month")) %>% 
    group_by(month) %>% 
    summarise(na_count = sum(text == "" | is.na(text)),
              n_tot = n()) %>% 
    mutate(na_share = na_count/n_tot) %>% 
    ggplot(aes(month, n_tot)) + 
    geom_col() +
    theme_minimal()
  
  bild %>% 
    mutate(month = lubridate::floor_date(date, "month")) %>% 
    group_by(month) %>% 
    summarise(na_count = sum(text == "" | is.na(text)),
              n_tot = n()) %>% 
    mutate(na_share = na_count/n_tot) %>% 
    ggplot(aes(month, na_count)) + 
    geom_col() +
    theme_minimal()
  
  ### artificial decrease in NA share due to change in n_tot
  bild %>% 
    mutate(month = lubridate::floor_date(date, "month")) %>% 
    group_by(month) %>% 
    summarise(na_count = sum(text == "" | is.na(text)),
              n_tot = n()) %>% 
    mutate(na_share = na_count/n_tot) %>% 
    ggplot(aes(month, na_share)) + 
    geom_col() +
    theme_minimal()
}

## assess driving features

### generate quanteda corpus
library("quanteda")
require(quanteda.textmodels)
require(caret)

if (!file.exists(here("data/raw/newspapers/bildCorpus.Rdata"))){
  
  bildCorpus <- corpus(bild, text_field = "text")
  bildCorpus$period <- bild$period; rm(bild)
  summary(bildCorpus)
  
  save(bildCorpus, file = here("data/raw/newspapers/bildCorpus.Rdata"))
}else{
  load(here("data/raw/newspapers/bildCorpus.Rdata"))
}

rm(bild)


if(!file.exists(here("data/raw/newspapers/dfmat_bild.Rdata"))){

  # create id, dv
  bildCorpus$id_numeric <- 1:ndoc(bildCorpus)
  
  # tokenize and pre-process Ümlaut
  toks_bild <- 
    tokens(bildCorpus, remove_punct = TRUE, remove_numbers = TRUE) %>% 
    tokens_remove(pattern = stopwords("de", source = "marimo")) %>% 
    tokens_keep(pattern = "^[\\p{script=Latn}]+$", valuetype = "regex") %>% 
    tokens_wordstem(language = "de") %>% 
    tokens_tolower()
  
  print(toks_bild[2], max_ndoc = 1, max_ntoken = -1)
  
  
  # construct document-feature matrix
  dfmat_bild <- dfm(toks_bild)
  print(dfmat_bild)
  save(dfmat_bild, file = here("data/raw/newspapers/dfmat_bild.Rdata"))
  
}else{
  load(here("data/raw/newspapers/dfmat_bild.Rdata"))
}

rm(bildCorpus)

if (!file.exists(here("data/raw/newspapers/tmod_nb.Rdata")) & runmodel){

  # train-test split
  set.seed(42)
  id_train <- sample(1:ndoc(bildCorpus), 
                     round(0.8* ndoc(bildCorpus)),
                     replace = F)
  dfmat_training <- dfm_subset(dfmat_bild, id_numeric %in% id_train)
  dfmat_test <- dfm_subset(dfmat_bild, !id_numeric %in% id_train)
  
  # train NB Classifier
  tmod_nb <- textmodel_nb(dfmat_training, dfmat_training$period)
  summary(tmod_nb)
  save(tmod_nb, file = here("data/raw/newspapers/tmod_nb.Rdata"))
  
  # sync feature space
  dfmat_matched <- dfm_match(dfmat_test, features = featnames(dfmat_training))
  
  # performance
  actual_class <- dfmat_matched$period
  predicted_class <- predict(tmod_nb, newdata = dfmat_matched)
  tab_class <- table(actual_class, predicted_class)
  tab_class
  
}else{
  load(here("data/raw/newspapers/tmod_nb.Rdata"))
}


# get features - apparently not very informative
coefs <- 
  coef(tmod_nb) %>% 
  as.data.frame()

colnames(coefs) <- c("neg", "pos")

coefs <- 
  coefs %>% 
  arrange(-pos)

coefs %>% head(10)

rm(tmod_nb)


## cosine similarity
require(quanteda.textstats)

# dfmat_period <- dfm_subset(dfmat_bild, subset = period)
# dfmat_else <- dfm_subset(dfmat_bild, subset = !period)

# dfmat_period <- dfm_tfidf(dfmat_period)
# dfmat_else <- dfm_tfidf(dfmat_else)

# subset
id_sample <- sample(docvars(dfmat_bild, "id_numeric"), 100, replace = F)

# hack from the master: https://stackoverflow.com/questions/58302449/what-does-the-cholmod-error-problem-too-large-means-exactly-problem-when-conv
dfmtrimmed <- 
  dfm_trim(
    dfm_subset(
      dfmat_bild, 
      docvars(dfmat_bild, "id_numeric") %in% id_sample), 
    min_docfreq = 100, 
    min_termfreq = 100, 
    verbose = TRUE)

rm(dfmat_bild)

y <- textstat_simil(dfmtrimmed, margin="documents", method = "cosine")
