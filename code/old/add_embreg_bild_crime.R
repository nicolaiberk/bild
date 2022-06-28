# ______________________________________________
# Media effects Bild
# Goal: Assess crime association of migration in Bild
# Procedure: load data, run regression, vis
# ______________________________________________
# Date:  Tue Feb 22 18:39:54 2022
# Author: Nicolai Berk
#  R version 4.1.1 (2021-08-10)
# ______________________________________________


# Setup ####
library(tidyverse)
library(dplyr)
library(here)
library(data.table)
library(text2vec)
library(quanteda)
library(conText)
library(glue)

# load data ####
papers <- 
  list.files(here("data/newspapers")) %>% 
  stringr::str_match("_(.*)_.*") %>% 
  as_tibble() %>% 
  select(V2) %>% 
  filter(!grepl(pattern = "2019", V2) & !grepl(pattern = "article", V2)) %>% 
  unlist()

# embeddings
load(here("data/local_glove.Rdata"))

# transformation matrix
load(here("data/trns_mtrx.Rdata"))


# define terms
mig_term <- "*migrant*"
crime_terms <- c("täter", "verbrechen", "kriminell", "illegal", "festgenommen")

# define empty tibble
immig_cs <- tibble()


for (paper in papers){
  
  
  if (!file.exists(here(glue("data/toks_{paper}.Rdata")))){
    
    print(glue("Tokenizing {paper} articles..."))
  
    # articles
    articles <- 
      fread(here(glue("data/newspapers/_{paper}_articles.csv"))) %>%  # add other NPs to run DiD at later stage
      mutate(ext = tolower(text)) %>% 
      select(date, text) %>% 
      mutate(paper = paper) %>% 
      filter(!is.na(date))
      
    # create covariate
    articles <- 
      articles %>% 
      mutate(
        date = as.Date(date, 
                       origin = "1970-01-01", 
                       format = ifelse(paper == "spon", "%d.%m.%Y", "%Y-%m-%d"))
          )
    
    
    articles$year <- lubridate::year(articles$date)
    articles$month <- lubridate::month(articles$date)
    articles$ym <- paste(articles$year, articles$month, sep = "-")
  
  
  
    # transform ####
    
    # create corpus
    cr_sample_corpus <- corpus(articles, text_field = "text"); rm(articles); rm(articles)
    
    # tokenize corpus removing unnecessary (i.e. semantically uninformative) elements
    toks <- tokens(cr_sample_corpus, remove_punct = T, remove_symbols = T, remove_numbers = T, 
                   remove_separators = T); rm(cr_sample_corpus)
    
    # clean out stopwords and words with 2 or fewer characters
    toks_nostop <- tokens_select(toks, pattern = stopwords("en"), selection = "remove", 
                                 min_nchar = 3); rm(toks)
    
    # only use features that appear at least 5 times in the corpus
    feats <- dfm(toks_nostop, tolower = F, verbose = TRUE) %>% dfm_trim(min_termfreq = 5) %>% 
      featnames()
    
    # leave the pads so that non-adjacent words will not become adjacent
    toks <- tokens_select(toks_nostop, feats, padding = TRUE)
    
    save(toks, file = here(glue("data/toks_{paper}.Rdata"))); rm(toks)
  }
  
  # run analysis ####
  load(here(glue("data/toks_{paper}.Rdata")))
  
  
  # build a tokenized corpus of contexts sorrounding the target term
  immig_toks <- tokens_context(x = toks, pattern = mig_term, window = 6L)
  
  # build document-feature matrix
  immig_dfm <- dfm(immig_toks)
  
  # build a document-embedding-matrix
  immig_dem <- dem(x = immig_dfm, pre_trained = local_glove, transform = TRUE, 
                   transform_matrix = local_transform, verbose = TRUE)
  
  immig_wv_ym <- dem_group(immig_dem, groups = immig_dem@docvars$year)
  dim(immig_wv_ym)
  
  
  # find nearest neighbors by party setting as_list = FALSE combines each group's
  # results into a single tibble (useful for joint plotting)
  immig_nns <- nns(immig_wv_ym, pre_trained = local_glove, N = 5, candidates = immig_wv_ym@features, 
                   as_list = TRUE)
  
  # check out results for different periods
  print(immig_nns[["2015"]])
  print(immig_nns[["2016"]])
  print(immig_nns[["2017"]])
  
  
  
  immig_cs <- 
    cos_sim(immig_wv_ym, pre_trained = local_glove, features = c(crime_terms), as_list = FALSE) %>% 
    as_tibble() %>% 
    mutate(time = as.Date(paste(target, "01-01", sep = "-"))) %>% 
    mutate(paper = paper) %>% 
    rbind(immig_cs)

}

save(immig_cs, file =  here("data/cos_sim_crime_mig.csv"))

# vis ####
immig_cs %>%
  filter(time <= as.Date("2018-01-01")) %>%
  filter(paper == "bild") %>% 
  mutate(time = lubridate::year(time)) %>% 
  # group_by(time, paper) %>% 
  # summarise(value = mean(value)) %>% 
  ggplot(aes(x = time, y = value, col = feature)) +
  geom_line() +
  # geom_vline(xintercept = as.Date("2016-07-01"), col = "red", lty = 2) +
  ggtitle(glue::glue("Association of the term '{mig_term}' with crime terms in news coverage"),
          cat(c("Crime terms:"), crime_terms)) +
  theme_minimal() 
  # facet_wrap(~paper)

ggsave(here("paper/vis/all_nps_ass_crime.png"))
