# ______________________________________________
# Media reactions to RR violence
# Goal: Embeddings for each paper with text2vec
# Procedure: function, set params, generate & save
# ______________________________________________
# Date:  Fri Apr 23 18:13:56 2021
# Author: Nicolai Berk
#  R version 4.0.3 (2020-10-10)
# ______________________________________________

# ______________________________________________
# This is not necessary for dictionary expansion, 
#  but could be useful later on to estimate the
#  shift in framing before and after an attack
#  (Rodriguez et al. 2020).
# 
# To do this, the model needed to run on the texts,
#  and we might want to compare the submodels to a 
#  model trained on the merged set of articles.
# 
# It might be very useful to use this to generate
#  migration dictionaries for each party.
# 
# Note that when running with full texts, docs
#  need to be split into sentences.
# 
# ______________________________________________




# Setup ####
library(dplyr)
library(tidyr)
library(quanteda)
library(stringr)
library(ggplot2)
library(gridExtra)
library(word2vec)
library(here)

# function ####

EmbeddingGenerator <- function(paper, textcol, is_csv) {
  
  ## load data
  if(is_csv){
    
    path <- here(paste0('_dt/_', paper, '_raw.csv'))
    dta <- read.csv(path)
    rm(list = setdiff(ls(), c('dta', 'paper', 'textcol')))
    
  }else{
    
    path <- here(paste0('_dt/_', paper, '_raw.Rdata'))
    dta <- get(load(path))
    rm(list = setdiff(ls(), c('dta', 'paper', 'textcol'))) # this is necessary bc load() loads to original namespace
    
  }
  
  ## clean
  cleaner <- function(x){
    y <- gsub(x, pattern = "\\n|\\t|\\r|  ", replacement = "")
    return(y)
  }
  
  dta <- lapply(dta[textcol], cleaner) %>% 
    as.data.frame()
  
  ## prepare tokens
  toks <- dta$title %>% 
    corpus() %>% 
    tokens(remove_punct = T, remove_numbers = T) %>% # remove punctuation and numbers
    tokens_tolower()
  
  rm(dta)
  
  dta_clean <- toks %>% 
    lapply(toString) %>% 
    gsub(pattern = ",", replacement = "")
  names(dta_clean) <-  names(toks)
  
  ## estimate word2vec
  set.seed(42)
  model <- word2vec(x = dta_clean, type = "cbow", dim =200, iter = 20)
  
  ## save model
  write.word2vec(model, here(paste0("_dt/_emb_models/", paper, ".bin")))
}


# set params ####
paper <- c('bild', 'faz', 'spon', 'sz', 'taz', 'weltonline')
textcol <- rep('title', 6)
is_csv <- c(F, T, F, T, T, F)

input <- cbind(paper, textcol, is_csv) %>% as.data.frame()

# generate & save (THIS RUNS AGES) ####
for (row in 1:6){ # change to 1:6 if full re-run is necessary
  EmbeddingGenerator(
    paper = input[row,'paper'],
    textcol = input[row,'textcol'],
    is_csv = input[row,'is_csv']
  )
  
}


# # test
# model <- read.word2vec(here(paste0("_dt/_emb_models/weltonline.bin")))
# embedding <- as.matrix(model)
# predict(model, "migration", type = "nearest", top_n = 10)
                       