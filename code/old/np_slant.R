# ______________________________________________
# causal effect of media frames
# Goal: estimate migration slant in papers
# Procedure: identify mentions, extract windows, measure slant
# ______________________________________________
# Date:  Wed May 19 12:22:30 2021
# Author: Nicolai Berk
#  R version 4.0.3 (2020-10-10)
# ______________________________________________


# Setup ####
library(tidyverse)
library(here)
library(data.table)
library(word2vec)
library(stringr)

## load dict
mig_dict <- sort(read.csv(here("data/embeddings/german_glove.csv"))$x)
pos_dict <- c()
neg_dict <- c()

papers <- c("bild", "faz", "spon", "sz", "taz", "weltonline")
window_size <- 5

for (p in papers){
  
  cat(paste("\n\nEstimating migration slant in", p, "..."))
  
  ## load newspaper articles
  cat("\n\tLoading data...")
  dta <- fread(here(paste0("data/newspapers/_", p, "_articles.csv")), encoding = "UTF-8")[,c("title", "url", "text")]
  dta <- dta[text != "",]
  
  # identify mentions ####
  
  ## get kwic windows based on mig_dict
  cat("\n\tExtract windows...")
  kwicVec <- kwic(dta$text, pattern = mig_dict, window = window_size, case_insensitive = T)
  
  # extract windows ####
  
  # measure slant ####

  }