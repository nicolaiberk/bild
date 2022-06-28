# ______________________________________________
# Media reactions to RR violence
# Goal: Estimate migration content with dictionary
# Procedure: load data, assign scores, save sample
# ______________________________________________
# Date:  Fri Apr 30 17:04:58 2021
# Author: Nicolai Berk
#  R version 4.0.3 (2020-10-10)
# ______________________________________________


# Setup ####
library(tidyverse)
library(here)
library(data.table)
library(pbapply)

# load data ####

## load dict
dict <- sort(read.csv("_dicts/german_glove.csv")$x)
papers <- c("bild", "faz", "spon", "sz", "taz", "weltonline")
n_sample <- 100

for (p in papers){
  
  cat(paste("\n\nSampling", p, "..."))
  
  ## load newspaper articles
  cat("\n\tLoading data...")
  raw <- fread(here(paste0("_dt/_out/_", p, "_articles.csv")), encoding = "UTF-8")[,c("title", "url", "text")]
  raw <- raw[text != "",]
  ncols_raw <- ncol(raw)
  
  # assign scores & sample ####
  
  ## count occurences/article length
  cat("\n\tAssigning dictionary scores...")
  for (d in dict){
    cat(paste0('\n\t\tCounting occurrences of "', d, '"...'))
    raw[,eval(d)] <- str_count(raw$text, d)
  }
  
  raw$sum <- rowSums(raw[,(ncols_raw+1):ncol(raw)])
  raw$ntokens <- str_length(raw$text)
  raw$mig_share <- (raw$sum/raw$ntokens)
  
  ## pull stratified sample
  cat("\n\tPulling stratified sample...")
  
  ### sample 100 cases from articles low, mid, and high on migration mentions
  quantiles_nozero <- quantile(raw$sum[raw$sum != 0], na.rm = T)
  low_sample <- sample_n(raw[raw$sum == 0], n_sample)
  mid_sample <- sample_n(raw[raw$sum >= quantiles_nozero[2] &
                               raw$sum <= quantiles_nozero[4]], n_sample)
  high_sample <- sample_n(raw[raw$sum > quantiles_nozero[4]], n_sample)
  
  ## check language, keep german
  low_sample <-  low_sample[cld2::detect_language(low_sample$text) == "de",]
  mid_sample <-  mid_sample[cld2::detect_language(mid_sample$text) == "de",]
  high_sample <- high_sample[cld2::detect_language(high_sample$text) == "de",]
  
  ## resample LOW_SAMPLE if necessary
  while (nrow(low_sample) < n_sample){
      resample <- sample_n(raw[raw$sum == 0], (n_sample-nrow(low_sample)))
      resample <- resample[(cld2::detect_language(resample$text) == "de") &
                             (!resample$url %in% low_sample$url)]
      low_sample <- rbind(low_sample, resample)
    }
  
  ## resample mid_sample
  while (nrow(mid_sample) < n_sample){
    resample <- sample_n(raw[raw$sum >= quantiles_nozero[2] &
                               raw$sum <= quantiles_nozero[4]], (n_sample-nrow(mid_sample)))
    resample <- resample[(cld2::detect_language(resample$text) == "de") &
                           (!resample$url %in% mid_sample$url)]
    mid_sample <- rbind(mid_sample, resample)
  }

  ## resample high_sample
  while (nrow(high_sample) < n_sample){
    resample <- sample_n(raw[raw$sum > quantiles_nozero[4]], (n_sample-nrow(high_sample)))
    resample <- resample[(cld2::detect_language(resample$text) == "de") &
                           (!resample$url %in% high_sample$url)]
    high_sample <- rbind(high_sample, resample)
  }
  
  ## combine to overall sample
  if(!"full_sample" %in% ls()){
    full_sample <- rbind(low_sample, mid_sample, high_sample)
  }else{
    full_sample <- rbind(full_sample, low_sample, mid_sample, high_sample)
  }
  
  
}

# save sample ####
fwrite(full_sample, file = here(paste0("_dt/sample_handcoding_", Sys.Date(), ".csv")))

