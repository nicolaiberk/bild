# ______________________________________________
# causal effect of media frames
# Goal: extend migration dictionary
# Procedure: set params, generate & save
# ______________________________________________
# Date:  Fri Apr 23 18:13:56 2021
# Author: Nicolai Berk
#  R version 4.0.3 (2020-10-10)
# ______________________________________________




# Setup ####
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(gridExtra)
library(word2vec)
library(here)
library(lsa)
library(pbapply)

# generate one overall dict, based on german wiki model ####


# define dicts
seed_dict <- c("asyl", "asylant", "asylanten", "asylantinnen", 
               "asylbewerber", "asylbewerberin", "asylbewerberinnen", 
               "auslander","auslanderinnen", 
               "einwanderer", "einwandererin", "einwandererinnen", "einwandernde", "einwanderung",  
               "fluchtling", "fluchtende", "fluchtlinge",
               "migration", "immigration", 
               "schutzsuchende", "schutzbedurftige",
               "visa", "visum", 
               "zuwanderung", "zuwanderer",  "zuwandernde",
               "ausländer", "ausländerinnen", 
                 "flüchtling", "flüchtende", "flüchtlinge", 
                 "schutzbedürftige")

## see https://deepset.ai/german-word-embeddings (GloVe)

## load pre-trained embeddings, if not already (get a coffee, this runs a bit)
if (!file.exists('_emb_models/glove.german.txt')) {
  download.file('https://int-emb-glove-de-wiki.s3.eu-central-1.amazonaws.com/vectors.txt', 
                destfile = '_emb_models/glove.german.txt')
}

vectors = data.table::fread('_emb_models/glove.german.txt', data.table = F,  encoding = 'UTF-8')
colnames(vectors) = c('word', paste('dim', 1:(dim(vectors)[2] - 1), sep = '_'))
embeddings <- as.matrix(vectors[2:301])
row.names(embeddings) <- vectors$word; rm(vectors)

## extend dict
temp_dict <- seed_dict[seed_dict %in% row.names(embeddings)]

## generate cosin similarity matrix for the relevant terms to all others (lunchtime!)
cosin_matrix <- matrix(nrow = nrow(embeddings), ncol = length(temp_dict))
colnames(cosin_matrix) <- temp_dict
row.names(cosin_matrix) <- row.names(embeddings)

if (!file.exists('_emb_models/glove_cosine.Rda')) {
  for (term in temp_dict){
    print(paste("Calculating cosine similarity of", term, "to all other terms..."))
    cosin_matrix[, term] <- pbapply(embeddings, MARGIN = 1, FUN = cosine, y = embeddings[term, ])
  }
  save(cosin_matrix, file = "_emb_models/glove_cosine.Rda")
}

load('_emb_models/glove_cosine.Rda')


## define migration dictionary
for (term in temp_dict){
  ### find close terms for several terms related to migration
  new_dict <- row.names(cosin_matrix[cosin_matrix[, term] > 0.6, ]) # cutoff is absolutely arbitrary, inspect outcome
  extended_dict <- c(temp_dict, new_dict)
}

### keep every word once
refined <- unique(c(extended_dict, seed_dict))

## write to .csv
write.csv(refined, here(paste0('data/embeddings/german_glove.csv')))


## alternative: take most similar word to each term:
for (term in temp_dict){
  ### find closest term for several terms related to migration
  new_dict <- row.names(cosin_matrix[which.max(cosin_matrix[row.names(cosin_matrix) != term, term])])
  extended_dict <- c(temp_dict, new_dict)
}

refined <- unique(c(seed_dict, extended_dict))

### write to .csv
write.csv(refined, here(paste0('_sc/_dict/_dicts/german_glove_alt.csv')))
