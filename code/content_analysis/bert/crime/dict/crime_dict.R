# ______________________________________________
# Bild
# Goal: crime dictionary extension
# Procedure: define seed, extend, save
# ______________________________________________
# Date:  Wed Mar 23 09:32:40 2022
# Author: Nicolai Berk
#  R version 4.1.1 (2021-08-10)
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
# define seed ####

seed_dict <- 
  c(
    "täter", "täterin", "polizei", "verbrechen", "überfall", "mord",
    "attacke", "raub", "straftat", "ermittelt", "gericht", "prozess",
    "staatsanwaltschaft", "staatsanwalt", "staatsanwältin", "gefährder", 
    "haft", "festnahme", "betrug", "betrüger", "erstochen", "opfer", 
    "anwalt", "drogen", "dealer", "getötet", "verletzung", "delikt",
    "vergewaltigung"
    )

## set up embedidng input

if (!file.exists(here('code/dict/_emb_models/glove.german.txt'))) {
  download.file('https://int-emb-glove-de-wiki.s3.eu-central-1.amazonaws.com/vectors.txt', 
                destfile = here('code/dict/_emb_models/glove.german.txt'))
}

vectors = data.table::fread(here('code/dict/_emb_models/glove.german.txt'), data.table = F,  encoding = 'UTF-8')
colnames(vectors) = c('word', paste('dim', 1:(dim(vectors)[2] - 1), sep = '_'))
embeddings <- as.matrix(vectors[2:301])
row.names(embeddings) <- vectors$word; rm(vectors)

# extend ####
temp_dict <- seed_dict[seed_dict %in% row.names(embeddings)]

## generate cosin similarity matrix for the relevant terms to all others (lunchtime!)
cosin_matrix <- matrix(nrow = nrow(embeddings), ncol = length(temp_dict))
colnames(cosin_matrix) <- temp_dict
row.names(cosin_matrix) <- row.names(embeddings)

if (!file.exists(here('code/dict/_emb_models/glove_cosine.Rda'))) {
  for (term in temp_dict){
    print(paste("Calculating cosine similarity of", term, "to all other terms..."))
    cosin_matrix[, term] <- pbapply(embeddings, MARGIN = 1, FUN = cosine, y = embeddings[term, ])
  }
  save(cosin_matrix, file = here("code/dict/_emb_models/glove_cosine.Rda"))
}


load(here('code/dict/_emb_models/glove_cosine.Rda'))

## define migration dictionary
for (term in temp_dict){
  ### find close terms for several terms related to migration
  new_dict <- row.names(cosin_matrix[cosin_matrix[, term] > 0.5, ]) # cutoff is absolutely arbitrary, inspect outcome
  extended_dict <- c(temp_dict, new_dict)
}

### keep every word once
table(extended_dict)

refined <- unique(c(extended_dict, seed_dict))

## alternative: take most similar word to each term:
for (term in temp_dict){
  ### find closest term for several terms related to migration
  new_dict <- row.names(cosin_matrix[which.max(cosin_matrix[row.names(cosin_matrix) != term, term])])
  extended_dict <- c(temp_dict, new_dict)
}

refined <- unique(c(seed_dict, extended_dict))

# save ####
write.csv(refined, here('code/dict/_dicts/crime_glove.csv'))
