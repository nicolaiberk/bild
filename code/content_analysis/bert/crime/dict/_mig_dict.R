# ______________________________________________
# Media reactions to RR violence
# Goal: Generate migration dictionary
# Procedure: function, set params, generate & save
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

# function ####

DictionaryExpander <- function(paper, union_size, seed_dict, top_n) {
  ## load model
  model <- read.word2vec(here(paste0("_dt/_emb_models/", paper, ".bin")))
  
  ## create matrix for embeddings
  embedding <- as.matrix(model)
  
  temp_dict <- seed_dict[seed_dict %in% row.names(embedding)]
  

  
  ## define migration dictionary 
  
  ### find close terms for several terms related to migration
  extended <- predict(model, temp_dict, 
                           type = "nearest", top_n = top_n)
  
  ### get most commonly associated terms
  dump <- c()
  for (i in extended){
    dump <- c(i$term2, dump)
  }
  
  ### keep only those associated with at least [union_size] terms
  #     (it might be better to do this with a similarity cutoff, 
  #     e.g. include everything with similarity >0.9).
  refined <- table(dump)[table(dump) >= union_size] %>% names() 
  refined <- unique(c(refined, seed_dict))
  
  ## write to .csv
  write.csv(refined, here(paste0('_sc/_dict/_dicts/', paper, '.csv')))
  
  
  ## alternative: take most similar word to each term:
  
  ### find closest term for several terms related to migration
  extended <- predict(model, temp_dict, 
                      type = "nearest", top_n = top_n)
  dump <- c()
  for (i in extended){
    dump <- c(i$term2[1], dump)
  }
  
  refined <- unique(c(seed_dict, dump))

  ### write to .csv
  write.csv(refined, here(paste0('_sc/_dict/_dicts/', paper, '_alt.csv')))
  
  
}


# params ####
seed_dict <- c("asyl", "asylant", "asylanten", "asylantinnen", 
               "asylbewerber", "asylbewerberin", "asylbewerberinnen", 
               "auslander","auslanderinnen", 
               "einwanderer", "einwandererin", "einwandererinnen", "einwandernde", "einwanderung",  
               "fluchtling", "fluchtende", "fluchtlinge",
               "migration", "immigration", 
               "schutzsuchende", "schutzbedurftige",
               "visa", "visum", 
               "zuwanderung", "zuwanderer",  "zuwandernde"
              )
umlaut_dict <- c("ausländer", "ausländerinnen", 
                 "flüchtling", "flüchtende", "flüchtlinge", 
                 "schutzbedürftige")

paper <- c('bild', 'faz', 'spon', 'sz', 'taz', 'weltonline')

top_n <- 100 # number of most similar terms to be considered for inclusion
union_size <- 4 # number of terms to at least be associated with to be included in the dictionary


# generate dicts ####
for (p in paper){
  print(p)
  DictionaryExpander(paper = p, union_size = union_size, seed_dict = seed_dict, top_n = top_n)
}



# generate one overall dict, based on german wiki model ####

## see https://deepset.ai/german-word-embeddings (GloVe)

## load pre-trained embeddings, if not already (get a coffee, this runs a bit)
if (!file.exists(here('_dt/_emb_models/glove.german.txt'))) {
  download.file('https://int-emb-glove-de-wiki.s3.eu-central-1.amazonaws.com/vectors.txt', 
                destfile = here('_dt/_emb_models/glove.german.txt'))
}

vectors = data.table::fread(here('_dt/_emb_models/glove.german.txt'), data.table = F,  encoding = 'UTF-8')
colnames(vectors) = c('word', paste('dim', 1:(dim(vectors)[2] - 1), sep = '_'))
embeddings <- as.matrix(vectors[2:301])
row.names(embeddings) <- vectors$word; rm(vectors)

## extend dict
seed_dict <- c(seed_dict, umlaut_dict)
temp_dict <- seed_dict[seed_dict %in% row.names(embeddings)]

## generate cosin similarity matrix for the relevant terms to all others (lunchtime!)
cosin_matrix <- matrix(nrow = nrow(embeddings), ncol = length(temp_dict))
colnames(cosin_matrix) <- temp_dict
row.names(cosin_matrix) <- row.names(embeddings)

if (!file.exists(here('_dt/_emb_models/glove_cosine.Rda'))) {
  for (term in temp_dict){
    print(paste("Calculating cosine similarity of", term, "to all other terms..."))
    cosin_matrix[, term] <- pbapply(embeddings, MARGIN = 1, FUN = cosine, y = embeddings[term, ])
  }
  save(cosin_matrix, file = here("_dt/_emb_models/glove_cosine.Rda"))
}

load(here('_dt/_emb_models/glove_cosine.Rda'))


## define migration dictionary
for (term in temp_dict){
  ### find close terms for several terms related to migration
  new_dict <- row.names(cosin_matrix[cosin_matrix[, term] > 0.6, ]) # cutoff is absolutely arbitrary, inspect outcome
  extended_dict <- c(temp_dict, new_dict)
}

### keep every word once
refined <- unique(c(extended_dict, seed_dict))

## write to .csv
write.csv(refined, here(paste0('_sc/_dict/_dicts/german_glove.csv')))


## alternative: take most similar word to each term:
for (term in temp_dict){
  ### find closest term for several terms related to migration
  new_dict <- row.names(cosin_matrix[which.max(cosin_matrix[row.names(cosin_matrix) != term, term])])
  extended_dict <- c(temp_dict, new_dict)
}

refined <- unique(c(seed_dict, extended_dict))

### write to .csv
write.csv(refined, here(paste0('_sc/_dict/_dicts/german_glove_alt.csv')))
