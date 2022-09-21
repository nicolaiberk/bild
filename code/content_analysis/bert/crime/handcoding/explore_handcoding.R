# ______________________________________________
# Explore handcodings RA's
# ______________________________________________
# Date:  Tue Apr 19 16:32:17 2022
# Author: Nicolai Berk
#  R version 4.1.1 (2021-08-10)
# ______________________________________________


# Setup ####
library(tidyverse)
library(dplyr)
library(here)
library(data.table)

# load

# crime_coding_nico <- 
#   fread(here("data/handcoding/crime/handcoding_nico.csv"))

crime_coding_nat <- 
  fread(here("data/handcoding/crime/nathalie/handcoding_nathalie.csv"), encoding = "UTF-8")

crime_coding_rob <- 
  fread(here("data/handcoding/crime/robin/handcoding_robin.csv"), encoding = "UTF-8")

## descriptives
table(crime_coding_nat$crime) # 25.5% Crime
table(crime_coding_rob$crime) # 25.4% Crime

## correlation of codings
common_nat <- 
  crime_coding_nat %>% 
  filter(url %in% crime_coding_rob$url) %>% 
  arrange(url)

common_rob <- 
  crime_coding_rob %>% 
  filter(url %in% crime_coding_nat$url) %>% 
  arrange(url)

# correlation table - not so great
table(common_nat$crime, common_rob$crime)

# cor: 0.79
cor(common_nat$crime == "Ja", common_rob$crime == "Ja")


# merge

coding_merge <- 
  crime_coding_nat %>% 
  full_join(crime_coding_rob, by = c("url", "title", "text")) %>% 
  mutate(crime.nat = crime.x, crime.rob = crime.y) %>% 
  select(-crime.x, -crime.y)

# assess differences

## Nathalie yes, Robin no - mostly not crime but foreign news
coding_merge %>% 
  filter(crime.nat == "Ja", crime.rob == "Nein") %>% 
  select(title)


## Nathalie no, Robin yes - mostly crime
coding_merge %>% 
  filter(crime.nat == "Nein", crime.rob == "Ja") %>% 
  select(title)

# generate merge file with robin overriding Nathalie
training_data <- 
  crime_coding_nat %>%
  filter(!url %in% crime_coding_rob$url) %>% 
  rbind(crime_coding_rob)

fwrite(training_data, file = "data/handcoding/training_crime.csv")

## random check nathalie codings - looks mostly good
crime_coding_nat %>% 
  filter(crime == "Ja") %>% 
  sample_n(10) %>% 
  select(title)


crime_coding_nat %>% 
  filter(crime == "Nein") %>% 
  sample_n(10) %>% 
  select(title)


## random check robin codings - looks good
crime_coding_rob %>% 
  filter(crime == "Ja") %>% 
  sample_n(10) %>% 
  select(title)


crime_coding_nat %>% 
  filter(crime == "Nein") %>% 
  sample_n(10) %>% 
  select(title)
