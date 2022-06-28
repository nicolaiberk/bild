# aggregate BERT estimates to daily measure
prevalence <- fread(here("data/processed/bert_crime_clean.csv"))


prevalence <- 
  prevalence %>% 
  select(date_clean, paper, crime_label, crime_prob) %>% 
  group_by(date_clean, paper) %>% 
  mutate(n_articles = 1) %>% 
  summarise_all(sum)

fwrite(prevalence, here("data/processed/bert_crime_daily.csv"))
