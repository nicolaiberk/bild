# ______________________________________________
# Effect of framing on issue attitudes
# Goal: Define relevant frames, export estimates
# ______________________________________________
# Date:  Thu Oct 04 17:06:22 2021
# Author: Nicolai Berk
#  R version 4.0.3 (2020-10-10)
# ______________________________________________


# Setup ####
library(tidyverse)
library(data.table)
library(here)
library(stm)
library(lubridate)
library(tidytext)
library(ggthemes)
library(scales)
library(knitr)
library(dplyr)



# explore following Julia Silge's blogpost: https://juliasilge.com/blog/evaluating-stm/
load(here('data/out_docs.Rdata'))
load(here('data/topic_model_K60.Rdata'))


## look into selected topic prevalence across time & newspapers
prevalence <- data.frame(row.names = 1:length(out$documents))

### topic 1: english
prevalence$english <- topic_model$theta[,1]

### topic 2: stock market NY/FRA
prevalence$finance_bonds <- topic_model$theta[,2]

### topic 3: international distribution of refugees
prevalence$international_dist <- topic_model$theta[,3]

### topic 4: Stock markets eastern europe
prevalence$stocks_cee <- topic_model$theta[,4]

### topic 5: foreign government reaction to migration pact
prevalence$un_mig_pact <- topic_model$theta[,5]

### topic 6: unionsstreit asyl
prevalence$asylstreit <- topic_model$theta[,6]

### topic 7: discussion of right-wing intellectuals
prevalence$rw_intellectuals <- topic_model$theta[,7]

### topic 8: asian stock markets
prevalence$stocks_asia <- topic_model$theta[,8]

### topic 9: german länder politics
prevalence$laender <- topic_model$theta[,9]

### topic 10: legal aspects integration (doppelpass, einwanderungsgesetz)
prevalence$legal_integration <- topic_model$theta[,10]

### topic 11: job market integration of refugees
prevalence$jobmarket_integration <- topic_model$theta[,11]

### topic 12: european borders
prevalence$eu_borders <- topic_model$theta[,12]

### topic 13: war in syria
prevalence$war_syria <- topic_model$theta[,13]

### topic 14: interviews morals surrounding immigration
prevalence$iv_imm_moral <- topic_model$theta[,14]

### topic 15: schweizer zuwanderungsinititative
prevalence$swiss_initiative <- topic_model$theta[,15]

### topic 16: spiegel epilogue
prevalence$spiegel <- topic_model$theta[,16]

### topic 17: local distribution of refugees
prevalence$local_dist <- topic_model$theta[,17]

### topic 18: immigration through Mexico into the US
prevalence$mex_mig <- topic_model$theta[,18]

### topic 19: EU refugee distribution/asylum policy
prevalence$eu_asylum <- topic_model$theta[,19]

### topic 20: private mediterranean search & rescue (Alan Kurdi vs. Salvini and so on)
prevalence$sr_medit <- topic_model$theta[,20]

### topic 21: Bild footer
prevalence$bild_footer <- topic_model$theta[,21]

### topic 22: episodic migration framing
prevalence$episodic_mig <- topic_model$theta[,22]

### topic 23: islamic state
prevalence$islamic_state <- topic_model$theta[,23]

### topic 24: german economic development/GDP
prevalence$economy <- topic_model$theta[,24]

### topic 25: Crime & Schlepper
prevalence$crime_schlepp <- topic_model$theta[,25]

### topic 26: right-wing anti-refugee protests
prevalence$rr_protests <- topic_model$theta[,26]

### topic 27: Greek refugee camps (Moria/Kos/idomeni)
prevalence$camps_greece <- topic_model$theta[,27]

### topic 28: refugee families (episodic framing)
prevalence$families_episodic <- topic_model$theta[,28]

### topic 29: dax
prevalence$stocks_dax <- topic_model$theta[,29]

### topic 30: essener tafel scandal
prevalence$essener_tafel <- topic_model$theta[,]

### topic 31: carneval and refugees
prevalence$carneval <- topic_model$theta[,31]

### topic 32: refugee numbers arriving to germany
prevalence$refugee_nums <- topic_model$theta[,32]

### topic 33: cost of refugees/national finances
prevalence$refugee_cost <- topic_model$theta[,33]

### topic 34: refugee camps middle east
prevalence$ME_camps <- topic_model$theta[,34]

### topic 35: great coalition
prevalence$great_coalition <- topic_model$theta[,35]

### topic 36: asylum law
prevalence$asylum_law <- topic_model$theta[,36]

### topic 37: refugees in berlin
prevalence$refugees_berlin <- topic_model$theta[,37]

### topic 38: the boat is full
prevalence$boat_full <- topic_model$theta[,38]

### topic 39: asylum law reform
prevalence$asylum_reform <- topic_model$theta[,39]

### topic 40: NY stock exchange
prevalence$ny_stocks <- topic_model$theta[,40]

### topic 41: history of migration
prevalence$history <- topic_model$theta[,41]

### topic 42: trump
prevalence$trump <- topic_model$theta[,42]

### topic 43: german politicians on european asylum politics
prevalence$politicians_eu <- topic_model$theta[,43]

### topic 44: deportations
prevalence$deportations <- topic_model$theta[,44]

### topic 45: public opinion 
prevalence$polls <- topic_model$theta[,45]

### topic 46: refugee children
prevalence$children <- topic_model$theta[,46]

### topic 47: balcan route
prevalence$balcan <- topic_model$theta[,47]

### topic 48: sexual assault through refugees
prevalence$sexual_assault <- topic_model$theta[,48]

### topic 49: humanitarian tragedy in the mediterranean
prevalence$mediterranean <- topic_model$theta[,49]

### topic 50: population statistics germany
prevalence$pop_stats <- topic_model$theta[,50]

### topic 51: france-uk migration and calais camp
prevalence$calais <- topic_model$theta[,51]

### topic 52: BAMF scandal
prevalence$BAMF <- topic_model$theta[,52]

### topic 53: CSU and poverty migration
prevalence$csu_poverty <- topic_model$theta[,53]

### topic 54: party politics
prevalence$party <- topic_model$theta[,54]

### topic 55: Australian detention camps
prevalence$australia <- topic_model$theta[,55]

### topic 56: cultural integration
prevalence$cultural_integration <- topic_model$theta[,56]

### topic 57: Berlin social state office
prevalence$lageso <- topic_model$theta[,57]

### topic 58: Hamburg refugee housing
prevalence$hamburg <- topic_model$theta[,58]

### topic 59: churches and migration
prevalence$church <- topic_model$theta[,59]

### topic 60: hungarian refugee referendum
prevalence$hungary_referendum <- topic_model$theta[,60]


# daily estimates
prevalence_daily <- 
  prevalence  %>% 
  cbind(paper = out$meta$paper) %>%
  cbind(date_new = out$meta$date_new) %>% 
  group_by(paper, date_new) %>% 
  summarise(across(.fns = sum),
            n_articles = n())

save(prevalence_daily, file = here(paste0('data/daily_mig_topics', Sys.Date(), '.Rdata')))


output <- cbind(out$meta, prevalence)
save(output, file = here(paste0('data/mig_articles_topics', Sys.Date(), '.Rdata')))
