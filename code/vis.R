# ______________________________________________
# Project Title
# Goal: Visualisation
# Procedure: Step 1, Step 2, Step 3
# ______________________________________________
# Date:  Thu Sep 09 17:46:22 2021
# Author: Nicolai Berk
#  R version 4.0.3 (2020-10-10)
# ______________________________________________


# Setup ####
library(tidyverse)
library(dplyr)
library(here)
library(data.table)
library(ggExtra)
library(glue)
library(haven)
library(gridExtra)
library(lubridate)

# Step 1 ####

## 2.1 vis dependents ####

## load data
gles_p_long <- fread('data/gles/Panel/long_cleaned.csv')

## immigration attitude across time
imm_mean <- 
  gles_p_long %>%
  group_by(wave) %>% 
  filter(!is.na(`1130_clean`)) %>% 
  summarise(immigration = mean(`1130_clean`),
            immigration_sd = sd(`1130_clean`),
            date_clean  = max(date_clean),
            respondents = n()) %>% 
  mutate(
    imm_up = (immigration + qt(1 - (0.05/2), respondents - 1)*immigration_sd/respondents),
    imm_low = (immigration - qt(1 - (0.05/2), respondents - 1)*immigration_sd/respondents),
  ) %>% 
  ggplot(aes(x = date_clean)) +
  geom_line(aes(y = immigration), col = "red") +
  geom_ribbon(aes(ymax = imm_up, ymin = imm_low), fill = "red") +
  ggtitle("Immigration attitude in Germany across time", "Scale: -3 \nData: GLES Panel, waves 1-13, a1 & a2")

## integration attitude across time
int_mean <- 
  gles_p_long %>%
  group_by(wave) %>% 
  filter(!is.na(`1210_clean`)) %>% 
  summarise(integration = mean(`1210_clean`),
            integration_sd = sd(`1210_clean`),
            date_new  = max(as.Date(date_clean, unit = 'days')),
            respondents = n()) %>% 
  mutate(
    imm_up = (integration + qt(1 - (0.05/2), respondents - 1)*integration_sd/respondents),
    imm_low = (integration - qt(1 - (0.05/2), respondents - 1)*integration_sd/respondents),
  ) %>% 
  ggplot(aes(x = date_new)) +
  geom_line(aes(y = integration), col = "blue") +
  geom_ribbon(aes(ymax = imm_up, ymin = imm_low), fill = "blue") +
  ggtitle("Integration attitude in Germany across time", "Data: GLES Panel, waves 1-13")

grid.arrange(imm_mean, int_mean, nrow = 2) %>% 
  ggsave(here('paper/vis/dv_plot.png'), plot = .)


### average per paper

#### pivot long paper
gles_p_long_paper <- 
  gles_p_long %>% 
  select(lfdn, wave, contains('clean')) %>% 
  select(lfdn, wave, date_clean, contains('1661'), contains('1130'), contains('1210')) %>% 
  pivot_longer(cols = starts_with('1661'), names_prefix = '1661',
               names_to = 'paper', values_to = 'days_read') %>% 
  mutate(
    paper = str_extract(paper, pattern = '.{1}')
  )

## adjust paper names
gles_p_long_paper$paper[gles_p_long_paper$paper == 'a'] <- 'bild'
gles_p_long_paper$paper[gles_p_long_paper$paper == 'c'] <- 'faz'
gles_p_long_paper$paper[gles_p_long_paper$paper == 'd'] <- 'sz'
gles_p_long_paper$paper[gles_p_long_paper$paper == 'e'] <- 'taz'
gles_p_long_paper$paper[gles_p_long_paper$paper == 'f'] <- 'welt'

gles_p_long_paper$reader <- gles_p_long_paper$days_read > 0


for (issue in c('Immigration', 'Integration')){
  if (issue == 'Immigration'){
    gles_p_long_paper['dv'] <- gles_p_long_paper['1130_clean']
  }else{
    gles_p_long_paper['dv'] <- gles_p_long_paper['1210_clean']
  }
  
  
  gles_p_long_paper %>% 
    filter(reader == T) %>% 
    group_by(wave, paper) %>% 
    filter(!is.na(dv)) %>% 
    filter(!paper %in% letters) %>% 
    summarise(dv_sd = sd(dv),
              dv = mean(dv),
              date_clean  = max(date_clean, na.rm = T),
              respondents = n()) %>% 
    mutate(
      dv_up = (dv + qt(1 - (0.05/2), respondents - 1)*dv_sd/respondents),
      dv_low = (dv - qt(1 - (0.05/2), respondents - 1)*dv_sd/respondents),
    ) %>% 
    ggplot(aes(x = date_clean)) +
    geom_line(aes(y = dv, col = paper)) +
    geom_ribbon(aes(ymax = dv_up, ymin = dv_low, fill = paper), alpha = 0.5) +
    ggtitle(glue("{issue} attitude among newspaper-readers"), "Data: GLES Panel, waves 1-13, a1 & a2")
  
  ggsave(filename = here(glue('paper/vis/{issue}_papers.png')),
         width = 8, height = 6)

}

## 2.2 vis independents ####

survey_dates <- 
  gles_p_long %>% 
  filter(!is.na(date_clean)) %>% 
  group_by(wave) %>% 
  summarise(date = max(date_clean)) %>% 
  ungroup() %>% 
  select(date)
  

## salience over time
load(here('data/salience_cleaned.csv'))

salience_sum <- 
  salience_raw %>% 
  mutate(date_m = floor_date(date_new, 'month')) %>% 
  group_by(date_m, paper) %>% 
  summarise(
    n_mig = sum(mig),
    n_all = n()
  )

salience_sum <- 
  salience_sum %>% 
  mutate(
    share_mig = n_mig/n_all
  )

salience_sum %>% 
  filter(date_m < as.Date('2020-01-01')) %>% 
  ggplot(aes(x = date_m, y = share_mig, col = paper)) +
  geom_line(size = 1.2) +
  geom_vline(xintercept = survey_dates$date, col = 'darkgray') +
  facet_wrap(~paper, nrow = 6) +
  ggtitle('Migration salience in different newspapers', 'Grey lines indicate survey waves')
ggsave(here('paper/vis/salience_papers.png'))

salience_sum %>% 
  filter(date_m < as.Date('2020-01-01')) %>%
  filter(date_m > as.Date('2016-06-01')) %>% 
  ggplot(aes(x = date_m, y = share_mig, col = paper)) +
  geom_line(size = 1.2) +
  geom_vline(xintercept = survey_dates$date, col = 'black') +
  facet_wrap(~paper, nrow = 6) +
  ggtitle('Migration salience in different newspapers', 'Black lines indicate survey waves')
ggsave(here('paper/vis/salience_papers_focus.png'))



## frames over time
load(here('data/frames_cleaned.csv'))

framing_sum <- 
  framing_raw %>% 
  select(-date, -text, -title, -url) %>% 
  mutate(
    date = floor_date(as.Date(date_new), 'month')
  ) %>% 
  select(-date_new) %>% 
  group_by(paper, date) %>% 
  summarise_all(sum) %>% 
  pivot_longer(cols = !c(date, paper), names_to = 'frame', values_to = 'attention')

framing_sum %>% 
  filter(date < as.Date('2020-01-01')) %>%
  group_by(paper, frame) %>% 
  ggplot(aes(x = date, y = attention, col = frame)) +
  geom_line() +
  geom_vline(xintercept = survey_dates$date, col = 'darkgray') +
  facet_grid(frame~paper, scales = 'free') +
  ggtitle('Migration salience in different newspapers', 'Grey lines indicate survey waves')
ggsave(here('paper/vis/frames_papers_unwtd.png'))

framing_sum %>% 
  filter(date < as.Date('2020-01-01')) %>%
  filter(date > as.Date('2016-06-01')) %>% 
  group_by(paper, frame) %>% 
  ggplot(aes(x = date, y = attention, col = frame)) +
  geom_line() +
  geom_vline(xintercept = survey_dates$date, col = 'darkgray') +
  facet_grid(frame~paper, scales = 'free') +
  ggtitle('Migration salience in different newspapers', 'Grey lines indicate survey waves')
ggsave(here('paper/vis/frames_papers_unwtd_focus.png'))


### weighted by abs n of articles (makes it comparable across newspapers)
overall_sum <- 
  merge(salience_sum, framing_sum, by.x = c('paper', 'date_m'), by.y = c('paper', 'date')) %>% 
  filter(paper != 'spon')

overall_sum %>% 
  mutate(
    attention_share = attention/n_all
  ) %>%  
  ggplot(aes(x = date_m, y = attention_share, col = frame)) +
  geom_line(size = 0.8) +
  geom_vline(xintercept = survey_dates$date, col = 'darkgray') +
  facet_grid(frame~paper, scales = 'free')
ggsave(here('paper/vis/frames_papers_wtd.png'))

overall_sum %>% 
  mutate(
    attention_share = attention/n_all
  ) %>%  
  filter(date_m > as.Date('2016-06-01')) %>% 
  ggplot(aes(x = date_m, y = attention_share, col = frame)) +
  geom_line(size = 0.8) +
  geom_vline(xintercept = survey_dates$date, col = 'darkgray') +
  facet_grid(frame~paper, scales = 'free')
ggsave(here('paper/vis/frames_papers_wtd_focus.png'))




## 2.3 vis p-values attitude did ####
attitude_dids <- read.csv(here('data/coeftable_noimp.csv'))


attitude_dids$sig_fe <- (attitude_dids$p_value_fe < 0.05)
attitude_dids$sig_lm <- (attitude_dids$p_value_lm < 0.05)

prop.table(table(attitude_dids$sig_fe)) %>% round(2) # 21% instead of 5% -> significant effects overrepresented
prop.table(table(attitude_dids$sig_lm)) %>% round(2) # 12% instead of 5% -> significant effects overrepresented


fe_plot <- attitude_dids %>% 
  ggplot(aes(x = p_value_fe, y = ..count../nrow(attitude_dids))) +
  geom_histogram(bins = 20, col = 'gray', breaks = seq(from = 0, to = 0.95, 0.05)) +
  geom_hline(aes(yintercept = 0.05), col = 'red', lty = 2) +
  ylim(0, 0.25) +
  xlab('Density')+ylab('p')+
  ggtitle(glue("P-values from {nrow(attitude_dids)} fixed-effect DiD-models"), 
          "Treatment = wave, condition = media outlet consumption")

theoryplot <- data.frame(p = runif(1336, 0, 1)) %>% 
  ggplot(aes(x = p, y = ..count../1336)) +
  geom_histogram(bins = 20, col = 'gray', breaks = seq(from = 0, to = 0.95, 0.05)) +
  geom_hline(aes(yintercept = 0.05), col = 'red', lty = 2) +
  ylim(0, 0.25) +
  xlab('Density')+ylab('p')+
  ggtitle(glue("Theoretical distribution of p-values with no effect of media consumption"))

gridExtra::grid.arrange(fe_plot, theoryplot, nrow = 2) %>% 
  ggsave(plot = ., 
         filename = here("paper/vis/DiD_model_ps.png"),
         width = 10, height = 8)

## framing effects
hist(framing_dids$p)

## salience DIDs
framing_dids %>% 
  filter(frame == 'salience') %>% 
  select(p) %>% 
  unlist() %>% 
  hist() 


## vis effects across specifications ####
load(here('data/efftable_noimp.Rdata'))

efftable$frame[efftable$frame == "camps"] <- "camps (+)"
efftable$frame[efftable$frame == "crime"] <- "crime (-)"
efftable$frame[efftable$frame == "capcrime"] <- "capcrime (-)"
efftable$frame[efftable$frame == "salience"] <- "salience (-)"
efftable$frame[efftable$frame == "medit"] <- "medit (+)"
efftable$frame[efftable$frame == "labmar"] <- "labmar (+)"
efftable$frame[efftable$frame == "refnums"] <- "refnums (-)"
efftable$frame[efftable$frame == "labmar"] <- "labmar (+)"
efftable$frame[efftable$frame == "medit"] <- "medit (+)"
efftable$frame[efftable$frame == "deport"] <- "deport (+)"

efftable$dv_lag <- factor(as.factor(efftable$dv_lag), levels = c("g", "6m", "1m", "1w"))

efftable %>% 
  ggplot(mapping = aes(x = t_value, y = dv_lag, col = iv)) +
  geom_point() +
  geom_vline(aes(xintercept = 0), col = "red", lty = 2) +
  facet_grid(frame ~ dv)
ggsave(filename = here("paper/vis/effectplot_noimp.png"),
       width = 6, height = 8)

efftable %>% 
  ggplot(mapping = aes(x = t_value, fill = dv, col = dv)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~frame)
ggsave(filename = here("paper/vis/t_values_noimp.png"),
       width = 6, height = 8)


