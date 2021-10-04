# ______________________________________________
# Effect of framing on issue attitudes
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
gles_p_long <- fread(here('data/gles/Panel/long_cleaned.csv'))

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
  ggtitle("Immigration attitude in Germany across time", 
          "Should immigration of foreigners be made easier (-3) or restricted (3) \nData: GLES Panel, waves 1-13, a1 & a2")

## integration attitude across time
int_mean <- 
  gles_p_long %>%
  group_by(wave) %>% 
  filter(!is.na(`1210_clean`)) %>% 
  summarise(integration = mean(`1210_clean`)*(-1),
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
  ggtitle("Integration attitude in Germany across time", 
          "Should foreigners be allowed to live according to their own culture (-3) or fully adapt to German culture (3) \nData: GLES Panel, waves 1-13, a1 & a2")

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
gles_p_long_paper$paper[gles_p_long_paper$paper == 'a'] <- 'Bild'
gles_p_long_paper$paper[gles_p_long_paper$paper == 'c'] <- 'FAZ'
gles_p_long_paper$paper[gles_p_long_paper$paper == 'd'] <- 'SZ'
gles_p_long_paper$paper[gles_p_long_paper$paper == 'e'] <- 'TAZ'
gles_p_long_paper$paper[gles_p_long_paper$paper == 'f'] <- 'Welt'

gles_p_long_paper$reader <- gles_p_long_paper$days_read > 0

plots <- list()
for (issue in c('Immigration', 'Integration')){
  if (issue == 'Immigration'){
    gles_p_long_paper['dv'] <- gles_p_long_paper['1130_clean']
  }else{
    gles_p_long_paper['dv'] <- gles_p_long_paper['1210_clean']*(-1)
  }
  
  
  temp <- 
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
    )
  
  unique_waves <- paste(sort(unique(temp$wave)), collapse = ', ')
  
  if (issue == 'Integration'){
    scale_desc <- "Should foreigners be allowed to live according to their own culture (-3) or fully adapt to German culture (3) \n"
  }else{
    scale_desc <- "Should immigration of foreigners be made easier (-3) or restricted (3) \n"
  }
  
  plots[[issue]] <- 
    ggplot(temp, aes(x = date_clean)) +
    geom_line(aes(y = dv, col = paper)) +
    geom_ribbon(aes(ymax = dv_up, ymin = dv_low, fill = paper), alpha = 0.5) +
    xlab('') + ylab('')+
    ggtitle(glue("{issue} attitude among newspaper-readers"), 
            paste0(scale_desc, glue("Data: GLES Panel; waves {unique_waves}")))
  

}

grid.arrange(plots[['Immigration']], plots[['Integration']]) %>% 
  ggsave(filename = here(glue('paper/vis/issues_readers.png')),
         width = 8, height = 8)



## 2.2 vis independents ####

load(here('data/efftable_noimp.Rdata'))
survey_dates <- 
  gles_p_long %>% 
  filter(!is.na(date_clean)) %>% 
  filter(!is.na('1130_clean') | !is.na('1210_clean')) %>% 
  filter(date_clean >= as.Date('2017-01-01')) %>% 
  filter(date_clean < as.Date('2018-01-01')) %>% 
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
  geom_line() +
  geom_vline(xintercept = survey_dates$date, col = 'darkgray') +
  ggtitle('Migration salience in different newspapers', 'Gray lines indicate survey waves') +
  xlab('') + ylab('Share of migration articles')
ggsave(here('paper/vis/salience_papers.png'), width = 8, height = 5)

sal_2017 <- 
  salience_sum %>% 
  filter(date_m < as.Date('2018-01-01')) %>% 
  filter(date_m >= as.Date('2017-01-01')) %>% 
  ggplot(aes(x = date_m, y = share_mig, col = paper)) +
  geom_line() +
  geom_vline(xintercept = survey_dates$date, col = 'darkgray') +
  ggtitle('Migration salience in different newspapers', 'Gray lines indicate survey waves') +
  xlab('') + ylab('Share of migration articles')
ggsave(sal_2017, filename = here('paper/vis/salience_papers_focus.png'), 
       width = 8, height = 5)


## frames over time
load(here("data/media_daily_2021-10-03.Rdata"))

framing_sum <- 
  merged_media %>% 
  mutate(
    date = floor_date(as.Date(date_new), 'month')
  ) %>% 
  select(-date_new) %>% 
  group_by(paper, date) %>% 
  summarise_all(mean, na.rm = T) %>% 
  pivot_longer(cols = !c(date, paper, crime, n_tot, 
                         n_mig_fra, n_mig_sal, share_mig,
                         medit, deport, refnums, camps, 
                         labmar, capcrime), 
               names_to = 'frame', values_to = 'attention') %>% 
  mutate(paper = recode(paper, 
                        'bild' = 'Bild', 'faz' = 'FAZ', 'spon' = 'Spiegel',
                        'sz' = 'SZ', 'taz' = 'TAZ', 'welt' = 'Welt'
                        )) %>% 
  mutate(frame = recode(frame,
                        'camps_share' = 'Camps', 'crime_share' = 'Petty Crime',
                        'capcrime_share' = 'Capital Crime', 'deport_share' = 'Deportations',
                        'labmar_share' = 'Labour Market', 'medit_share' = 'Mediterranean',
                        'refnums_share' = 'Refugee Numbers'
                        ))

framing_sum %>% 
  filter(date < as.Date('2020-01-01')) %>%
  group_by(paper, frame) %>% 
  ggplot(aes(x = date, y = attention, col = frame)) +
  geom_line() +
  facet_grid(paper~frame, scales = 'free') +
  ggtitle('Migration framing in different newspapers') +
  ylab('Share of all migration articles') +
  xlab('') + theme(legend.position = "none")
ggsave(here('paper/vis/frames_papers.png'), width = 10, height = 6)

framing_sum %>% 
  filter(date < as.Date('2018-01-01')) %>% 
  filter(date >= as.Date('2017-01-01')) %>% 
  group_by(paper, frame) %>% 
  ggplot(aes(x = date, y = attention, col = frame)) +
  geom_line() +
  geom_rug(data = survey_dates, aes(x = date),sides = 't', inherit.aes = F) +
  facet_grid(paper~frame, scales = 'free') +
  ggtitle('Migration frames in different newspapers 2017', 'Black ticks indicate survey waves') +
  scale_x_date(date_labels = '%b') +
  ylab('Share of all migration articles') +
  xlab('') + theme(legend.position = "none")
ggsave(here('paper/vis/frames_papers_focus.png'), width = 10, height = 6)


## 2.3 vis p-values attitude did ####
attitude_dids <- read.csv(here('data/coeftable_gles_noimp.csv'))


attitude_dids$sig_fe <- (attitude_dids$p_fe < 0.05)
attitude_dids$sig_lm <- (attitude_dids$p_lm < 0.05)

prop.table(table(attitude_dids$sig_fe)) %>% round(2) # 12% instead of 5% -> significant effects overrepresented


fe_plot <- attitude_dids %>% 
  ggplot(aes(x = p_fe, y = ..count../nrow(attitude_dids))) +
  geom_histogram(bins = 20, col = 'gray', breaks = seq(from = 0, to = 0.95, 0.05)) +
  geom_hline(aes(yintercept = 0.05), col = 'red', lty = 2) +
  ylim(0, 0.25) +
  ylab('Density')+xlab('p')+
  ggtitle(glue("P-values from {nrow(attitude_dids)} fixed-effect DiD-models"), 
          "Treatment = wave, condition = media outlet consumption")

theoryplot <- data.frame(p = runif(1336, 0, 1)) %>% 
  ggplot(aes(x = p, y = ..count../1336)) +
  geom_histogram(bins = 20, col = 'gray', breaks = seq(from = 0, to = 0.95, 0.05)) +
  geom_hline(aes(yintercept = 0.05), col = 'red', lty = 2) +
  ylim(0, 0.25) +
  ylab('Density')+xlab('p')+
  ggtitle(glue("Theoretical distribution of p-values with no effect of media consumption"))

gridExtra::grid.arrange(fe_plot, theoryplot, nrow = 2) %>% 
  ggsave(plot = ., 
         filename = here("paper/vis/DiD_model_ps.png"),
         width = 8, height = 6)


## migration and integration only
attitude_dids <- 
  attitude_dids %>% 
  filter(issue == '1130' | issue == '1210')

prop.table(table(attitude_dids$sig_fe)) %>% round(2) # 18% instead of 5% -> significant effects overrepresented


fe_plot <- attitude_dids %>% 
  ggplot(aes(x = p_fe, y = ..count../nrow(attitude_dids))) +
  geom_histogram(bins = 20, col = 'gray', breaks = seq(from = 0, to = 0.95, 0.05)) +
  geom_hline(aes(yintercept = 0.05), col = 'red', lty = 2) +
  ylim(0, 0.25) +
  ylab('Density')+xlab('p')+
  ggtitle(glue("P-values from {nrow(attitude_dids)} fixed-effect DiD-models"), 
          "Treatment = wave, condition = media outlet consumption")

theoryplot <- data.frame(p = runif(1336, 0, 1)) %>% 
  ggplot(aes(x = p, y = ..count../1336)) +
  geom_histogram(bins = 20, col = 'gray', breaks = seq(from = 0, to = 0.95, 0.05)) +
  geom_hline(aes(yintercept = 0.05), col = 'red', lty = 2) +
  ylim(0, 0.25) +
  ylab('Density')+xlab('p')+
  ggtitle(glue("Theoretical distribution of p-values with no effect of media consumption"))

gridExtra::grid.arrange(fe_plot, theoryplot, nrow = 2) %>% 
  ggsave(plot = ., 
         filename = here("paper/vis/DiD_model_ps_immint.png"),
         width = 8, height = 6)


## vis effects across specifications ####
load(here('data/efftable_did_2021-10-04.Rdata'))

efftable$dv_lag_f <- factor(efftable$dv_lag, levels = c('6m', '1m', '1w', '1d'))

plots <- list()
for (frame in c('crime', 'capcrime', 'refnums', 'medit', 'camps', 'labmar', 'deport')){
  
  plots[[frame]] <- 
    efftable %>% 
    ggplot(mapping = 
             aes_string(
              x = paste0('beta_', frame), 
              y = 'dv_lag_f', 
              xmin = paste0('lower_beta_', frame),
              xmax = paste0('upper_beta_', frame),
              col = 'respondents'
            )) +
    geom_vline(aes(xintercept = 0), col = "red", lty = 2) +
    geom_pointrange(size = 0.3, position = ggstance::position_dodgev(height = 0.75)) +
    facet_grid( ~ dv) +
    xlab("Beta") +
    ylab("") +
    ggtitle(frame) +
    theme_minimal() +
    coord_cartesian(xlim = c(-2, 2))
}

library(ggpubr)
ggarrange(plotlist = plots, ncol = 2, nrow = 4, 
          common.legend = T, legend = "bottom") %>% 
  ggsave(filename = here("paper/vis/effectplot_frames_did.png"),
         width = 10, height = 7)




## individual fe model
load(here('data/efftable_fe_2021-10-04.Rdata'))

efftable$dv_lag_f <- factor(efftable$dv_lag, levels = c('30d', '7d'))


## absolute frame attention model
plots <- list()
for (frame in c('crime', 'capcrime', 'refnums', 'medit', 'camps', 'labmar', 'deport')){
  
  plots[[frame]] <- 
    efftable %>% 
    filter(specification == 'absolute') %>%
    ggplot(mapping = 
             aes_string(
               x = paste0('beta_', frame), 
               y = 'dv_lag_f', 
               xmin = paste0('lower_beta_', frame),
               xmax = paste0('upper_beta_', frame),
               col = 'respondents'
             )) +
    geom_vline(aes(xintercept = 0), col = "red", lty = 2) +
    geom_pointrange(size = 0.3, position = ggstance::position_dodgev(height = 0.75)) +
    facet_grid( ~ dv, scales = 'free') +
    xlab("Beta") +
    ylab("") +
    ggtitle(frame) +
    theme_minimal()
}

ggarrange(plotlist = plots, 
          common.legend = T, legend = "bottom") %>% 
  ggsave(filename = here("paper/vis/effectplot_frames_fe_abs.png"),
         width = 10, height = 7)


## relative frame attention model
plots <- list()
for (frame in c('salience', 'crime', 'capcrime', 'refnums', 'medit', 'camps', 'labmar', 'deport')){
  
  plots[[frame]] <- 
    efftable %>% 
    filter(specification == 'share') %>% 
    ggplot(mapping = 
             aes_string(
               x = paste0('beta_', frame), 
               y = 'dv_lag_f', 
               xmin = paste0('lower_beta_', frame),
               xmax = paste0('upper_beta_', frame),
               col = 'respondents'
             )) +
    geom_vline(aes(xintercept = 0), col = "red", lty = 2) +
    geom_pointrange(size = 0.3, position = ggstance::position_dodgev(height = 0.75)) +
    facet_grid( ~ dv, scales = 'free') +
    xlab("Beta") +
    ylab("") +
    ggtitle(frame) +
    theme_minimal()
}

ggarrange(plotlist = plots, 
          common.legend = T, legend = "bottom") %>% 
  ggsave(filename = here("paper/vis/effectplot_frames_fe_rel.png"),
         width = 10, height = 7)
