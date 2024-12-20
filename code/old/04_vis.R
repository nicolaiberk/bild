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
library(ggpubr)

# Step 1 ####

## 2.1 vis dependents ####

## load data
gles_p_long <- fread(here('data/gles/Panel/long_cleaned.csv'))

# ## immigration attitude across time
# imm_mean <- 
#   gles_p_long %>%
#   group_by(wave) %>% 
#   filter(!is.na(`1130_clean`)) %>% 
#   summarise(immigration = mean(`1130_clean`),
#             immigration_sd = sd(`1130_clean`),
#             date_clean  = max(date_clean),
#             respondents = n()) %>% 
#   mutate(
#     imm_up = (immigration + qt(1 - (0.05/2), respondents - 1)*immigration_sd/respondents),
#     imm_low = (immigration - qt(1 - (0.05/2), respondents - 1)*immigration_sd/respondents),
#   ) %>% 
#   ggplot(aes(x = date_clean)) +
#   geom_line(aes(y = immigration), col = "red") +
#   geom_ribbon(aes(ymax = imm_up, ymin = imm_low), fill = "red") +
#   ggtitle("Immigration attitude in Germany across time", 
#           "Should immigration of foreigners be made easier (-3) or restricted (3) \nData: GLES Panel, waves 1-13, a1 & a2")
# 
# ### exclusive readers
# 
# ## integration attitude across time
# int_mean <- 
#   gles_p_long %>%
#   group_by(wave) %>% 
#   filter(!is.na(`1210_clean`)) %>% 
#   summarise(integration = mean(`1210_clean`)*(-1),
#             integration_sd = sd(`1210_clean`),
#             date_new  = max(as.Date(date_clean, unit = 'days')),
#             respondents = n()) %>% 
#   mutate(
#     imm_up = (integration + qt(1 - (0.05/2), respondents - 1)*integration_sd/respondents),
#     imm_low = (integration - qt(1 - (0.05/2), respondents - 1)*integration_sd/respondents),
#   ) %>% 
#   ggplot(aes(x = date_new)) +
#   geom_line(aes(y = integration), col = "blue") +
#   geom_ribbon(aes(ymax = imm_up, ymin = imm_low), fill = "blue") +
#   ggtitle("Integration attitude in Germany across time", 
#           "Should foreigners be allowed to live according to their own culture (-3) or fully adapt to German culture (3) \nData: GLES Panel, waves 1-13, a1 & a2")
# 
# grid.arrange(imm_mean, int_mean, nrow = 2) %>% 
#   ggsave(here('paper/vis/dv_plot.png'), plot = .)


## average per paper
plots <- list()
for (issue in c('Immigration Attitude', 'MIP: Migration', 'AfD Support')){
  if (issue == 'Immigration Attitude'){
    gles_p_long[,'dv'] <- gles_p_long[,'1130_clean']
  }else if (issue == 'Integration'){
    gles_p_long[,'dv'] <- gles_p_long[,'1210_clean']*(-1)
  }else if (issue == 'MIP: Migration'){
    gles_p_long[,'dv'] <- gles_p_long$`840_clean` %in% c(3411:3413, 3750:3759)
  }else{
    gles_p_long[,'dv'] <- gles_p_long[,'430i_clean']
  }
  
  if (issue == 'Integration'){
    scale_desc <- "Should foreigners be allowed to live according to their own culture (-3)\n or fully adapt to German culture (3) \n"
  }else if (issue == 'Immigration Attitude'){
    scale_desc <- "Should immigration of foreigners be made easier (-3) or restricted (3) \n"
  }else if (issue == 'MIP: Migration'){
    scale_desc <- "What do you think is currently the most important political Problem in Germany (% answering immigration) \n"
  }else{
    scale_desc <- "What do you think about the AfD? Don't like the party at all (-5),\n like the party a lot (5) \n"
  }
  
  temp <- 
    gles_p_long %>% 
    filter(!is.na(dv)) %>% 
    filter(wave %in% c(1, 3:8)) %>%  
    filter(readership != "None", readership != "FR", readership != "") %>% 
    group_by(wave, readership) %>% 
    summarise(dv_mean = mean(dv, na.rm = T),
              resps = n(),
              dv_sd = sd(dv, na.rm = T),
              date = max(date_clean))
  
  unique_waves <- paste(sort(unique(temp$wave)), collapse = ', ')
  
  plots[[issue]] <- 
    temp %>% 
    ggplot(
      aes(x = date, 
          y = dv_mean, 
          ymin = dv_mean +     qt((0.05/2), resps - 1)*dv_sd/resps, 
          ymax = dv_mean + qt(1 - (0.05/2), resps - 1)*dv_sd/resps, 
          col = readership, 
          fill = readership, 
          lty = readership)) +
    geom_vline(xintercept = as.Date("2017-01-31"), col = "red", lty = 2) +
    geom_line() +
    geom_ribbon(alpha = 0.5) +
    theme_minimal() +
    xlab('') + ylab('')+
    ggtitle(glue("{issue} among different newspaper-consumers across time"), 
            paste0(scale_desc, glue("Data: GLES Panel; waves {unique_waves}")))+
    labs(fill = "Paper", colour = "Paper", lty = "Paper")
}

ggarrange(plots[["Immigration Attitude"]], 
             plots[["MIP: Migration"]], 
             plots[["AfD Support"]], 
          nrow = 3, 
          common.legend = T, 
          legend = "right") %>% 
  ggsave(filename = here(glue('paper/vis/issues_most_read.png')),
         width = 12, height = 15)


# ### pivot long paper (multiple entries for respondents reading several newspapers)
# gles_p_long_paper <- 
#   gles_p_long %>% 
#   select(lfdn, wave, contains('clean')) %>% 
#   select(lfdn, wave, date_clean, contains('1661'), contains('1130'), contains('1210'), contains('430i')) %>% 
#   pivot_longer(cols = starts_with('1661'), names_prefix = '1661',
#                names_to = 'paper', values_to = 'days_read') %>% 
#   mutate(
#     paper = str_extract(paper, pattern = '.{1}')
#   )
# 
# ## adjust paper names
# gles_p_long_paper$paper[gles_p_long_paper$paper == 'a'] <- 'Bild'
# gles_p_long_paper$paper[gles_p_long_paper$paper == 'c'] <- 'FAZ'
# gles_p_long_paper$paper[gles_p_long_paper$paper == 'd'] <- 'SZ'
# gles_p_long_paper$paper[gles_p_long_paper$paper == 'e'] <- 'TAZ'
# gles_p_long_paper$paper[gles_p_long_paper$paper == 'f'] <- 'Welt'
# 
# gles_p_long_paper$reader <- gles_p_long_paper$days_read > 0
# 
# plots <- list()
# for (issue in c('Immigration', 'Integration', 'AfD-support')){
#   if (issue == 'Immigration'){
#     gles_p_long_paper['dv'] <- gles_p_long_paper['1130_clean']
#   }else if (issue == 'Integration'){
#     gles_p_long_paper['dv'] <- gles_p_long_paper['1210_clean']*(-1)
#   }else{
#     gles_p_long_paper['dv'] <- gles_p_long_paper['430i_clean']
#   }
#   
#   
#   temp <- 
#     gles_p_long_paper %>% 
#     filter(reader == T) %>% 
#     group_by(wave, paper) %>% 
#     filter(!is.na(dv)) %>% 
#     filter(!paper %in% letters) %>% 
#     summarise(dv_sd = sd(dv),
#               dv = mean(dv),
#               date_clean  = max(date_clean, na.rm = T),
#               respondents = n()) %>% 
#     mutate(
#       dv_up = (dv + qt(1 - (0.05/2), respondents - 1)*dv_sd/respondents),
#       dv_low = (dv - qt(1 - (0.05/2), respondents - 1)*dv_sd/respondents),
#     )
#   
#   unique_waves <- paste(sort(unique(temp$wave)), collapse = ', ')
#   
#   if (issue == 'Integration'){
#     scale_desc <- "Should foreigners be allowed to live according to their own culture (-3) or fully adapt to German culture (3) \n"
#   }else if (issue == 'Immigration'){
#     scale_desc <- "Should immigration of foreigners be made easier (-3) or restricted (3) \n"
#   }else{
#     scale_desc <- "What do you think about the AfD? Don't like the party at all (-5), like the party a lot (5) \n"
#   }
#   
#   plots[[issue]] <- 
#     ggplot(temp, aes(x = date_clean)) +
#     geom_line(aes(y = dv, col = paper)) +
#     geom_ribbon(aes(ymax = dv_up, ymin = dv_low, fill = paper), alpha = 0.5) +
#     xlab('') + ylab('')+
#     ggtitle(glue("{issue} attitude among newspaper-readers"), 
#             paste0(scale_desc, glue("Data: GLES Panel; waves {unique_waves}")))
#   
# 
# }
# 
# grid.arrange(plots[['Immigration']], plots[['Integration']]) %>% 
#   ggsave(filename = here(glue('paper/vis/issues_readers.png')),
#          width = 8, height = 8)
# 
# 
# ggsave(plot = plots[['Immigration']], 
#        here('pres/vis/imm_readers_pres.png'), width = 6, height = 4)
# 
# ggsave(plot = plots[['AfD-support']], 
#        here('pres/vis/afd_readers_pres.png'), width = 6, height = 4)


## 2.2 vis readership size ####

readers_survey <- 
  gles_p_long %>% 
  filter(wave == "1", most_read != "FR") %>% 
  group_by(most_read) %>% 
  summarise(count = n()) %>% 
  mutate(share = prop.table(count)) %>% 
  ggplot(aes(x = reorder(most_read, -share), y = share)) + 
  geom_bar(stat = "identity") +
  theme_minimal() +
  theme(legend.position = "None") +
  ylab("") + xlab("") +
  ggtitle("Most commonly read daily newspaper", "Data: GLES Panel; wave 1")
  
load("data/papers_dist.RDta")

auflage$Paper <- 
  auflage$Paper %>% 
  recode("Frankfurter Allgemeine" = "FAZ",
         "Süddeutsche Zeitung" = "SZ")

sales_print <- 
  auflage %>% 
  filter(Quarter == 20174, Paper != "DER SPIEGEL") %>% 
  # mutate(share = prop.table(Sales)) %>% 
  ggplot(aes(x = reorder(Paper, -Sales), y = Sales)) + 
  geom_bar(stat = "identity") +
  theme_minimal() +
  theme(legend.position = "None") +
  ylab("") + xlab("") +
  ggtitle("Print sales per newspaper", "Data: IVW 2021; First Quarter 2017")


ggarrange(readers_survey, sales_print) %>% 
  ggsave(file = "paper/vis/Paper_des_final.png", width = 8, height = 5)



## 2.3 vis independents ####

load(here('data/efftable_noimp.Rdata'))
survey_dates <- 
  gles_p_long %>% 
  filter(!is.na(date_clean)) %>% 
  filter(!is.na('1130_clean') & !is.na('1661a_clean')) %>%  
  filter(date_clean < as.Date("2018-01-01")) %>% 
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
  ggplot(aes(x = date_m, y = share_mig, col = paper, lty = paper)) +
  geom_line() +
  geom_vline(xintercept = survey_dates$date, col = 'darkgray') +
  ggtitle('Migration salience in different newspapers', 'Gray lines indicate survey waves') +
  xlab('') + ylab('Share of migration articles') +
  theme_minimal() +
  labs(color = "Paper", lty = "Paper")
ggsave(here('paper/vis/salience_papers.png'), width = 8, height = 5)

sal_2017 <- 
  salience_sum %>% 
  filter(date_m < as.Date('2018-01-01')) %>% 
  filter(date_m >= as.Date('2016-06-01')) %>% 
  ggplot(aes(x = date_m, y = share_mig, col = paper)) +
  geom_line() +
  geom_vline(xintercept = survey_dates$date, col = 'darkgray') +
  ggtitle('Migration salience in different newspapers', 'Gray lines indicate survey waves') +
  xlab('') + ylab('Share of migration articles')
ggsave(sal_2017, filename = here('paper/vis/salience_papers_focus.png'), 
       width = 8, height = 5)


## frames over time
load(here("data/media_daily_2021-11-19.Rdata"))

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
framing_sum$post <- framing_sum$date > as.Date("2017-02-01")

framing_sum %>% 
  filter(date < as.Date('2019-01-01')) %>%
  filter(frame %in% c("Camps", "Capital Crime", "Mediterranean", "Petty Crime")) %>% 
  group_by(paper, frame) %>% 
  ggplot(aes(x = date, y = attention, col = frame)) +
  geom_line() +
  geom_vline(xintercept = as.Date("2017-02-01"), col = "red", lty = 2) +
  facet_grid(paper~frame, scales = 'free') +
  ggtitle('Migration framing in different newspapers') +
  ylab('Share of all migration articles') +
  xlab('') + theme(legend.position = "none")
ggsave(here('paper/vis/frames_papers.png'), width = 10, height = 6)

## presi version
framing_sum %>% 
  filter(date < as.Date('2019-01-01')) %>%
  filter(paper %in% c("Bild")) %>% 
  filter(frame %in% c("Camps", "Capital Crime", "Labour Market", "Petty Crime")) %>% 
  group_by(paper, frame) %>% 
  ggplot(aes(x = date, y = attention, col = frame)) +
  geom_point(alpha = 0.3) +
  geom_smooth(aes(lty = post)) +
  geom_vline(xintercept = as.Date("2017-02-01"), col = "red", lty = 2) +
  facet_wrap(~paper, scales = 'free', nrow = 2) +
  ggtitle('Migration framing in different newspapers') +
  ylab('Share of all migration articles') +
  xlab('') +
  labs(col = "Frame")
ggsave(here('pres/vis/frames_papers_pres.png'), width = 8, height = 5)

framing_sum %>% 
  filter(date < as.Date('2018-01-01')) %>%
  filter(date > as.Date('2016-06-01')) %>% 
  filter(paper %in% c("Bild")) %>% 
  filter(frame %in% c("Camps", "Capital Crime", "Labour Market", "Petty Crime")) %>% 
  group_by(paper, frame) %>% 
  ggplot(aes(x = date, y = attention, col = frame)) +
  geom_point(alpha = 0.3) +
  geom_smooth(aes(lty = post)) +
  facet_wrap(~paper, scales = 'free', nrow = 2) +
  ggtitle('Migration framing in different newspapers') +
  ylab('Share of all migration articles') +
  xlab('') +
  labs(col = "Frame")
ggsave(here('pres/vis/frames_papers_focus_pres.png'), width = 8, height = 5)
ggsave(here('paper/vis/frames_papers_focus.png'), width = 10, height = 6)


# effect of Diekmann leaving
framing_sum %>%
  filter(date < as.Date('2020-01-01')) %>%
  filter(paper %in% c("Bild", "Welt", "FAZ")) %>% 
  filter(frame %in% c("Camps", "Capital Crime", "Labour Market", "Petty Crime")) %>%
  group_by(paper, frame) %>% 
  ggplot(aes(x = date, y = attention, col = paper)) +
  geom_point(alpha = 0.3) +
  geom_smooth(aes(col = paper, lty = post)) +
  geom_vline(xintercept = as.Date('2017-01-31'), lty = 2, col = 'red') +
  facet_wrap(~frame, scales = "free_y") +
  xlab('')
ggsave(here('pres/vis/frames_bild_diekmann_pres.png'), width = 8, height = 5)
ggsave(here('paper/vis/frames_bild_diekmann.png'), width = 10, height = 6)


framing_sum %>%
  filter(date < as.Date('2018-01-01')) %>%
  filter(date >= as.Date('2016-01-01')) %>%
  filter(paper %in% c("Bild", "Welt", "FAZ")) %>% 
  filter(frame %in% c("Camps", "Capital Crime", "Labour Market", "Petty Crime")) %>%
  group_by(paper, frame) %>% 
  ggplot(aes(x = date, y = attention, col = paper)) +
  geom_point(alpha = 0.3) +
  geom_smooth(aes(col = paper, lty = post)) +
  geom_vline(xintercept = as.Date('2017-01-31'), lty = 2, col = 'red') +
  facet_wrap(~frame, scales = "free_y") +
  xlab('')
ggsave(here('pres/vis/frames_bild_diekmann_focus_pres.png'), width = 8, height = 5)
ggsave(here('paper/vis/frames_bild_diekmann_focus.png'), width = 10, height = 6)



framing_sum %>% 
  filter(date < as.Date('2018-01-01')) %>% 
  filter(date >= as.Date('2016-06-01')) %>% 
  group_by(paper, frame) %>% 
  ggplot(aes(x = date, y = attention, col = frame)) +
  geom_line() +
  geom_rug(data = survey_dates, aes(x = date),sides = 't', inherit.aes = F) +
  facet_grid(paper~frame, scales = 'free') +
  ggtitle('Migration frames in different newspapers 2017', 'Black ticks indicate survey waves') +
  scale_x_date(date_labels = '%b') +
  ylab('Share of all migration articles') +
  xlab('') + theme(legend.position = "none")
ggsave(here('pres/vis/frames_papers_focus_pres.png'), width = 8, height = 5)
ggsave(here('paper/vis/frames_papers_focus.png'), width = 10, height = 6)


## 2.4 vis effects simple did Reichelt ####

did_set <- read.csv("data/simple_did.csv")

did_set$paper <- factor(did_set$paper, levels = rev(c("Bild", "FR", "FAZ", "SZ", "taz", "Welt", "None")))

did_set$period[did_set$period == "all"] <- "All Waves"
did_set$period[did_set$period == "First two"] <- "Pre and post wave only"

did_set$dv[did_set$dv == "imm"] <- "Immigration"
did_set$dv[did_set$dv == "int"] <- "Integration"
did_set$dv[did_set$dv == "afd"] <- "AfD-support"

did_set %>% 
  filter(paper != "FR") %>% 
  ggplot(aes(x = est, xmin = lower, xmax = upper, y = paper, col = !(lower > 0 |(upper < 0))))+
  geom_vline(xintercept = 0, lty = 2, col = "red") +
  geom_pointrange() +
  theme_minimal() + xlab("") + ylab("") +
  facet_grid(period~dv) +
  theme(legend.position = "none")
ggsave(filename = "paper/vis/simple_did.png", width = 10, height = 6)

## 2.5 vis p-values attitude did ####
attitude_dids <- read.csv(here('data/coeftable_gles_noimp.csv'))


attitude_dids$sig_fe <- (attitude_dids$p_fe < 0.05)
attitude_dids$sig_lm <- (attitude_dids$p_lm < 0.05)

prop.table(table(attitude_dids$sig_fe)) %>% round(2) # 12% instead of 5% -> significant effects overrepresented


attitude_dids %>% 
  ggplot(aes(x = p_fe, y = ..count../nrow(attitude_dids))) +
  geom_histogram(bins = 20, col = 'gray', breaks = seq(from = 0, to = 0.95, 0.05)) +
  geom_hline(aes(yintercept = 0.05), col = 'red', lty = 2) +
  ylim(0, 0.25) +
  ylab('Density')+xlab('p')+
  ggtitle(glue("P-values from {nrow(attitude_dids)} fixed-effect DiD-models"), 
          "Treatment = wave, condition = media outlet consumption")
ggsave(filename = here("paper/vis/DiD_model_ps_notheory.png"),
       width = 8, height = 3)

# theoryplot <- data.frame(p = runif(1336, 0, 1)) %>% 
#   ggplot(aes(x = p, y = ..count../1336)) +
#   geom_histogram(bins = 20, col = 'gray', breaks = seq(from = 0, to = 0.95, 0.05)) +
#   geom_hline(aes(yintercept = 0.05), col = 'red', lty = 2) +
#   ylim(0, 0.25) +
#   ylab('Density')+xlab('p')+
#   ggtitle(glue("Theoretical distribution of p-values with no effect of media consumption"))
# 
# gridExtra::grid.arrange(fe_plot, theoryplot, nrow = 2) %>% 
#   ggsave(plot = ., 
#          filename = here("paper/vis/DiD_model_ps.png"),
#          width = 8, height = 6)


## migration and integration only
mig_dids <- 
  attitude_dids %>% 
  filter(issue == '1130' | issue == '1210')

prop.table(table(mig_dids$sig_fe)) %>% round(2) # 18% instead of 5% -> significant effects overrepresented


mig_dids %>% 
  ggplot(aes(x = p_fe, y = ..count../nrow(mig_dids))) +
  geom_histogram(bins = 20, col = 'gray', breaks = seq(from = 0, to = 0.95, 0.05)) +
  geom_hline(aes(yintercept = 0.05), col = 'red', lty = 2) +
  ylim(0, 0.25) +
  ylab('Density')+xlab('p')+
  ggtitle(glue("P-values from {nrow(mig_dids)} fixed-effect DiD-models of migration attitudes"), 
          "Treatment = wave, condition = media outlet consumption")
ggsave(filename = here("paper/vis/DiD_model_ps_immint_notheory.png"),
       width = 8, height = 3)


## look at attitude dids across treatment specifications
mig_dids$respondents[mig_dids$respondents == 'only readers'] <- 'all readers'

mig_dids %>% 
  ggplot(aes(x = p_fe, y = ..count../(nrow(mig_dids)/2))) +
  geom_histogram(bins = 20, col = 'gray', breaks = seq(from = 0, to = 0.95, 0.05)) +
  geom_hline(aes(yintercept = 0.05), col = 'red', lty = 2) +
  ylab('Density')+xlab('p')+
  facet_grid(~respondents)+
  ggtitle(glue("P-values from {nrow(mig_dids)} fixed-effect DiD-models of migration attitudes"), 
          "Treatment = wave, condition = media outlet consumption")
ggsave(filename = here("paper/vis/DiD_model_ps_immint_bytreat.png"),
       width = 8, height = 3)



## afd-support
afd_dids <- 
  attitude_dids %>% 
  filter(issue == '430i')

prop.table(table(afd_dids$sig_fe)) %>% round(2) # 8% instead of 5% -> significant effects overrepresented


afd_dids %>% 
  ggplot(aes(x = p_fe, y = ..count../nrow(afd_dids))) +
  geom_histogram(bins = 20, col = 'gray', breaks = seq(from = 0, to = 0.95, 0.05)) +
  geom_hline(aes(yintercept = 0.05), col = 'red', lty = 2) +
  ylim(0, 0.25) +
  ylab('Density')+xlab('p')+
  ggtitle(glue("P-values from {nrow(afd_dids)} fixed-effect DiD-models of migration attitudes"), 
          "Treatment = wave, condition = media outlet consumption")
ggsave(filename = here("paper/vis/DiD_model_ps_afd_notheory.png"),
       width = 8, height = 3)


## look at attitude dids across treatment specifications
afd_dids$respondents[afd_dids$respondents == 'only readers'] <- 'all readers'

afd_dids %>% 
  ggplot(aes(x = p_fe, y = ..count../(nrow(afd_dids)/2))) +
  geom_histogram(bins = 20, col = 'gray', breaks = seq(from = 0, to = 0.95, 0.05)) +
  geom_hline(aes(yintercept = 0.05), col = 'red', lty = 2) +
  ylab('Density')+xlab('p')+
  facet_grid(~respondents)+
  ggtitle(glue("P-values from {nrow(afd_dids)} fixed-effect DiD-models of migration attitudes"), 
          "Treatment = wave, condition = media outlet consumption")
ggsave(filename = here("paper/vis/DiD_model_ps_afd_bytreat.png"),
       width = 8, height = 3)




# 3. vis effects across specifications ####
load(here('data/efftable_did_2021-10-25.Rdata'))
efftable$dv_lag_f <- factor(efftable$dv_lag, levels = c('all', '6m', '1m', '1w', '1d'))

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
ggarrange(plotlist = plots, ncol = 1, nrow = 7,
          common.legend = T, legend = "bottom") %>% 
  ggsave(filename = here("paper/vis/effectplot_frames_did.png"),
         width = 9, height = 12)




## individual fe model
load(here('data/efftable_fe_2021-12-03.Rdata'))
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

ggarrange(plotlist = plots, ncol = 2, nrow = 4,
          common.legend = T, legend = "bottom") %>% 
  ggsave(filename = here("paper/vis/effectplot_frames_fe_abs.png"),
         width = 10, height = 10)


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

ggarrange(plotlist = plots,  nrow = 4, ncol = 2,
          common.legend = T, legend = "bottom") %>% 
  ggsave(filename = here("paper/vis/effectplot_frames_fe_rel.png"),
         width = 7, height = 10)





## for presi: visualise effect distribution

### did model
load(here('data/efftable_did_2021-10-25.Rdata'))
efftable %>% 
  select(!contains("lower")) %>% 
  select(!contains("upper")) %>% 
  pivot_longer(cols = contains("beta"), names_to = "frame", names_prefix = "beta_") %>%
  mutate(frame = recode(frame,
                        'camps_share' = 'Camps', 'crime_share' = 'Petty Crime',
                        'capcrime_share' = 'Capital Crime', 'deport_share' = 'Deportations',
                        'labmar_share' = 'Labour Market', 'medit_share' = 'Mediterranean',
                        'refnums_share' = 'Refugee Numbers'
  )) %>% 
  filter(frame != "salience") %>% 
  ggplot(aes(x = value, y = frame, col = frame)) +
  geom_violin() +
  geom_vline(xintercept = 0, col = "red", lty = 2) +
  xlab("Standardised Effect") + ylab("Frame") +
  ggtitle("Framing effects in DiD-models across specifications") +
  theme(legend.position = "None") +
  facet_wrap(~dv, scales = "free")
ggsave(here("pres/vis/frame_effects_did.png"), width = 8, height = 5)

### fe model
load(here('data/efftable_fe_2021-10-25.Rdata'))
efftable %>% 
  select(!contains("lower")) %>% 
  select(!contains("upper")) %>% 
  pivot_longer(cols = contains("beta"), names_to = "frame", names_prefix = "beta_") %>%
  mutate(frame = recode(frame,
                        'camps_share' = 'Camps', 'crime_share' = 'Petty Crime',
                        'capcrime_share' = 'Capital Crime', 'deport_share' = 'Deportations',
                        'labmar_share' = 'Labour Market', 'medit_share' = 'Mediterranean',
                        'refnums_share' = 'Refugee Numbers'
  )) %>% 
  filter(frame != "salience") %>% 
  ggplot(aes(x = value, y = frame, col = frame)) +
  geom_violin() +
  geom_vline(xintercept = 0, col = "red", lty = 2) +
  xlab("Standardised Effect") + ylab("Frame") +
  ggtitle("Framing effects in 2-way FE-models across specifications") +
  theme(legend.position = "None") +
  facet_wrap(~dv, scales = "free")
ggsave(here("pres/vis/frame_effects_fe.png"), width = 8, height = 5)
