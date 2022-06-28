# ______________________________________________
# media effects bild
# Goal: estimate FE models for all frames for different ideological sbgroups
# Procedure: load data, run models, vis 
# ______________________________________________
# Date:  Mon Feb 14 15:10:08 2022
# Author: Nicolai Berk
#  R version 4.1.1 (2021-08-10)
# ______________________________________________


# Setup ####
library(tidyverse)
library(dplyr)
library(here)
library(data.table)

# load data ####
merged_data <- fread(here('data/merged_2022-03-01.csv'))

# define treatment
merged_data <- 
  merged_data %>% 
  mutate(crime_share = crime_schlepp_share + sexual_assault_share,
         imm_att = factor(imm_att, levels = c("conservative", "moderate", "liberal", "")))


# run models ####
library(fixest)

## 7-day lag
merged_data_7d <- 
  merged_data %>% 
  filter(lag == "7")

# naïve model
merged_data_7d <- 
  merged_data_7d %>% 
  mutate(dv = `1130_clean`) %>% 
  arrange(lfdn, date_clean.x) %>% 
  group_by(lfdn) %>% 
  mutate(dv_lag = lag(`1130_clean`))

summary(lm(dv ~ crime_schlepp_share, data = merged_data_7d)) # increasingly conservatiive
# summary(lm(dv ~ crime_schlepp_share + dv_lag, data = merged_data_7d)) # negative effect
feglm(dv ~ crime_schlepp_share + dv_lag | wave, data = merged_data_7d) # positive
feglm(dv ~ crime_schlepp_share + dv_lag | wave, data = merged_data_7d, cluster = c("lfdn", "wave")) # positive, sig
# feglm(dv ~ crime_schlepp_share + dv_lag | wave, data = merged_data_7d, cluster = c("wave", "lfdn")) # positive

# remove missing conditioning variables
merged_data_7d <- 
  merged_data_7d %>% 
  filter(imm_att != "")

# interaction model
feglm(dv ~ crime_schlepp_share*imm_att | wave, data = merged_data_7d, cluster = c("lfdn", "wave")) # becomes stronger for libs
feglm(dv ~ crime_schlepp_share*imm_att + dv_lag | wave, data = merged_data_7d, cluster = c("lfdn", "wave")) # stronger for libs, not sig

# interaction with issue salience
feglm(dv ~ crime_schlepp_share*`1140_clean` | wave, data = merged_data_7d, cluster = c("lfdn", "wave")) # no interaction effect of salience
feglm(dv ~ crime_schlepp_share*`1140_clean` + dv_lag | wave, data = merged_data_7d, cluster = c("lfdn", "wave")) # no interaction effect of salience
feglm(dv ~ crime_schlepp_share*as.factor(`1140_clean`) + dv_lag | wave, data = merged_data_7d, cluster = c("lfdn", "wave")) # no interaction effect of salience






feglm(dv ~ n_articles*imm_att + dv_lag | wave, data = merged_data_7d, cluster = c("lfdn", "wave")) # generally no effect, tiny liberalising effects for libs
# but this should be estimated with the share of migration articles ut of all articles


# all crime
feglm(dv ~ crime_share | lfdn + wave, data = merged_data_7d) # null
feglm(dv ~ crime_share*imm_att | wave, data = merged_data_7d, cluster = c("lfdn", "wave")) # only convinces libs and moderates
feglm(dv ~ crime_share*imm_att, data = merged_data_7d, cluster = c("lfdn", "wave")) # only convinces libs and moderates

## include lagged dv
feglm(dv ~ crime_share*imm_att + dv_lag, data = merged_data_7d, cluster = c("lfdn", "wave")) # only convinces libs and moderates


# crime makes libs more conservative
merged_data_7d %>% 
  ggplot(aes(crime_schlepp_share, dv)) +
  geom_jitter(alpha = 0.05) +
  geom_smooth(method = "lm") +
  facet_grid(~imm_att)

merged_data_7d %>% 
  ggplot(aes(crime_share, dv)) +
  geom_jitter(alpha = 0.05) +
  geom_smooth(method = "lm") +
  facet_grid(~imm_att)

# IS news polarise
merged_data_7d %>% 
  ggplot(aes(islamic_state_share, dv)) +
  geom_jitter(alpha = 0.05) +
  geom_smooth(method = "lm") +
  facet_grid(~imm_att)

# no reaction to stock news
merged_data_7d %>% 
  ggplot(aes(stocks_cee_share, dv)) +
  geom_jitter(alpha = 0.05) +
  geom_smooth(method = "lm") +
  facet_grid(~imm_att)

# mediterranean tragedy makes conservatives more liberal (notrobust)
merged_data_7d %>% 
  ggplot(aes(mediterranean_share, dv)) +
  geom_jitter(alpha = 0.05) +
  geom_smooth(method = "lm") +
  facet_grid(~imm_att)

summary(lm(dv ~ mediterranean_share*imm_att, data = merged_data_7d)) # significant positive effect on conservatives
summary(lm(dv ~ mediterranean_share*imm_att + dv_lag, data = merged_data_7d)) # disappears with lagged dv
feglm(dv ~ mediterranean_share*imm_att + dv_lag, data = merged_data_7d, cluster = c("lfdn", "wave")) # but not significant
feglm(dv ~ mediterranean_share*imm_att + dv_lag| wave, data = merged_data_7d, cluster = c("lfdn", "wave")) # but not significant

merged_data_7d %>% 
  ggplot(aes(sr_medit_share, dv)) +
  geom_jitter(alpha = 0.05) +
  geom_smooth(method = "lm") +
  facet_grid(~imm_att)

summary(lm(dv ~ sr_medit_share*imm_att, data = merged_data_7d)) # liberals have stronger backlash than conservatives
summary(lm(dv ~ mediterranean_share*imm_att + dv_lag, data = merged_data_7d)) # disappears with lagged dv
feglm(dv ~ mediterranean_share*imm_att + dv_lag, data = merged_data_7d, cluster = c("lfdn", "wave")) # but not significant
feglm(dv ~ mediterranean_share*imm_att + dv_lag | wave, data = merged_data_7d, cluster = c("lfdn", "wave")) # but not significant


# camps affect liberals and moderates to become more liberal!
merged_data_7d %>% 
  ggplot(aes(camps_greece_share, dv)) +
  geom_jitter(alpha = 0.05) +
  geom_smooth(method = "lm") +
  facet_grid(~imm_att)

summary(lm(dv ~ camps_greece_share*imm_att, data = merged_data_7d)) # highly significant without clustering
summary(lm(dv ~ camps_greece_share*imm_att + dv_lag, data = merged_data_7d)) # highly significant without clustering
feglm(dv ~ camps_greece_share*imm_att + dv_lag, data = merged_data_7d, cluster = c("lfdn", "wave")) # but not significant
feglm(dv ~ camps_greece_share*imm_att + dv_lag | wave, data = merged_data_7d, cluster = c("lfdn", "wave")) # only moderates significant


# salience polarises
merged_data_7d %>% 
  ggplot(aes(n_articles, dv)) +
  geom_jitter(alpha = 0.05) +
  geom_smooth(method = "lm") +
  facet_grid(~imm_att)


# differential consumption
merged_data_7d %>% 
  filter(!is.na(`1130_clean`)) %>% 
  ggplot(aes(as.factor(`1130_clean`), crime_share)) +
  geom_boxplot()

merged_data_7d %>% 
  filter(!is.na(`1130_clean`)) %>% 
  ggplot(aes(`1130_clean`, crime_share)) +
  geom_jitter(alpha = 0.05)+
  geom_smooth(method = "lm")
# liberals seem to consume less crime news (but weak correlation)

summary(lm(crime_share ~ `1130_clean`, data = merged_data_7d)) # more conservative -> more crime news consumed
summary(lm(crime_share ~ imm_att, data = merged_data_7d)) # more conservative -> more crime news
feglm(crime_share ~ `1130_clean`, data = merged_data_7d, cluster = c("lfdn", "wave")) # not robust, but not sure about clustering here
feglm(crime_share ~ imm_att, data = merged_data_7d, cluster = c("lfdn", "wave")) # not robust



## robustness: 30-day lag
merged_data_30d <- 
  merged_data %>% 
  filter(lag == "30")

# naïve model
merged_data_30d$dv <- merged_data_30d$`1130_clean`
feglm(dv ~ crime_schlepp | lfdn + wave, data = merged_data_30d) # null

# remove missing conditioning variables
merged_data_30d <- 
  merged_data_30d %>% 
  filter(imm_att != "")

# interaction model
feglm(dv ~ crime_schlepp_share*imm_att | wave, data = merged_data_30d) # general positive (more conservative), turns null for libs (but not sig)
feglm(dv ~ n_articles*imm_att | wave, data = merged_data_30d) # general positive (more conservative), turns negative for libs (but not sig)

# polarisation of general migration news -> issue definition matters (show for more issues)
feglm(dv ~ sexual_assault_share, data = merged_data_30d) # not sig
feglm(dv ~ sexual_assault_share*imm_att, data = merged_data_30d) # not sig

# wa


## dv MIP: criminal immigrants

# naïve model
merged_data <- 
  merged_data %>% 
  mutate(dv = `840_c1` == "3411",
         dv_lag = lag(dv),
         ctrl = `840_c1` %in% c(3412:3413, 3750:3759),
         ctrl_lag = lag(ctrl))

feglm(dv ~ crime_schlepp_share + dv_lag | wave, data = merged_data) # positive effect
feglm(ctrl ~ crime_schlepp_share + ctrl_lag | wave, data = merged_data) # stronger positive effect


# interaction model
feglm(dv ~ crime_schlepp*imm_att | wave, data = merged_data) # general positive (more conservative), turns null for libs (but not significant)


# vis ####

