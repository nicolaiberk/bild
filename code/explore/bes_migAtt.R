# ______________________________________________
# Bild
# Goal: Assess opinion change UK readers of Daily Mail
# Procedure: load data, define vars, model & plot
# ______________________________________________
# Date:  Tue Aug 23 14:32:35 2022
# Author: Nicolai Berk
#  R version 4.1.1 (2021-08-10)
# ______________________________________________


# Setup ####
library(tidyverse)
library(dplyr)
library(here)
library(data.table)

# load data, transform ####

# bes_panel <- 
#   haven::read_dta(here("data/raw/bes/BES2019_W21_Panel_v21.0.dta")) %>% 
#   select(id, contains("papersLast"), contains("migra"), contains("Read"), contains("date"))
# 
# save(bes_panel, file = here("data/raw/bes/bes_subset.Rdata"))

load(here("data/raw/bes/bes_subset.Rdata"))

bes_panel <- 
  bes_panel %>% 
  pivot_longer(immigrantsWelfareStateW1:respdateW20, 
               names_to = c(".value", "wave"),
               names_pattern = "(.*)W(.+)"
               )

# define vars ####

## dv: immigration attitude

### "Immigrants are a burden on the welfare state"
bes_panel <- 
  bes_panel %>% 
  mutate(dv = ifelse(immigrantsWelfareState == 9999, NA, immigrantsWelfareState-3)) # higher = immigrants higher burden


## treatment: reader of daily mail in last pre-wave (online or print)
bes_panel <- 
  bes_panel %>% 
  filter(wave == 6) %>% 
  mutate(mail_reader = p_paper_read == 2) %>% 
  select(id, mail_reader) %>% 
  right_join(bes_panel, by = "id")

# check if ever read daily mail
bes_panel <- 
  bes_panel %>% 
  filter(as.integer(wave) > 6) %>% 
  select(id, wave, p_paper_read) %>% 
  group_by(id) %>% 
  summarise(mail_ever = sum(p_paper_read == 2)) %>% 
  filter(!is.na(mail_ever)) %>% 
  right_join(bes_panel, by = "id")

# recode treatment to NA if not never read bild
bes_panel <- 
  bes_panel %>% 
  mutate(treatment = ifelse(mail_reader, T, ifelse(mail_ever == 0, F, NA)))

## timing: August 2015

### clean time var

bes_panel <- 
  bes_panel %>% 
  mutate(
    fieldend = 
      case_when(
        wave == '1'  ~ '2014-03-09',
        wave == '2'  ~ '2014-06-25',
        wave == '3'  ~ '2014-10-17',
        wave == '4'  ~ '2015-03-30',
        wave == '5'  ~ '2015-05-06',
        wave == '6'  ~ '2015-05-26',
        wave == '7'  ~ '2016-05-04',
        wave == '8'  ~ '2016-06-22',
        wave == '9'  ~ '2016-07-04',
        wave == '10' ~ '2016-12-12',
        wave == '11' ~ '2017-05-03',
        wave == '12' ~ '2017-06-07',
        wave == '13' ~ '2017-06-23',
        wave == '14' ~ '2018-05-21',
        wave == '15' ~ '2019-03-29',
        wave == '16' ~ '2019-06-18',
        wave == '17' ~ '2019-11-12',
        wave == '18' ~ '2019-12-11',
        wave == '19' ~ '2019-12-23',
        wave == '20' ~ '2020-06-21',
        wave == '21' ~ '2021-05-25'
        
      )
  ) %>% 
  mutate(post = as.Date(fieldend) < as.Date("2015-08-01"))


# model & plot ####

## plot trends - no real discernible effect
bes_panel %>% 
  filter(!is.na(treatment), !is.na(dv)) %>% 
  group_by(fieldend, treatment) %>% 
  summarise(dv_mean = mean(dv, na.rm = T),
            dv_sd = sd(dv, na.rm = T),
            N = n(),) %>% 
  mutate(
    dv_lower = dv_mean + qt(0.025, df = N-1)*(dv_sd/sqrt(N)),
    dv_upper = dv_mean + qt(0.975, df = N-1)*(dv_sd/sqrt(N))
  ) %>% 
  ggplot(aes(x = as.Date(fieldend),
             y = dv_mean, 
             ymin = dv_lower, 
             ymax = dv_upper,
             col = treatment)) +
  geom_line() +
  geom_pointrange() +
  geom_vline(xintercept = as.Date("2015-08-01")) +
  theme_minimal()

## model - null
single_model <- 
  fixest::feglm(
  
    dv ~ treatment*post | wave + id,
    data = bes_panel

    )

single_model

## very small effect (0.08 on 5-point scale)

## plot effect (cannot reject minimal effect)
size <- 1
theoretical_effect_size <- 0.5 # aka 10% of the scale
boundary_share <- 0.25 # minimal relevant effect size

modelsummary::modelplot(
  list(dv_name = single_model),
  coef_map = list("treatmentTRUE:postTRUE" = ""),
  facet = T
) +
  geom_vline(xintercept = 0, lty = 1, col = "red", size = size) +
  geom_vline(xintercept = boundary_share*theoretical_effect_size, 
             lty = 3, col = "black", size = size) +
  geom_vline(xintercept = boundary_share*-1*theoretical_effect_size, 
             lty = 3, col = "black", size = size) +
  geom_vline(xintercept = theoretical_effect_size, 
             lty = 2, col = "black", size = size) +
  geom_vline(xintercept = -1*theoretical_effect_size, 
             lty = 2, col = "black", size = size)


# unclear if daily mail coverage changed so much (it did according to Reichelt)
# migration might have been politicised in UK already at the time?
# generally, this does not tell us much as no strong expectations from coverage
