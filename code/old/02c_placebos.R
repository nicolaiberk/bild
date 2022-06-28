# ______________________________________________
# Media framing effects
# Goal: Visualise treatment strength & placebos
# Procedure: load data, visualise
# ______________________________________________
# Date:  Mon Jan 03 10:36:01 2022
# Author: Nicolai Berk
#  R version 4.1.1 (2021-08-10)
# ______________________________________________


# Setup ####
library(tidyverse)
library(dplyr)
library(here)
library(data.table)
library(lubridate)
library(rdrobust)

# descriptive change ####


## load & transform data frame attention
load(here("data/media_daily_2022-02-02.Rdata"))

merged_media <- 
  merged_media %>% 
  mutate(paper = recode(paper, 
                        'bild' = 'Bild', 'faz' = 'FAZ', 'spon' = 'Spiegel',
                        'sz' = 'SZ', 'taz' = 'TAZ', 'welt' = 'Welt'),
         post = date_new > as.Date("2017-02-01"),
         date_month = floor_date(as.Date(date_new), 'month'))

## visualise

### relative change
merged_media %>% 
  select(date_month, paper:share_mig, english_share:hungary_referendum_share) %>%
  group_by(paper, date_month) %>%
  summarise(across(share_mig:hungary_referendum_share,mean, na.rm = T)) %>%
  pivot_longer(
    cols = !c(paper, date_month),
    names_to = "frame",
    values_to = "attention"
  ) %>%
  mutate(post = date_month >= as.Date("2017-02-01")) %>% 
  filter(date_month < as.Date("2020-01-01")) %>% 
  filter(paper %in% c("Bild", "Welt", "FAZ")) %>% 
  ggplot(aes(x = date_month, y = attention, col = paper, lty = paper)) +
  geom_line() +
  geom_vline(xintercept = as.Date("2017-02-01"), lty = 2, col = "red") +
  facet_wrap(~frame) +
  ylim(c(0, 0.2)) +
  ggtitle("Relative attention to selected migration frames in Bild and Welt, 2013-2018")


# crime only
merged_media %>% 
  mutate(
    date_month = floor_date(as.Date(date_new), 'quarter')
  ) %>%
  group_by(paper, date_month) %>%
  summarise(across(share_mig:hungary_referendum_share,mean, na.rm = T),
            across(n_mig_sal:n_tot, sum, na.rm = T)) %>%
  mutate(crime_share = crime_schlepp_share + sexual_assault_share,
         post = date_month >= as.Date("2017-02-01")) %>% 
  filter(date_month < as.Date("2020-01-01")) %>% 
  ggplot(aes(x = date_month, y = crime_share, col = paper, lty = paper)) +
  geom_line() +
  geom_vline(xintercept = as.Date("2017-02-01"), lty = 2, col = "red") +
  ggtitle("Relative attention to crime frames, 2013-2019") + 
  xlab("Date") + ylab("Share of crime content") +
  theme_minimal()

ggsave(here("paper/vis/desc_treatment_crime.png"))


merged_media %>% 
  group_by(paper, date_new) %>%
  summarise(across(share_mig:hungary_referendum_share,mean, na.rm = T),
            across(n_mig_sal:n_tot, sum, na.rm = T)) %>%
  mutate(crime_share = crime_schlepp_share + sexual_assault_share,
         post = date_new >= as.Date("2017-02-01")) %>% 
  filter(date_new < as.Date("2018-01-01"),
         date_new >= as.Date("2016-01-01")) %>% 
  # filter(paper %in% c("Bild", "Welt", "FAZ")) %>% 
  ggplot(aes(x = date_new, y = crime_share, col = paper, lty = paper)) +
  geom_point(alpha = 0.1) +
  geom_smooth() +
  # geom_line() +
  geom_vline(xintercept = as.Date("2017-02-01"), lty = 2, col = "red") +
  # facet_wrap(~frame, scales = "free") +
  ggtitle("Relative attention to crime frames, 2016-2018") + 
  xlab("Date") + ylab("Share of crime content") +
  theme_minimal()

ggsave(here("paper/vis/desc_treatment_crime_focus.png"))


### salience
merged_media %>% 
  select(date_month, paper, share_mig) %>% 
  filter(date_month >= as.Date("2016-01-01")) %>% 
  filter(date_month < as.Date("2018-01-01")) %>% 
  group_by(paper, date_month) %>% 
  summarise_all(mean, na.rm = T) %>% 
  mutate(paper = recode(paper, 
                        'bild' = 'Bild', 'faz' = 'FAZ', 'spon' = 'Spiegel',
                        'sz' = 'SZ', 'taz' = 'TAZ', 'welt' = 'Welt')) %>%
  # filter(paper %in% c("Bild", "Welt", "FAZ")) %>% 
  ggplot(aes(x = date_month, y = share_mig, col = paper, lty = paper)) +
  geom_point(alpha = 0.1) +
  geom_smooth() +
  geom_vline(xintercept = as.Date("2017-01-01"), lty = 2, col = "red") +
  ggtitle("Relative salience of migration issue in Bild and Welt, 2013-2018")
ggsave(here("paper/vis/desc_treatment_salience.png"))



# simple DiD ####
merged_media <- 
  merged_media %>% 
  filter(date_new < as.Date("2018-01-01"),
         date_new >= "2016-01-01")

# ## generate dvs
# merged_media <- 
#   merged_media %>% 
#   mutate(
#     date_new = floor_date(as.Date(date_new), 'day')
#   ) %>% 
#   select(-c(crime, medit, deport, refnums, camps, labmar, capcrime,  as.character(1:60))) %>% 
#   group_by(paper, date_new) %>% 
#   summarise_all(sum, na.rm = T) %>% 
#   mutate(paper = recode(paper, 
#                         'bild' = 'Bild', 'faz' = 'FAZ', 'spon' = 'Spiegel',
#                         'sz' = 'SZ', 'taz' = 'TAZ', 'welt' = 'Welt'))


## other papers, same frame, same date
treatment_ests <- 
  data.frame()

merged_media$dv    <-  merged_media[["crime_schlepp_share"]] + merged_media[["sexual_assault_share"]]
merged_media$post  <-  merged_media$date_new >= as.Date("2017-02-01")

for (p in unique(merged_media$paper)){
  
  merged_media$treatment <-  merged_media$paper == p
  
  did_model <- lm(dv ~ post*treatment, data = merged_media)
  
  est <- did_model$coefficients["postTRUE:treatmentTRUE"][[1]]
  lower <- confint(did_model)["postTRUE:treatmentTRUE", 1]
  upper <- confint(did_model)["postTRUE:treatmentTRUE", 2]
  
  treatment_ests <- 
    rbind(treatment_ests,
          data.frame(
            model = "DiD",
            paper = p,
            est   = est,
            lower = lower,
            upper = upper
          ))
}

## vis
treatment_ests$paper <- 
  factor(treatment_ests$paper,
          levels = treatment_ests %>%
                    filter(model == "DiD") %>% 
                    arrange(est) %>% 
                    select(paper) %>%
                    unlist()
    )


treatment_ests %>% 
  ggplot(aes(x = est, xmin = lower, xmax = upper, 
             y = paper, col = paper != "Bild")) +
  geom_pointrange() +
  geom_vline(xintercept = 0,
             col = "red", lty = 2) +
  theme_minimal() +
  xlab("Estimated DiD") + ylab("") +
  ggtitle("Placebo papers 2016 - 2017") +
  theme(legend.position = "None") 
  # facet_grid(~model, scales = "free")
ggsave(filename = here(paste0("paper/vis/placebo_papers", Sys.Date(), ".png")))


## same paper, other frames, same date
treatment_ests <- 
  data.frame()

merged_media$treatment <- merged_media$paper == "Bild"

for (t in colnames(merged_media %>% select(english_share:hungary_referendum_share))){
  merged_media$dv  <-  merged_media[[t]]

  did_model <- lm(dv ~ post*treatment, data = merged_media)
  est <- did_model$coefficients["postTRUE:treatmentTRUE"][[1]]
  lower <- confint(did_model)["postTRUE:treatmentTRUE", 1]
  upper <- confint(did_model)["postTRUE:treatmentTRUE", 2]
  
  treatment_ests <- 
    rbind(treatment_ests,
          data.frame(
            model = "DiD",
            topic = t,
            est   = est,
            lower = lower,
            upper = upper
          ))
      
}

## vis

treatment_ests %>% 
  ggplot(aes(x = est, xmin = lower, xmax = upper, 
             y = topic, col = !(topic %in% c("crime_schlepp_share", "sexual_assault_share")))) +
  geom_pointrange() +
  geom_vline(xintercept = 0,
             col = "red", lty = 2) +
  theme_minimal() +
  xlab("Estimated DiD") + ylab("") +
  ggtitle("Placebo frames 2016 - 2017") +
  theme(legend.position = "None")
  # facet_grid(~model, scales = "free")
ggsave(filename = here(paste0("paper/vis/placebo_topics_", Sys.Date(), ".png")),
       width = 6, height = 12)


## same paper, same frame, other dates
load(here("data/media_daily_2022-02-02.Rdata"))

treatment_ests <- 
  data.frame()

merged_media$dv  <-  merged_media[["crime_schlepp_share"]] + merged_media[["sexual_assault_share"]]
merged_media$treatment <- merged_media$paper == "Bild"

for (y in 2014:2018){
  
  merged_media$post <-  
    merged_media$date_new > as.Date(paste(y, "02", "01", sep = "-"))
  
  ## restrict sample to pre-post year
  prevalence_temp <- 
    merged_media %>% 
    filter(date_new >= as.Date(paste0(y-1, "-01-01")),
           date_new < as.Date(paste0(y+1, "-01-01")))
  
  ## DiD
  
  did_model <- lm(dv ~ post*treatment, data = prevalence_temp)
  
  est <- did_model$coefficients["postTRUE:treatmentTRUE"][[1]]
  lower <- confint(did_model)["postTRUE:treatmentTRUE", 1]
  upper <- confint(did_model)["postTRUE:treatmentTRUE", 2]
  
  treatment_ests <- 
    rbind(treatment_ests,
          data.frame(
            year = y,
            est = est,
            lower = lower,
            upper = upper
          ))
}


## vis
treatment_ests %>% 
  ggplot(aes(x = est, xmin = lower, xmax = upper, 
             y = year, col = year != 2017)) +
  geom_pointrange() +
  geom_vline(xintercept = 0,
             col = "red", lty = 2) +
  theme_minimal() +
  xlab("Estimate") + ylab("") +
  ggtitle("Placebo dates") +
  theme(legend.position = "None")
  # facet_grid(~model, scales = "free")
ggsave(filename = here(paste0("paper/vis/placebo_dates_", Sys.Date(), ".png")))

