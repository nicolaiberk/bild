# ______________________________________________
# Bild
# Goal: Estimate FE model w values and attentiveness
# Procedure: load, estimation, save
# ______________________________________________
# Date:  Tue Mar 01 23:22:33 2022
# Author: Nicolai Berk
#  R version 4.1.1 (2021-08-10)
# ______________________________________________


# Setup ####
library(tidyverse)
library(dplyr)
library(here)
library(data.table)
library(fixest)

# run other scripts (only once)

# source(here("code/01a_preprocessing_dv.R")) # clean survey data
# rm(list = ls())
# source(here("code/03a_merge.R")) # merge to text estimates
# rm(list = ls())


# load ####
merged_data <- fread(file = paste0("data/merged_2022-03-05.csv"))

merged_data <- 
  merged_data %>% 
  mutate(crime_share = crime_schlepp_share + sexual_assault_share,
         # imm_att = factor(imm_att, levels = c("conservative", "moderate", "liberal", "miss")),
         dv = `1130_clean`) %>% 
  filter(lag == 7)

merged_data <- 
  merged_data %>% 
  filter(!is.na(`1130_clean`)) %>% 
  group_by(lfdn) %>% 
  arrange(lfdn, date_clean.x) %>% 
  select(`1130_clean`, lfdn) %>% 
  slice(1) %>% 
  mutate(init_mig = `1130_clean`) %>%
  select(init_mig, lfdn) %>% 
  right_join(merged_data, by = "lfdn")

# add lagged crime_share and dv
merged_data <- 
  merged_data %>%
  ungroup() %>% 
  arrange(lfdn, date_clean.x) %>% 
  group_by(lfdn) %>% 
  filter(!is.na(crime_share)) %>% 
  mutate(crime_share_lag = lag(crime_share),
         dv_lag = lag(dv))

# estimation ####

# individual exposure ####

# direct crime
feglm(dv ~ crime_share | wave + lfdn,  # positive, p < 0.1
      cluster = c("lfdn"), data = merged_data)

feglm(dv ~ crime_share + dv_lag | wave,  # positive, highly significant
      cluster = c("lfdn"), data = merged_data)

# crime X initial imm_att
feglm(dv ~ crime_share:init_mig + crime_share | wave + lfdn,  
      cluster = c("lfdn"), data = merged_data %>% filter(imm_att != ""))
# conservatives react less (highly significant)!

feglm(dv ~ crime_share:dv_lag + crime_share | wave + lfdn,  
      cluster = c("lfdn"), data = merged_data)
# conservatives react least (highly significant)!

feglm(dv ~ crime_share:dv_lag + crime_share + dv_lag | wave,  
      cluster = c("lfdn"), data = merged_data)
# no difference between - the difference between the last two estimates 
#   should be the central concern when mailing Will!

feglm(dv ~ crime_share:init_mig + crime_share | wave + lfdn,  
      cluster = c("lfdn"), data = merged_data)

feglm(dv ~ crime_share:init_mig + crime_share + init_mig | wave,  
      cluster = c("lfdn"), data = merged_data)

# however holds when only considering initial opinion

# can interaction be explained with past crime exposure?

feglm(dv ~ crime_share + crime_share:dv_lag + crime_share:crime_share_lag + dv_lag | wave,  
      cluster = c("lfdn"), data = merged_data)

feglm(dv ~ crime_share + crime_share:dv_lag + crime_share:crime_share_lag | wave + lfdn,  
      cluster = c("lfdn"), data = merged_data)

feglm(dv ~ crime_share + crime_share:dv_lag + crime_share:crime_share_lag + init_mig | wave,  
      cluster = c("lfdn"), data = merged_data)

# past crime share exposure conditioning explains dv_lag conditioning in dv_lag model, but not lfdn model

# but figure out direct effect first

## plot 
data_hyp <- 
  expand.grid(
    crime_share = seq(0, 0.2, 0.01),
    init_mig = seq(-3,3,1),
    wave = as.character(1:10),
    dv_lag = -3:3
  ) %>% 
  as.data.frame()

data_hyp$pred <- 
  predict(crime_init_m, newdata = data_hyp)

data_hyp %>% 
  group_by(crime_share, init_mig) %>% 
  summarise_all(mean, na.rm = T) %>% 
  ggplot(aes(y = pred, x = crime_share, col = as.factor(init_mig))) +
  geom_line() +
  geom_point()


# positive effect among moderates, increases for libs and decreases for conservatives


# crime square term - diminishing returns, but not significant
feglm(dv ~ poly(crime_share, 2) | wave + lfdn,  
      cluster = c("lfdn"), data = merged_data %>% 
        filter(!is.na(crime_share)))

feglm(dv ~ crime_share + crime_share_lag | wave + lfdn,  
      cluster = c("lfdn"), data = merged_data)

# actually negative effect of lagged var (regression to mean?)

feglm(dv ~ crime_share*init_mig + dv_lag | wave,  # lagged dv instead of fe to capture stable individual traits explaining dv (bc collinearity)
      cluster = c("lfdn"), 
      data = merged_data)

# wtf now conservatives react more strongly?

feglm(dv ~ crime_share*init_mig + crime_share_lag | wave + lfdn,  
      cluster = c("lfdn"), 
      data = merged_data)
# and interaction effect persists


# partyid
merged_data <- 
  merged_data %>% 
  mutate(pid = `190b_clean`)

feglm(dv ~ crime_share * pid | wave + lfdn,  
      cluster = c("lfdn"), data = merged_data)


# crime X values
feglm(dv ~ crime_share * security_init | wave + lfdn, 
      cluster = c("lfdn"), data = merged_data)

feglm(dv ~ crime_share * conformity_init | wave + lfdn, 
      cluster = c("lfdn"), data = merged_data)



# crime X political knowledge
feglm(dv ~ crime_share * polknowl_init | wave + lfdn, 
      cluster = c("lfdn"), data = merged_data)

# crime x values x polknowl - nothing
feglm(dv ~ crime_share * security_init * polknowl_init | wave + lfdn, 
      cluster = c("lfdn"), data = merged_data)



# crime X salience - nothing
feglm(dv ~ crime_share * n_articles | wave + lfdn, 
      cluster = c("lfdn"), data = merged_data)

## might estimate bootstrap here





## case-based estimates ####
merged_data$post <- merged_data$date_clean.x >= as.Date("2017-02-01")
merged_data$treat <- merged_data$readership == "Bild"

## DIRECT 
feglm(dv ~ post * treat |  lfdn, 
      cluster = c("lfdn"), 
      data = merged_data %>% 
        filter(date_clean.x >= as.Date("2016-01-01"),
               date_clean.x <  as.Date("2018-01-01")))
# no direct effect


## x past opinion
feglm(dv ~ post * treat * init_mig |  lfdn, 
      cluster = c("lfdn"), 
      data = merged_data %>% 
        filter(date_clean.x >= as.Date("2016-01-01"),
               date_clean.x <  as.Date("2018-01-01")))

# decrease among conservatives also visible here

# no FE's

gles_p_long <- fread(here('data/gles/Panel/long_cleaned.csv'))

gles_p_long$dv <- gles_p_long$`1130_clean`
gles_p_long$treat <- gles_p_long$`1661a_bin`
gles_p_long$post <- gles_p_long$date_clean>= as.Date("2017-02-01")
gles_p_long <- 
  gles_p_long %>% 
  select(lfdn, date_clean, `1130_clean`) %>% 
  filter(!is.na(`1130_clean`)) %>% 
  arrange(lfdn, date_clean) %>% 
  group_by(lfdn) %>% 
  slice(1) %>% 
  mutate(init_mig = `1130_clean`) %>% 
  select(lfdn, init_mig) %>% 
  right_join(gles_p_long, by = "lfdn")

gles_p_long <- 
  gles_p_long %>% 
  mutate(mip_mig_crime_lag = lag(mip_mig_crime)) %>% 
  filter(!is.na(mip_mig_crime)) %>% 
  arrange(lfdn, date_clean) %>% 
  group_by(lfdn) %>% 
  slice(1) %>% 
  mutate(mip_mig_crime_init = mip_mig_crime) %>% 
  select(lfdn, mip_mig_crime_init) %>% 
  right_join(gles_p_long, by = "lfdn")

# conditioning effect of prior opinion
feglm(dv ~ post * treat * init_mig | lfdn, 
      cluster = c("lfdn"), 
      data = gles_p_long %>% 
        filter(date_clean >= as.Date("2016-01-01"),
               date_clean <  as.Date("2018-01-01")))

# conditioning effect of prior mention of criminal migrants being an issue
table(gles_p_long$mip_mig_crime_init, gles_p_long$treat)

feglm(dv ~ post * treat * init_mig + post * treat * mip_mig_crime_init, 
      cluster = c("lfdn"), 
      data = gles_p_long %>% 
        filter(date_clean >= as.Date("2016-01-01"),
               date_clean <  as.Date("2018-01-01"),
               !is.na(post), !is.na(treat), !is.na(dv))) %>% 
  summary()

summary(feglm(dv ~ post * treat * mip_mig_crime_init + init_mig, 
      cluster = c("lfdn"), 
      data = gles_p_long %>% 
        filter(date_clean >= as.Date("2016-01-01"),
               date_clean <  as.Date("2018-01-01"),
               !is.na(post), !is.na(treat), !is.na(dv))))


###### END TEMP #######

## x values

## x attentiveness

## x salience

# save ####
