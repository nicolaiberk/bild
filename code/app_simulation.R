# ______________________________________________
# Bild
# Goal: Simulation of FE model
# Procedure: generate data, estimate models, visualise
# ______________________________________________
# Date:  Fri Apr 29 14:55:00 2022
# Author: Nicolai Berk
#  R version 4.1.1 (2021-08-10)
# ______________________________________________


# Setup ####
library(tidyverse)
library(dplyr)
library(here)
library(data.table)

set.seed(42)

# generate data ####




## parameters
n_ids <- 10000
n_waves <- 5

# add effect size and treatment size in DiD to make FE and DiD comparable
# -> effect should be the same, just treatment strength should differ


## observations
id <- 1:n_ids
wave <- 1:n_waves

## values
id_fe <- rnorm(n_ids)
wave_fe <- rnorm(n_waves)

# generate empty dataframe
df <- 
  expand.grid(ID = id, 
              wave = wave)

df$id_fe <- id_fe

df <- 
  df %>% 
  arrange(ID)

df$wave_fe <- wave_fe

df <- 
  df %>% 
  filter(wave == 1) %>% 
  mutate(imm_init = round(rnorm(n_ids))) %>% 
  select(imm_init, ID) %>% 
  right_join(df)

df <- 
  df %>% 
  mutate(post = as.integer(wave > 1)) %>% 
  mutate(treat = as.integer(ID < (max(ID)/2))) %>% 
  mutate(epsilon = rnorm(nrow(df))) %>% 
  mutate(imm_att = round(imm_init + id_fe + wave_fe + post*treat*((imm_init-3)*-0.1) + epsilon))


## plot

df %>% 
  group_by(wave, treat) %>% 
  summarise(imm_att = mean(imm_att)) %>% 
  ggplot(aes(x = wave, y = imm_att, col = as.factor(treat))) +
  geom_line()

df %>% 
  mutate(group_treat = paste(treat, imm_init)) %>% 
  group_by(wave, group_treat) %>% 
  summarise(imm_att = mean(imm_att), treat = mean(treat)) %>% 
  ggplot(aes(x = wave, y = imm_att, group = as.factor(group_treat), col = as.factor(treat))) +
  geom_line()

# estimate models ####

## direct
fixest::feglm(imm_att ~ post*treat, cluster = 'ID',
              data = df)

## dependent on prior attitude
fixest::feglm(imm_att ~ post*treat*imm_init, cluster  = 'ID',
              data = df)

## visualise
df %>% 
  group_by(treat, post, imm_init) %>% 
  summarise(
    dv_point = mean(imm_att, na.rm = T)
  ) %>% 
  filter(post == 1) %>% 
  pivot_wider(
    names_from = treat,
    values_from = dv_point
  ) %>% 
  mutate(difference = `1` - `0`) %>% 
  ggplot(aes(x = imm_init, y = difference)) +
  geom_col(fill = 'lightblue') + 
  theme_minimal() + xlab("") + ylab("") +
  ggtitle("Pre-post differences of treated and untreated across initial migration attitude")


inter <- fixest::feglm(imm_att ~ post*treat*imm_init, cluster  = 'ID',
              data = df)

direct <- coef(inter)["post:treat"]
interact <- coef(inter)["post:treat:imm_init"]

ggplot() +
  geom_histogram(data = df %>% filter(wave == 1), mapping = aes(x = imm_init, after_stat(density)), alpha = 0.4, bins = 8) +
  geom_abline(slope = interact, intercept = direct, col = "red") +
  theme_minimal() +
  geom_hline(yintercept = 0, col = "black") +
  scale_x_continuous(limits=c(-3.5,3.5)) +
  scale_y_continuous(limits=c(0,.6)) +
  ylab("Estimate") + xlab("Initial migration attitude")




## fe model ####
df <- 
  df %>% 
  mutate(crime_share = rbeta(n_ids*n_waves, 0.5, 1)) %>% 
  mutate(imm_att = round(id_fe + wave_fe + crime_share*((imm_init-3)*-0.1) + epsilon))



fixest::feglm(imm_att ~ crime_share | wave + ID,
      cluster = c("ID"), data = df)

# with interaction
fixest::feglm(imm_att ~ crime_share:imm_init + crime_share | wave + ID,  
        cluster = c("ID"), data = df)

# with interaction and both id_fes and initial attitude
## still correctly identified, but initial attitude dropped due to MC
fixest::feglm(imm_att ~ crime_share*imm_init + crime_share | wave + ID,  
              cluster = c("ID"), data = df)

# with interaction and only initial attitude
## correctly identified
fixest::feglm(imm_att ~ crime_share*imm_init + crime_share | wave,  
              cluster = c("ID"), data = df)



# plot
ind_inter <- 
  fixest::feglm(imm_att ~ crime_share:imm_init + crime_share | wave + ID,  
                cluster = c("ID"), data = df)

direct <- coef(ind_inter)["crime_share"]
interact <- coef(ind_inter)["crime_share:imm_init"]

ggplot() +
  geom_histogram(data = df %>% filter(wave == 1), mapping = aes(x = imm_init, after_stat(density)), alpha = 0.4, bins = 8) +
  geom_abline(slope = interact, intercept = direct, col = "red") +
  theme_minimal() +
  geom_hline(yintercept = 0, col = "black") +
  scale_x_continuous(limits=c(-3,3)) +
  scale_y_continuous(limits=c(0,0.6)) +
  ylab("Estimate") + xlab("Initial migration attitude")



