# ______________________________________________
# Media effects
# Goal: Estimate case DiDs with bootstraps
# Procedure: load data, define vars, estimation
# ______________________________________________
# Date:  Thu Feb 03 14:54:48 2022
# Author: Nicolai Berk
#  R version 4.1.1 (2021-08-10)
# ______________________________________________


# Setup ####
library(tidyverse)
library(dplyr)
library(here)
library(data.table)
library(fixest)
library(boot)

# load data ####

## load cleaned survey data
gles_p_long <- 
  fread(here('data/gles/Panel/long_cleaned.csv'))

## get rid of missings in relevant waves to avoid sample attrition
gles_p_wide_nona <- 
  gles_p_long %>% 
  filter(wave %in% c(1, 3, 4, 6:8)) %>% 
  select(lfdn, wave, date_clean,
         # voteint_w1, # party vote
         # `1661a_bin`, # Bild readership
         # `1661a_clean`, # Bild days/week
         contains("1661"), # newspaper use
         `1600_clean`, # political internet use
         # `1610_bin`, # political social media use
         print_sources_n, # n of newspapers consumed
         news_sources_n, # print and tv sources consumed
         `1130_clean`, # immigration attitude
         imm_att # immigration attitude first wave
  ) %>% 
  pivot_wider(id_cols = lfdn, names_from = wave, 
              values_from = date_clean:imm_att) %>% 
  na.omit()



# define vars ####
gles_p_long_nona <- 
  gles_p_wide_nona %>% 
  pivot_longer(cols = `date_clean_1`:`imm_att_8`, 
               names_to = c(".value", "wave"),
               names_pattern = "(.*)_(.)"
  )
rm(gles_p_wide_nona)

fwrite(gles_p_long_nona, file = here('data/gles/Panel/long_cleaned_nona.csv'))

## define post-period
gles_p_long_nona$post <- 
  gles_p_long_nona$date_clean >= as.Date("2017-02-01")

## define treatment: read Bild or not
gles_p_long_nona$treat <- 
  gles_p_long_nona$`1661a_bin` == T

## define dv
gles_p_long_nona$dv <- gles_p_long_nona$`1130_clean`

# estimation ####

## test raw model with and without individual fixed effects
summary(lm(dv ~ treat*post, data = gles_p_long_nona)) # not significant 
summary(fixest::feglm(dv ~ treat*post | lfdn, cluster = "lfdn", data = gles_p_long_nona)) 
# fe: null effect - however this is the accurate model for showing persuasion effects!

summary(lm(dv ~ treat*post + imm_att, data = gles_p_long_nona)) # not significant 


## model interaction with liberal starting position
summary(lm(dv ~ treat*post*imm_att, data = gles_p_long_nona)) # slightly significant difference of liberals

## but is this just regression to the mean? compare to other right-wing readers
table(gles_p_long_nona$`1661a_bin`, gles_p_long_nona$imm_att) # Bild
table(gles_p_long_nona$`1661c_bin`, gles_p_long_nona$imm_att) # FAZ
table(gles_p_long_nona$`1661f_bin`, gles_p_long_nona$imm_att) # Welt

gles_long_cons <- 
  gles_p_long_nona %>% 
  filter(`1661a_bin` | `1661c_bin` | `1661f_bin`)

summary(lm(dv ~ treat*post*imm_att, data = gles_long_cons)) # significant shift persists, even compared to other right-wing readers

## THIS IS NOT REGRESSION TO THE MEAN! ##

# raw model compared to conservatives
summary(lm(dv ~ treat*post, data = gles_long_cons)) # not significant, but positive



# subsample for testing
# boot_sample <-
#   gles_p_long_nona %>%
#   select(lfdn) %>%
#   unique() %>%
#   sample_n(1000) %>%
#   unlist()
# 
# gles_p_long_nona <-
#   gles_p_long_nona %>%
#   filter(lfdn %in% boot_sample)

# define bootstrap parameter
L <- 1000


## standard did with blocked bootstrap ####
did <- function(x, i) {
  mydata <- do.call("rbind", lapply(i, function(n) subset(gles_p_long_nona, lfdn==x[n])))
  coefficients(lm(dv ~ treat * post,  data = mydata))
}

units <- unique(gles_p_long_nona$lfdn)

b0 <- boot(units, did, L, parallel = "snow")

cis <- boot.ci(b0, index = 4, type = "perc")$percent

# define point estimate, upper and lower CI
df <- data.frame(
  variable = "",
  group = "All",
  estimate = mean(b0$t[,4]),
  lower = quantile(b0$t[,4], probs = 0.025),
  upper = quantile(b0$t[,4], probs = 0.975)
)



## add conditioning vars ####

### initial migration stance ####
did_mig <- function(x, i) {
  mydata <- do.call("rbind", lapply(i, function(n) subset(gles_p_long_nona, lfdn==x[n])))
  m <- lm(dv ~ treat * post * imm_att,  data = mydata)
  lib_est <- 
    coefficients(m)["treatTRUE:postTRUE"] +
    coefficients(m)["treatTRUE:postTRUE:imm_attliberal"]
  
  mod_est <- 
    coefficients(m)["treatTRUE:postTRUE"] +
    coefficients(m)["treatTRUE:postTRUE:imm_attmoderate"]
  
  con_est <- 
    coefficients(m)["treatTRUE:postTRUE"]
  
  return(c(lib_est, mod_est, con_est))
}


b0 <- boot(units, did_mig, L, parallel = "snow")

# define point estimate, upper and lower CI
df <- data.frame(
  variable = "initial migration attitude", 
  group = factor(c("liberal", "moderate", "conservative"),
                 levels = c("liberal", "moderate", "conservative"),
                 ordered = T),
  estimate = apply(b0$t, 2, mean),
  lower = apply(b0$t, 2, quantile, probs = 0.025, na.rm = T),
  upper = apply(b0$t, 2, quantile, probs = 0.975, na.rm = T)
) %>% 
  rbind(df)


### all content political use (1681 + 1661 + 1701) ####
news_sample <- 
  gles_p_long_nona %>% 
  filter(news_sources_n > 0)

news_sample$n_news <- 
  cut(news_sample$news_sources_n,
      breaks = c(0, 1, 5, Inf),
      labels = c("1", "2-5", ">5"))

did_news <- function(x, i) {
  mydata <- do.call("rbind", lapply(i, function(n) subset(news_sample, lfdn==x[n])))
  m <- lm(dv ~ treat * post * n_news,  data = mydata)
  mono_est <- 
    coefficients(m)["treatTRUE:postTRUE"]
  
  med_est <- 
    coefficients(m)["treatTRUE:postTRUE"] +
    coefficients(m)["treatTRUE:postTRUE:n_news2-5"]
  
  max_est <- 
    coefficients(m)["treatTRUE:postTRUE"] +
    coefficients(m)["treatTRUE:postTRUE:n_news>5"]
  
  return(c(mono_est, med_est, max_est))
}


b0 <- boot(units, did_news, L, parallel = "snow")

# define point estimate, upper and lower CI
df <- data.frame(
  variable = rep("news sources", 3),
  group = factor(c("mono", "moderate", "omnivore"), 
                 levels = c("mono", "moderate", "omnivore"), 
                 ordered = T),
  estimate = apply(b0$t, 2, mean),
  lower = apply(b0$t, 2, quantile, probs = 0.025, na.rm = T),
  upper = apply(b0$t, 2, quantile, probs = 0.975, na.rm = T)
) %>% 
  rbind(df)

rm(news_sample)

### number of newspapers consumed (1661 + 1701) ####
print_sample <- 
  gles_p_long_nona %>% 
  filter(news_sources_n > 0)

print_sample$n_papers <- 
  cut(print_sample$print_sources_n,
      breaks = c(0, 1, 3, Inf),
      labels = c("1", "2-3", ">3"))

did_print <- function(x, i) {
  mydata <- do.call("rbind", lapply(i, function(n) subset(print_sample, lfdn==x[n])))
  m <- lm(dv ~ treat * post * n_papers,  data = mydata)
  mono_est <- 
    coefficients(m)["treatTRUE:postTRUE"]
  
  med_est <- 
    coefficients(m)["treatTRUE:postTRUE"] +
    coefficients(m)["treatTRUE:postTRUE:n_papers2-3"]
  
  max_est <- 
    coefficients(m)["treatTRUE:postTRUE"] +
    coefficients(m)["treatTRUE:postTRUE:n_papers>3"]
  
  return(c(mono_est, med_est, max_est))
}


b0 <- boot(units, did_print, L, parallel = "snow")

# define point estimate, upper and lower CI
df <- data.frame(
  variable = rep("print sources", 3),
  group = factor(c("mono", "moderate", "omnivore"), 
                 levels = c("mono", "moderate", "omnivore"), 
                 ordered = T),
  estimate = apply(b0$t, 2, mean),
  lower = apply(b0$t, 2, quantile, probs = 0.025, na.rm = T),
  upper = apply(b0$t, 2, quantile, probs = 0.975, na.rm = T)
) %>% 
  rbind(df)

rm(print_sample)

### online news use ####
gles_p_long_nona$online <- 
  gles_p_long_nona$`1600_clean` %>% 
  cut(breaks = c(-Inf, 0, 3, 6, 7),
      labels = c("0", "1-3", "4-6", "7"))

did_online <- function(x, i) {
  mydata <- do.call("rbind", lapply(i, function(n) subset(gles_p_long_nona, lfdn==x[n])))
  m <- lm(dv ~ treat * post * online,  data = mydata)
  
  none_est <- 
    coefficients(m)["treatTRUE:postTRUE"]
  
  low_est <- 
    coefficients(m)["treatTRUE:postTRUE"] +
    coefficients(m)["treatTRUE:postTRUE:online1-3"]
  
  mod_est <- 
    coefficients(m)["treatTRUE:postTRUE"] +
    coefficients(m)["treatTRUE:postTRUE:online4-6"]
  
  max_est <- 
    coefficients(m)["treatTRUE:postTRUE"] +
    coefficients(m)["treatTRUE:postTRUE:online7"]
  
  return(c(none_est, low_est, mod_est, max_est))
}

b0 <- boot(units, did_online, L, parallel = "snow")

# define point estimate, upper and lower CI
df <- data.frame(
  variable = rep("online sources", 4),
  group = factor(c("0", "1-3", "4-6", "7"), 
                 levels = c("0", "1-3", "4-6", "7"), 
                 ordered = T),
  estimate = apply(b0$t, 2, mean),
  lower = apply(b0$t, 2, quantile, probs = 0.025, na.rm = T),
  upper = apply(b0$t, 2, quantile, probs = 0.975, na.rm = T)
) %>% 
  rbind(df)


# ### party preference ####
# 
# ## load cleaned survey data
# gles_p_long <- 
#   fread(here('data/gles/Panel/long_cleaned.csv'))
# 
# ## get rid of missings in relevant waves to avoid sample attrition
# gles_p_wide_nona <- 
#   gles_p_long %>% 
#   filter(wave %in% c(1, 3, 4, 6:8)) %>% 
#   select(lfdn, wave, date_clean,
#          voteint_w1, # party vote
#          `1661a_bin`, # Bild readership
#          `1130_clean`, # immigration attitude
#   ) %>% 
#   pivot_wider(id_cols = lfdn, names_from = wave, 
#               values_from = date_clean:`1130_clean`) %>% 
#   na.omit()
# 
# 
# # define vars
# gles_p_long_nona <- 
#   gles_p_wide_nona %>% 
#   pivot_longer(cols = `date_clean_1`:`1130_clean_8`, 
#                names_to = c(".value", "wave"),
#                names_pattern = "(.*)_(.)"
#   ) %>% 
#   filter(voteint_w1 != "")
# rm(gles_p_wide_nona)
# 
# 
# ## define post-period
# gles_p_long_nona$post <- 
#   gles_p_long_nona$date_clean >= as.Date("2017-02-01")
# 
# ## define treatment: read Bild or not
# gles_p_long_nona$treat <- 
#   gles_p_long_nona$`1661a_bin` == T
# 
# ## define dv
# gles_p_long_nona$dv <- gles_p_long_nona$`1130_clean`
# 
# # prelminary checks
# summary(lm(dv ~ treat*post*voteint_w1, data = gles_p_long_nona))
# 
# gles_p_long_nona %>% 
#   filter(!voteint_w1 %in% c("Other")) %>% 
#   group_by(wave, treat, voteint_w1) %>% 
#   summarise(
#     `1130_clean` = mean(`1130_clean`),
#     date_clean = min(date_clean),
#     n_resps = n()
#   ) %>% 
#   mutate(voteXtreat = paste0(voteint_w1, treat)) %>% 
#   ggplot(aes(x = date_clean, 
#              y = `1130_clean`, 
#              group = voteXtreat, 
#              lty = treat, 
#              col = voteint_w1))+
#   geom_line() + geom_point(aes(size = n_resps, shape = treat))
# 
# table(gles_p_long_nona$voteint_w1, gles_p_long_nona$treat, gles_p_long_nona$post)
#   
# did_party <- function(x, i) {
#   mydata <- do.call("rbind", lapply(i, function(n) subset(gles_p_long_nona, lfdn==x[n])))
#   m <- lm(dv ~ treat * post * voteint_w1,  data = mydata)
#   
#   none_est <- 
#     coefficients(m)["treatTRUE:postTRUE"]
#   
#   low_est <- 
#     coefficients(m)["treatTRUE:postTRUE"] +
#     coefficients(m)["treatTRUE:postTRUE:voteint_w1"]
#   
#   mod_est <- 
#     coefficients(m)["treatTRUE:postTRUE"] +
#     coefficients(m)["treatTRUE:postTRUE:online4-6"]
#   
#   max_est <- 
#     coefficients(m)["treatTRUE:postTRUE"] +
#     coefficients(m)["treatTRUE:postTRUE:online7"]
#   
#   return(c(none_est, low_est, mod_est, max_est))
# }
# 
# b0 <- boot(units, did_online, L, parallel = "snow")
# 
# # define point estimate, upper and lower CI
# df <- data.frame(
#   variable = rep("online sources", 4),
#   group = factor(c("0", "1-3", "4-6", "7"), 
#                  levels = c("0", "1-3", "4-6", "7"), 
#                  ordered = T),
#   estimate = apply(b0$t, 2, mean),
#   lower = apply(b0$t, 2, quantile, probs = 0.025, na.rm = T),
#   upper = apply(b0$t, 2, quantile, probs = 0.975, na.rm = T)
# ) %>% 
#   rbind(df)


# save estimates ####
save(df, file = here(paste0("data/case_did_bootstrapped", Sys.Date(), ".Rdata")))

