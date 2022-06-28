# ______________________________________________
# Bild
# Goal: Estimate risk of stopping to read
# Procedure: load, prepare, risk model
# ______________________________________________
# Date:  Wed Dec 15 14:14:26 2021
# Author: Nicolai Berk
#  R version 4.1.1 (2021-08-10)
# ______________________________________________


# Setup ####
library(tidyverse)
library(dplyr)
library(here)
library(data.table)

# load ####
gles_p_long <- fread(here('data/gles/Panel/long_cleaned.csv'))

# migration trend of bild readers vs non-readers ####
bild_readers_w1 <- 
  gles_p_long %>% 
  filter(`1661a_clean` > 0 & wave == "1") %>% 
  select(lfdn) %>% 
  unlist() %>% 
  unique()

bild_nonreaders_w1 <- 
  gles_p_long %>% 
  filter(`1661a_clean` == 0 & wave == "1") %>% 
  select(lfdn) %>% 
  unlist() %>% 
  unique()

gles_p_long <- 
  gles_p_long %>% 
  mutate(bild_w1 = case_when(lfdn %in% bild_readers_w1 ~ T,
                             lfdn %in% bild_nonreaders_w1 ~ F))

gles_p_long %>% 
  filter(!is.na(bild_w1)) %>% 
  filter(!is.na(`1130_clean`)) %>% 
  filter(!is.na(date_clean)) %>% 
  group_by(wave, bild_w1) %>% 
  summarise(imm_op = mean(`1130_clean`),
            date_clean = max(date_clean)) %>% 
  ggplot(aes(x = date_clean, y = imm_op, col = bild_w1)) +
  geom_line()

# prepare ####

## select only Bild readers ####
bild_dta <- 
  gles_p_long %>% 
  filter(lfdn %in% bild_readers_w1)

# construct dataset
bild_dta <- 
  bild_dta %>%
  mutate(
    stop_read = `1661a_clean` == 0
  )

table(bild_dta$wave, bild_dta$stop_read)

# visualisation


# log reg per wave ####
gles_p_long %>% 
  filter(!is.na(`1661a_bin`) & !is.na(`1130_clean`)) %>% 
  group_by(wave) %>% 
  summarise(est = lm(`1661a_bin` ~ `1130_clean`)$coefficients["`1130_clean`"],
            lower = confint(lm(`1661a_bin` ~ `1130_clean`))[2,1],
            upper = confint(lm(`1661a_bin` ~ `1130_clean`))[2,2],
            p = summary(w1_m)$coefficients["`1130_clean`", "Pr(>|t|)"],
            date = min(date_clean)) %>% 
  ggplot(aes(x = date, y = est,  ymin = lower, ymax = upper)) +
  geom_pointrange() +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = as.Date("2017-02-01"), lty = 2, col = "red") +
  ggtitle("Association of immigration attitude on likelihood to read Bild across survey waves", "Red vertical line indicates Reichelt takeover.")
ggsave(filename = here("paper/vis/imm_bildread_waves.png"))

