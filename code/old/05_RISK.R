# ______________________________________________
# Effect of framing on issue attitudes
# Goal: Estimate risk-models of stopping to consume newspaper
# Procedure: load data, prepare, estimate
# ______________________________________________
# Date:  Wed Jan 12 11:12:28 2022
# Author: Nicolai Berk
#  R version 4.1.1 (2021-08-10)
# ______________________________________________


# Setup ####
library(tidyverse)
library(dplyr)
library(here)
library(data.table)
library(ggpubr)
library(survival)
library(survminer)
library(fixest)

# load data ####
gles_p_long <- fread(here('data/gles/Panel/long_cleaned.csv'))


# prepare ####

## select bild readers from first wave
bild_readers_w1 <- 
  gles_p_long %>% 
  filter(wave == 1 & `1661a_clean` > 0) %>% 
  select(lfdn) %>% 
  unlist()

risk_set <- 
  gles_p_long %>% 
  filter(lfdn %in% bild_readers_w1) 

## define immigration attitude and afd support based on first wave
risk_set <- 
  risk_set %>% 
  filter(wave == 1) %>% 
  mutate(imm_att = cut(`1130_clean`, breaks = c(-Inf, -1.5, 1.5, Inf), labels = c("liberal", "moderate", "conservative")),
         afd_att = cut(`430i_clean`, breaks = c(-Inf, -1.5, 1.5, Inf), labels = c("negative", "ambiguous", "positive"))) %>% 
  select(imm_att, afd_att, lfdn) %>% 
  right_join(risk_set, by = "lfdn")
  

# estimate ####

## vis declines in readership
readers_imm <- 
  risk_set %>% 
  group_by(imm_att, wave) %>% 
  filter(!is.na(imm_att), !is.na(`1661a_clean`)) %>% 
  summarise(bild_reader = mean(`1661a_clean` > 0, na.rm = T),
            date_clean = mean(date_clean)) %>% 
  ggplot(aes(x = date_clean, y = bild_reader, col = imm_att, lty = imm_att)) +
  geom_line() + geom_point() +
  ylim(c(0,1)) + ylab("Reader share") + xlab("Wave") +
  ggtitle("Share of Bild readers from first wave reporting reading Bild, by initial immigration attitude") +
  theme_minimal()

readers_afd <- 
  risk_set %>% 
  group_by(afd_att, wave) %>% 
  filter(!is.na(afd_att), !is.na(`1661a_clean`)) %>% 
  summarise(bild_reader = mean(`1661a_clean` > 0, na.rm = T),
            date_clean = mean(date_clean)) %>% 
  ggplot(aes(x = date_clean, y = bild_reader, col = afd_att, lty = afd_att)) +
  geom_line() + geom_point() +
  ylim(c(0,1)) + ylab("Reader share") + xlab("Wave") +
  ggtitle("Share of Bild readers from first wave reporting reading Bild, by initial AfD support") +
  theme_minimal()

ggarrange(readers_imm, readers_afd, nrow = 2) %>% 
  ggsave(filename = here("paper/vis/reader_share_afdatt.png"), width = 10, height = 8)


## risk model: stopping to read
risk_set <- 
  risk_set %>% 
  filter(!is.na(date_clean))

# define event: stopping to read
risk_set$event <- NA
risk_set$event[risk_set$`1661a_clean` == 0] <- 1

# define time: wave at which stopping to read
risk_set <- risk_set %>% 
  mutate(surv_wave = ifelse(event == 1, as.numeric(wave), -1))


survival_imm <- survfit(Surv(risk_set$surv_wave, risk_set$event) ~ risk_set$imm_att) 
summary(survival_imm)
ggsurvplot(survival_imm, data = risk_set, conf.int = T) +
  ggtitle("Risk of stopping to read by immigration attitude") +
  xlab("Wave")


survival_afd <- survfit(Surv(as.numeric(risk_set$surv_wave), risk_set$event) ~ risk_set$afd_att) 
summary(survival_afd)
ggsurvplot(survival_afd, data = risk_set, conf.int = T) +
  ggtitle("Risk of stopping to read by attitude towards the AfD") +
  xlab("Wave")


## lm predicting stopping to read
risk_set$event <- risk_set$`1661a_clean` == 0

summary(lm(event ~ imm_att, data = risk_set))
summary(lm(event ~ imm_att + wave, data = risk_set))
summary(lm(event ~ imm_att * wave, data = risk_set))



# Both models show insignificantly higher risk to stop reading for *conservative* readers
# -> no predictor!
# (but no ceiling effects either)


# lm(bildreader ~ imm_att) per wave ####

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