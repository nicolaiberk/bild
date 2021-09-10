# ______________________________________________
# Project Title
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

# Step 1 ####

## 2.1 vis dependent ####

## general movement across time (use full data)

## average per paper



## 2.2 vis independents ####

## frames over time

## salience over time

## per paper (use plot from rrpviol here)

## 2.3 vis p-values attitude did ####

attitude_dids$sig_fe <- (attitude_dids$p_value_fe < 0.05)
attitude_dids$sig_lm <- (attitude_dids$p_value_lm < 0.05)

prop.table(table(attitude_dids$sig_fe)) %>% round(2) # 21% instead of 5% -> significant effects overrepresented
prop.table(table(attitude_dids$sig_lm)) %>% round(2) # 12% instead of 5% -> significant effects overrepresented


fe_plot <- attitude_dids %>% 
  ggplot(aes(x = p_value_fe, y = ..count../nrow(attitude_dids))) +
  geom_histogram(bins = 20, col = 'gray', breaks = seq(from = 0, to = 0.95, 0.05)) +
  geom_hline(aes(yintercept = 0.05), col = 'red', lty = 2) +
  ylim(0, 0.25) +
  xlab('Density')+ylab('p')+
  ggtitle(glue("P-values from {nrow(attitude_dids)} fixed-effect DiD-models"), 
          "Treatment = wave, condition = media outlet consumption")

theoryplot <- data.frame(p = runif(1336, 0, 1)) %>% 
  ggplot(aes(x = p, y = ..count../nrow(theoretical_dist))) +
  geom_histogram(bins = 20, col = 'gray', breaks = seq(from = 0, to = 0.95, 0.05)) +
  geom_hline(aes(yintercept = 0.05), col = 'red', lty = 2) +
  ylim(0, 0.25) +
  xlab('Density')+ylab('p')+
  ggtitle(glue("Theoretical distribution of p-values with no effect of media consumption"))

gridExtra::grid.arrange(fe_plot, theoryplot, nrow = 2) %>% 
  ggsave(plot = ., 
         filename = here("paper/vis/DiD_model_ps.png"),
         width = 10, height = 8)

## framing effects
hist(framing_dids$p_value) # not sure why it looks like this - should be normally distributed?

## salience DIDs
hist(salience_dids$p_value) # not sure why it looks like this - should be normally distributed?


## vis effects across specifications ####
load(here('data/efftable.Rdata'))

efftable$frame[efftable$frame == "camps"] <- "camps (+)"
efftable$frame[efftable$frame == "crime"] <- "crime (-)"
efftable$frame[efftable$frame == "capcrime"] <- "capcrime (-)"
efftable$frame[efftable$frame == "salience"] <- "salience (-)"
efftable$frame[efftable$frame == "medit"] <- "medit (+)"
efftable$frame[efftable$frame == "labmar"] <- "labmar (+)"
efftable$frame[efftable$frame == "refnums"] <- "refnums (-)"
efftable$frame[efftable$frame == "labmar"] <- "labmar (+)"
efftable$frame[efftable$frame == "medit"] <- "medit (+)"
efftable$frame[efftable$frame == "deport"] <- "deport (+)"

efftable %>% 
  ggplot(mapping = aes(x = t_value, y = frame, col = iv)) +
  geom_point() +
  geom_vline(aes(xintercept = 0), col = "red", lty = 2) +
  facet_grid(dv_lag ~ dv)
ggsave(filename = here("paper/vis/effectplot.png"))
