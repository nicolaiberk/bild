# ______________________________________________
# Bild
# Goal: transform df wide to long
# Procedure: load, transform, save
# ______________________________________________
# Date:  Sun May 15 11:03:55 2022
# Author: Nicolai Berk
#  R version 4.1.1 (2021-08-10)
# ______________________________________________


# Setup ####

# 0. load ####

library(here)
library(tidyverse)
library(data.table)
library(haven)
library(lubridate)
library(glue)
library(naniar)
library(dplyr)

gles_p <- read_dta(here('data/raw/gles/Panel/GLESMerge/ZA6838_allwaves_sA_v4-0-0.dta'))

## id var
gles_p$lfdn <- as.factor(gles_p$lfdn)



# 1. Pivot wide -> long ####

## using single command
gles_p_long <- 
  gles_p  %>% 
  select(lfdn, # id
         contains("datetime"), contains("participation"),
         contains("_010"), contains("_020"), # political interest & satisfaction w. democracy
         contains("_2280"), contains("_2290"), # gender, age
         contains("190"), contains("430"), # vote intention, party scalometer
         contains("840"), contains("860"), # MIP
         contains("1555"), # Political Motivation
         # issue attitudes
         contains("1500"), contains("1090"), contains("1300"), contains("1210"),
         contains("2880"), contains("1130"), contains("1290"), contains("1411"), 
         contains("1210"), contains("1140"), contains("1220"), contains("1250"),
         contains("1260"), 
         contains("3320"), # schwartz values
         contains('_090_v1'), contains('_110'), contains('_130'), contains('3430'), # political knowledge
         # media use
         contains("1621"), contains("1600"), contains("1610"), contains("1681"),
         contains("1661"), contains("1701"), contains("1702"), contains("2180")) %>%
  select(!contains("flag")) %>%  
  pivot_longer(cols = kp1_datetime:kp8_1702, 
               names_to = c("wave", ".value"),
               names_pattern = "kp(..?)_(.*)"
  )

### check
# vis_miss(gles_p_long[sample(1:nrow(gles_p_long), 1000),])
# table(gles_p_long$wave)
# plot(as.Date(gles_p_long$datetime), gles_p_long$wave)

## save
fwrite(gles_p_long, file = here(paste0('data/raw/gles/Panel/long.csv')))

