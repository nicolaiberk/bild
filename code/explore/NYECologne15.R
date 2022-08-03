# ______________________________________________
# Cologne New Years RDD
# Goal: Assess Change in Migration Attitude Following Cologne
# Procedure: load data, clean, plot
# Conclusion: THIS survey data not telling
# ______________________________________________
# Date:  Wed Aug 03 17:30:16 2022
# Author: Nicolai Berk
#  R version 4.1.1 (2021-08-10)
# ______________________________________________


# Setup ####
library(tidyverse)
library(dplyr)
library(here)
library(data.table)

# load data ####
gles_ltp <- 
  haven::read_dta(here("data/raw/gles/LongTermPanel/ZA5770_v1-0-0.dta")) %>% 
  pivot_longer(
    cols = jdatum:nwkrnr3,
    names_to = c("Erhebung", ".value"),
    names_pattern = "(.)(.*)"
  )

# clean ####
gles_ltp <- 
  gles_ltp %>% 
  filter(Erhebung == "l") %>%
  mutate(
    `285a_clean` = ifelse(`285a` < 0, NA, `285a` - 1),
    `174b_clean` = ifelse(`174b` < 0, NA, `174b` - 6),
    date_clean = as.Date(datetime,
                              format = "%Y-%m-%d %H:%M:%S")
    )

# plot ####
gles_ltp %>% 
  group_by(date_clean) %>% 
  summarise(mig_att = mean(`174b_clean`, na.rm = T),
            N = n()) %>% 
  ggplot(aes(date_clean, mig_att, size = N)) +
  geom_point()


table(gles_ltp$date_clean) ## only three obs post-treatment
