
# ______________________________________________
# Media effects on issue definitions
# Goal: Preprocess survey data
# Procedure: load, pivot, clean
# ______________________________________________
# Date:  Mon Sep 13 08:17:34 2021
# Author: Nicolai Berk
#  R version 4.0.3 (2020-10-10)
# ______________________________________________

# 0. load ####

library(here)
library(tidyverse)
library(data.table)
library(haven)
library(lubridate)
library(glue)
library(naniar)
library(dplyr)


# load raw data ####
rm(list = ls())
gles_p_long <- fread(file = here(paste0('data/raw/gles/Panel/long.csv'))) %>% as.data.frame()

# define parameters ####
media_vars <- 
  gles_p_long %>% 
  select(contains('1661'), contains('1681'), contains("1600"), contains("1610")) %>% 
  select(!contains('1661h')) %>% 
  select(!contains('1681f')) %>%  
  colnames()



newsm_vars <- gles_p_long %>% select(contains('1701')) %>% colnames()
non_vars   <- gles_p_long %>% select(contains('1661h'), contains('1681f')) %>%  colnames()
# issue_vars <- gles_p_long %>% select(contains('2880'), "1500", "1090", "1130", 
#                                      "1290", "1210", "1140", "1220", "1300", 
#                                      "1411", "1250", "1260") %>% colnames()

main_news_vars <- gles_p_long %>% select(contains('1621')) %>% colnames()
web_politics_vars <- gles_p_long %>% select(contains('1600')) %>% colnames()
sm_politics_vars <- gles_p_long %>% select(contains('1610')) %>% colnames()
big5_vars <- gles_p_long %>% select(contains('2180')) %>% colnames()
schwartz_vars <- gles_p_long %>% select(contains('3320')) %>% colnames()




# clean variables ####

## date  ####
gles_p_long$date_clean <- gles_p_long$datetime
gles_p_long$date_clean[gles_p_long$date_clean == ''] <- NA
gles_p_long$date_clean[gles_p_long$date_clean == '-95'] <- NA
gles_p_long$date_clean[gles_p_long$date_clean == '-95 nicht teilgenommen'] <- NA
gles_p_long$date_clean <- as.Date(gles_p_long$date_clean)


## sociodemographics ####
gles_p_long <- 
  gles_p_long %>% 
  filter(wave == "x") %>% 
  mutate(gender_clean = `2280`) %>% 
  select(gender_clean, lfdn) %>% 
  right_join(gles_p_long, by = "lfdn")
  
  
gles_p_long <- 
  gles_p_long %>% 
  filter(wave == "x") %>% 
  mutate(`2290s` = ifelse(`2290s` == "1955 und frueher",
                          "1955", `2290s`)) %>% 
  mutate(ybirth_clean = as.numeric(`2290s`)) %>% 
  select(ybirth_clean, lfdn) %>% 
  right_join(gles_p_long, by = "lfdn")

gles_p_long <- 
  gles_p_long %>% 
  filter(wave != "x")



## media vars  ####

### non-readers #### 
gles_p_long$`1661h_bin` <- NA
gles_p_long$`1661h_bin`[(!is.na(gles_p_long$`1661h` > 0)) & 
                          (gles_p_long$`1661h` > 0)] <- 
  gles_p_long$`1661h`[(!is.na(gles_p_long$`1661h` > 0)) & 
                        (gles_p_long$`1661h` > 0)] == 1



### all other newspaper and tv ####
for (varname in media_vars){
  varname_clean <- paste0(varname, '_clean')
  varname_bin <- paste0(varname, '_bin')
  gles_p_long[,varname_clean] <- gles_p_long[varname] - 1
  gles_p_long[(gles_p_long[,varname_clean] < 0 | is.na(gles_p_long[,varname_clean])), varname_clean] <- NA
  gles_p_long[,varname_bin] <- ifelse(!is.na(gles_p_long[,varname]), gles_p_long[,varname] > 1, NA)
}

for (varname in c(newsm_vars, non_vars, main_news_vars)){
  varname_clean <- paste0(varname, '_clean')
  gles_p_long[,varname_clean] <- gles_p_long[,varname] == 1
  gles_p_long[(gles_p_long[,varname] < 0 | is.na(gles_p_long[,varname])), varname_clean] <- NA
}




### spiegel online ####
gles_p_long[,'1702_clean'] <- gles_p_long[,'1702'] - 1
gles_p_long[gles_p_long$`1702`< 0 & !is.na(gles_p_long$`1702`),'1702_clean'] <- NA





## ideology ####
gles_p_long[,'1500_clean'] <- NA
gles_p_long[gles_p_long$`1500` > 0 & !is.na(gles_p_long$`1500`), '1500_clean'] <- 
  gles_p_long[gles_p_long$`1500` > 0 & !is.na(gles_p_long$`1500`),'1500'] - 1


## party preference ####
gles_p_long[,'190b_inter'] <- gles_p_long[,'190ba']

gles_p_long[,'190b_clean'] <- NA_character_
gles_p_long[gles_p_long$`190b_inter` == 1  & 
              !is.na(gles_p_long$`190b_inter`),'190b_clean'] <- 'CDU/CSU'
gles_p_long[gles_p_long$`190b_inter` == 4  & 
              !is.na(gles_p_long$`190b_inter`),'190b_clean'] <- 'SPD'
gles_p_long[gles_p_long$`190b_inter` == 5  & 
              !is.na(gles_p_long$`190b_inter`),'190b_clean'] <- 'FDP'
gles_p_long[gles_p_long$`190b_inter` == 6  & 
              !is.na(gles_p_long$`190b_inter`),'190b_clean'] <- 'Grüne'
gles_p_long[gles_p_long$`190b_inter` == 7  & 
              !is.na(gles_p_long$`190b_inter`),'190b_clean'] <- 'Linke'
gles_p_long[gles_p_long$`190b_inter` == 322  & 
              !is.na(gles_p_long$`190b_inter`),'190b_clean'] <- 'AfD'
gles_p_long[gles_p_long$`190b_inter` == 801  & 
              !is.na(gles_p_long$`190b_inter`),'190b_clean'] <- 'Other'
gles_p_long[gles_p_long$`190b_inter` == -93  & 
              !is.na(gles_p_long$`190b_inter`),'190b_clean'] <- 'Stopped interview'
gles_p_long[gles_p_long$`190b_inter` == -95  & 
              !is.na(gles_p_long$`190b_inter`),'190b_clean'] <- 'Did not participate'
gles_p_long[gles_p_long$`190b_inter` == -97  & 
              !is.na(gles_p_long$`190b_inter`),'190b_clean'] <- 'Not applicable'
gles_p_long[gles_p_long$`190b_inter` == -98  & 
              !is.na(gles_p_long$`190b_inter`),'190b_clean'] <- "Don't know"
gles_p_long[gles_p_long$`190b_inter` == -99  & 
              !is.na(gles_p_long$`190b_inter`),'190b_clean'] <- 'Did not answer'


## party scalometer, MIP & SMIP
gles_p_long <- 
  gles_p_long %>% 
  mutate(`430a_clean` = ifelse(`430a` > 0, `430a` - 6, NA),
         `430b_clean` = ifelse(`430b` > 0, `430b` - 6, NA),
         `430c_clean` = ifelse(`430c` > 0, `430c` - 6, NA),
         `430d_clean` = ifelse(`430d` > 0, `430d` - 6, NA),
         `430e_clean` = ifelse(`430e` > 0, `430e` - 6, NA),
         `430f_clean` = ifelse(`430f` > 0, `430f` - 6, NA),
         `430i_clean` = ifelse(`430i` > 0, `430i` - 6, NA),
         `840_c1_clean` = ifelse(`840_c1` > 0, `840_c1`, NA),
         `840_c2_clean` = ifelse(`840_c2` > 0, `840_c2`, NA),
         `840_c3_clean` = ifelse(`840_c3` > 0, `840_c3`, NA),
         `840_c4_clean` = ifelse(`840_c4` > 0, `840_c4`, NA),
         `840_c5_clean` = ifelse(`840_c5` > 0, `840_c5`, NA)
         # `860_clean` = ifelse(`860` > 0, `860`, NA)
         ) %>% 
  mutate(mip_mig_crime = 
           ifelse(
             is.na(`840_c1_clean`),
             NA,
             `840_c1_clean` %in% c(3411) |
             `840_c2_clean` %in% c(3411) |
             `840_c3_clean` %in% c(3411) |
             `840_c4_clean` %in% c(3411) |
             `840_c5_clean` %in% c(3411)),
         mip_mig = 
           ifelse(
             is.na(`840_c1_clean`),
             NA,
             `840_c1_clean` %in% c(3411:3413, 3750:3759) |
             `840_c2_clean` %in% c(3411:3413, 3750:3759) |
             `840_c3_clean` %in% c(3411:3413, 3750:3759) |
             `840_c4_clean` %in% c(3411:3413, 3750:3759) |
             `840_c5_clean` %in% c(3411:3413, 3750:3759)
           ))

## issues [2880** coding 1-5], [1130, 1290, 1210, 1411, 1250, 1090 coding 1-7]; 1140, 1220, 1300, 1-5 (issue importance)

### ego positions
ego_vars <- gles_p_long %>% select(contains("2880")) %>% colnames()
ego_vars <- 
  c(
    ego_vars,
    gles_p_long %>% select(contains("1555")) %>% colnames()
    )


for (varname in ego_vars){
  varname_clean <- paste0(varname, '_clean')
  gles_p_long[,varname_clean] <- gles_p_long[, varname]
  gles_p_long[is.na(gles_p_long[,varname]), varname_clean] <- -1000
  gles_p_long[gles_p_long[,varname_clean] < 0, varname_clean] <- NA
  gles_p_long[,varname_clean] <- gles_p_long[,varname_clean] - 3
}

### general positions
issue_vars <- c(1130, 1290, 1210, 1411, 1250, 1090, 1260)

for (varname in issue_vars){
  varname <- as.character(varname)
  varname_clean <- paste0(varname, '_clean')
  gles_p_long[,varname_clean] <- gles_p_long[, varname]
  gles_p_long[is.na(gles_p_long[,varname]), varname_clean] <- -1000
  gles_p_long[gles_p_long[,varname_clean] < 0, varname_clean] <- NA
  gles_p_long[,varname_clean] <- gles_p_long[,varname_clean] - 4
  
  # generate missing indicators
  
  ## stopped interview
  varname_stop <- paste0(varname, '_stop')
  gles_p_long[,varname_stop] <- gles_p_long[, varname] == -93
  
  ## did not participate
  varname_nonpart <- paste0(varname, '_nonpart')
  gles_p_long[,varname_nonpart] <- gles_p_long[, varname] == -95
  
  ## did not answer question
  varname_dodge <- paste0(varname, '_dodge')
  gles_p_long[,varname_dodge] <- gles_p_long[, varname] == -99
  
}




salience_vars <- c(1140, 1220, 1300)

for (varname in salience_vars){
  varname <- as.character(varname)
  varname_clean <- paste0(varname, '_clean')
  gles_p_long[,varname_clean] <- gles_p_long[, varname]
  gles_p_long[is.na(gles_p_long[,varname]), varname_clean] <- -1000
  gles_p_long[gles_p_long[,varname_clean] < 0, varname_clean] <- NA
  gles_p_long[,varname_clean] <- gles_p_long[,varname_clean]*(-1) + 3
  
  # generate missing indicators
  
  ## stopped interview
  varname_stop <- paste0(varname, '_stop')
  gles_p_long[,varname_stop] <- gles_p_long[, varname] == -93
  
  ## did not participate
  varname_nonpart <- paste0(varname, '_nonpart')
  gles_p_long[,varname_nonpart] <- gles_p_long[, varname] == -95
  
  ## did not answer question
  varname_dodge <- paste0(varname, '_dodge')
  gles_p_long[,varname_dodge] <- gles_p_long[, varname] == -99
}

for (varname in big5_vars){
  varname <- as.character(varname)
  varname_clean <- paste0(varname, '_clean')
  gles_p_long[,varname_clean] <- gles_p_long[, varname]
  gles_p_long[is.na(gles_p_long[,varname]), varname_clean] <- -1000
  gles_p_long[gles_p_long[,varname_clean] < 0, varname_clean] <- NA
  gles_p_long[,varname_clean] <- gles_p_long[,varname_clean] - 3
}

for (varname in schwartz_vars){
  varname <- as.character(varname)
  varname_clean <- paste0(varname, '_clean')
  gles_p_long[,varname_clean] <- gles_p_long[, varname]
  gles_p_long[is.na(gles_p_long[,varname]), varname_clean] <- -1000
  gles_p_long[gles_p_long[,varname_clean] < 0, varname_clean] <- NA
  gles_p_long[,varname_clean] <- gles_p_long[,varname_clean]
}


## attentiveness

### politisches interesse
gles_p_long <- 
  gles_p_long %>% 
  mutate(`010_clean` = ifelse(`010` > 0, (`010` * (-1)) + 6, NA))

### 5% Hürde
gles_p_long$`090_v1_clean` <-  gles_p_long$`090_v1` == 1
gles_p_long$`090_v1_clean`[gles_p_long$`090_v1` < 0 & !(gles_p_long$`090_v1` == -98)] <- NA

### Bundestagsverteilung
gles_p_long$`110_clean` <- gles_p_long$`110` == 2
gles_p_long$`110_clean`[gles_p_long$`110` < 0 & !(gles_p_long$`110` == -98)] <- NA

### Bundeskanzlerwahl
gles_p_long$`130_clean` <- gles_p_long$`130` == 3
gles_p_long$`130_clean`[gles_p_long$`130` < 0 & !(gles_p_long$`130` == -98)] <- NA


### Frauke Petry face
gles_p_long$`3430q_clean` <- gles_p_long$`3430q` == 322
gles_p_long$`3430q_clean`[gles_p_long$`3430q` < 0 & !(gles_p_long$`3430q` == -98)] <- NA

### Horst Seehofer
gles_p_long$`3430c_clean` <- gles_p_long$`3430c` == 3
gles_p_long$`3430c_clean`[gles_p_long$`3430c` < 0 & !(gles_p_long$`3430c` == -98)] <- NA

### Katrin Göring-Eckhardt
gles_p_long$`3430l_clean` <- gles_p_long$`3430l` == 6
gles_p_long$`3430l_clean`[gles_p_long$`3430l` < 0 & !(gles_p_long$`3430l` == -98)] <- NA

### Christian Lindner
gles_p_long$`3430p_clean` <- gles_p_long$`3430p` == 5
gles_p_long$`3430p_clean`[gles_p_long$`3430p` < 0 & !(gles_p_long$`3430p` == -98)] <- NA

### Sahra Wagenknecht
gles_p_long$`3430m_clean` <- gles_p_long$`3430m` == 7
gles_p_long$`3430m_clean`[gles_p_long$`3430m` < 0 & !(gles_p_long$`3430m` == -98)] <- NA

### Martin Schulz
gles_p_long$`3430r_clean` <- gles_p_long$`3430r` == 4
gles_p_long$`3430r_clean`[gles_p_long$`3430r` < 0 & !(gles_p_long$`3430r` == -98)] <- NA

### Sigmar Gabriel
gles_p_long$`3430j_clean` <- gles_p_long$`3430j` == 4
gles_p_long$`3430j_clean`[gles_p_long$`3430j` < 0 & !(gles_p_long$`3430j` == -98)] <- NA

### Angela Merkel
gles_p_long$`3430a_clean` <- gles_p_long$`3430a` == 2
gles_p_long$`3430a_clean`[gles_p_long$`3430a` < 0 & !(gles_p_long$`3430a` == -98)] <- NA

### Winfried Kretschmann
gles_p_long$`3430s_clean` <- gles_p_long$`3430s` == 6
gles_p_long$`3430s_clean`[gles_p_long$`3430s` < 0 & !(gles_p_long$`3430s` == -98)] <- NA

### Wolfgang Schäuble
gles_p_long$`3430t_clean` <- gles_p_long$`3430t` == 2
gles_p_long$`3430t_clean`[gles_p_long$`3430t` < 0 & !(gles_p_long$`3430t` == -98)] <- NA

### Katja Kipping
gles_p_long$`3430u_clean` <- gles_p_long$`3430u` == 7
gles_p_long$`3430u_clean`[gles_p_long$`3430u` < 0 & !(gles_p_long$`3430u` == -98)] <- NA

### Bernd Höcke
gles_p_long$`3430v_clean` <- gles_p_long$`3430v` == 322
gles_p_long$`3430v_clean`[gles_p_long$`3430v` < 0 & !(gles_p_long$`3430v` == -98)] <- NA


## subset
gles_p_long <- gles_p_long %>% 
  select(lfdn, wave, ends_with('_clean'), ends_with('_bin'), 
         contains("1701"), contains('mip'), contains("participation"),
         contains("stop"), contains("dodge"), contains("nonpart"), contains("mip"))


## assign to single newspaper (random according to days read)
col_names <- 
  gles_p_long %>% 
  select(contains("1661") &
           contains("clean") &
           !contains("h")) %>% 
  colnames()
  
ra_wtd <- function(row){
  sample(col_names,
         size = 1, 
         prob = row/sum(row, na.rm = T)) %>% 
    return()
}

subset_read <- 
  gles_p_long %>% 
  select(contains("1661") &
           contains("clean") &
           !contains("h")) %>% 
  mutate(readership_var = 
           case_when(rowSums(.) == 0 ~ "None",
                     is.na(rowSums(.)) ~ "Missing"))

subset_read[is.na(subset_read$readership_var), "readership_var"] <- 
  subset_read %>% 
  filter(is.na(readership_var)) %>% 
  select(-readership_var) %>% 
  apply(., 1, ra_wtd)

subset_read$readership <- 
  case_when(
    subset_read$readership_var == "1661a_clean" ~ "Bild",
    subset_read$readership_var == "1661b_clean" ~ "FR",
    subset_read$readership_var == "1661c_clean" ~ "FAZ",
    subset_read$readership_var == "1661d_clean" ~ "SZ",
    subset_read$readership_var == "1661e_clean" ~ "taz",
    subset_read$readership_var == "1661f_clean" ~ "Welt",
    subset_read$readership_var == "1661g_clean" ~ "Other",
    subset_read$readership_var == "None"        ~ "None",
    subset_read$readership_var == "Missing"     ~ NA_character_
    )

gles_p_long$readership <- subset_read$readership
gles_p_long$readership_var <- subset_read$readership_var

rm(subset_read)

## clean magazine vars
for (i in letters[1:4]){
  gles_p_long[glue("1701{i}_clean")] <- !gles_p_long[[glue("1701{i}c")]]
  gles_p_long[is.na(gles_p_long[glue("1701{i}c_clean")]), glue("1701{i}_clean")] <- NA
  
}


## total number of news magazines or newspapers consumed
gles_p_long$print_sources_n <- 
  gles_p_long %>% 
  select(contains("1661"), `1701a_clean`:`1701d_clean`) %>% 
  select(contains("bin"), `1701a_clean`:`1701d_clean`, -"1661h_bin") %>% 
  rowSums()

## total number of news sources (incl tv consumed)
gles_p_long$news_sources_n <- 
  gles_p_long %>% 
  select(contains("1661"), `1701a_clean`:`1701d_clean`, contains("1681")) %>% 
  select(contains("bin"), -contains("flag"), -"1661h_bin", `1701a_clean`:`1701d_clean`) %>%
  rowSums()

## initial party vote
gles_p_long <- 
  gles_p_long %>% 
  filter(wave == 1) %>% 
  mutate(voteint_w1 = `190b_clean`) %>% 
  select(voteint_w1, lfdn) %>% 
  right_join(gles_p_long, by = "lfdn")

## AfD vote
missing_list <- 
  c('Stopped interview', 'Did not participate', 'Not applicable', 
    "Don't know", 'Did not answer')

gles_p_long <- 
  gles_p_long %>%  
  mutate(afd_vote = `190b_clean` == "AfD")
gles_p_long$afd_vote[gles_p_long$`190b_clean` %in% missing_list] <- NA

## missing vote
gles_p_long <- 
  gles_p_long %>%  
  mutate(vote_miss = `190b_clean` == "Don't know" | (`190b_clean` == "Did not answer"))


## political knowledge
gles_p_long$polknowl <- 
  gles_p_long %>% 
  select((contains("3430") & contains("clean")), `110_clean`, `090_v1_clean`, `130_clean`) %>% 
  mutate(polknowl = rowSums(., na.rm = T)/(ncol(.)- rowSums(is.na(.)))) %>% 
  select(polknowl) %>% 
  unlist()

## political knowledge and schwartz-values
gles_p_long <- 
  gles_p_long %>% 
  group_by(lfdn) %>% 
  summarise(polknowl_avg = mean(polknowl, na.rm = T)) %>% 
  right_join(gles_p_long, by = "lfdn") %>% 
  mutate(security = `3320e_clean` + `3320m_clean`,
         conformity = `3320n_clean` + `3320q_clean`)


## schwartz-values and polknowl initially
gles_p_long <-
  gles_p_long %>% 
  filter(!is.na(polknowl)) %>% 
  select(lfdn, date_clean, polknowl) %>% 
  arrange(lfdn, date_clean) %>% 
  group_by(lfdn) %>% 
  mutate(polknowl_init = polknowl) %>% 
  slice(1) %>% 
  select(polknowl_init, lfdn) %>% 
  right_join(gles_p_long, by = "lfdn")

gles_p_long <-
  gles_p_long %>% 
  filter(!is.na(conformity)) %>% 
  select(lfdn, date_clean, conformity) %>% 
  arrange(lfdn, date_clean) %>% 
  group_by(lfdn) %>% 
  mutate(conformity_init = conformity) %>% 
  slice(1) %>% 
  select(conformity_init, lfdn) %>% 
  right_join(gles_p_long, by = "lfdn")

gles_p_long <-
  gles_p_long %>% 
  filter(!is.na(security)) %>% 
  select(lfdn, date_clean, security) %>% 
  arrange(lfdn, date_clean) %>% 
  group_by(lfdn) %>% 
  mutate(security_init = security) %>% 
  slice(1) %>% 
  select(security_init, lfdn) %>% 
  right_join(gles_p_long, by = "lfdn")


## Add treatment groups and initial attitudes for estimation
gles_p_long <- 
  gles_p_long %>% 
  select(lfdn, date_clean, `1130_clean`, wave) %>% 
  filter(!is.na(`1130_clean`), wave == "1") %>% 
  mutate(init_mig = `1130_clean`) %>% 
  select(lfdn, init_mig) %>% 
  right_join(gles_p_long, by = "lfdn")

gles_p_long <- 
  gles_p_long %>% 
  select(lfdn, date_clean, `1130_clean`, wave) %>% 
  filter(!is.na(`1130_clean`), wave == "8") %>% 
  mutate(init_mig_placebo = `1130_clean`) %>% 
  select(lfdn, init_mig_placebo) %>% 
  right_join(gles_p_long, by = "lfdn")

gles_p_long <- 
  gles_p_long %>%
  filter(wave == "1") %>% 
  mutate(init_mig_groups = cut(`1130_clean`, 
                            c(-Inf, -0.5, 0.5, Inf), 
                            c("Liberal", "Neutral", "Conservative"))) %>% 
  select(lfdn, init_mig_groups) %>% 
  right_join(gles_p_long, by = "lfdn")

gles_p_long <- 
  gles_p_long %>%
  filter(wave == "8") %>% 
  mutate(init_mig_groups_placebo = cut(`1130_clean`, 
                               c(-Inf, -0.5, 0.5, Inf), 
                               c("Liberal", "Neutral", "Conservative"))) %>% 
  select(lfdn, init_mig_groups_placebo) %>% 
  right_join(gles_p_long, by = "lfdn")

gles_p_long <- 
  gles_p_long %>% 
  filter(wave == "1") %>% 
  mutate(bild_init = ifelse(is.na(`1661a_clean`), NA, `1661a_clean` > 0)) %>% 
  select(bild_init, lfdn) %>% 
  right_join(gles_p_long, by = "lfdn")

gles_p_long <- 
  gles_p_long %>% 
  filter(wave == "8") %>% 
  mutate(bild_init_placebo = ifelse(is.na(`1661a_clean`), NA, `1661a_clean` > 0)) %>% 
  select(bild_init_placebo, lfdn) %>% 
  right_join(gles_p_long, by = "lfdn")

gles_p_long <- 
  gles_p_long %>% 
  filter(wave == "1") %>% 
  mutate(faz_init = ifelse(is.na(`1661c_clean`), NA, `1661c_clean` > 0)) %>% 
  select(faz_init, lfdn) %>% 
  right_join(gles_p_long, by = "lfdn")

gles_p_long <- 
  gles_p_long %>% 
  filter(wave == "1") %>% 
  mutate(welt_init = ifelse(is.na(`1661f_clean`), NA, `1661f_clean` > 0)) %>% 
  select(welt_init, lfdn) %>% 
  right_join(gles_p_long, by = "lfdn")

# add mean accuracy motivation
gles_p_long <- 
  gles_p_long %>% 
  select(lfdn, `1555d_clean`) %>% 
  group_by(lfdn) %>% 
  summarise(acc_goal = mean(`1555d_clean`, na.rm = T)) %>% 
  right_join(gles_p_long, by = "lfdn")


# check if ever read Bild
gles_p_long <- 
  gles_p_long %>% 
  select(lfdn, wave, `1661a_clean`) %>% 
  group_by(lfdn) %>% 
  summarise(bild_ever = sum(`1661a_clean`, na.rm = T)) %>% 
  filter(!is.na(bild_ever)) %>% 
  right_join(gles_p_long, by = "lfdn")

# recode treatment to NA if not never read bild
gles_p_long <- 
  gles_p_long %>% 
  mutate(treat = ifelse(bild_init, T, ifelse(bild_ever == 0, F, NA)))

# placebo treatment
gles_p_long <- 
  gles_p_long %>% 
  mutate(treat_placebo = ifelse(bild_init_placebo, T, ifelse(bild_ever == 0, F, NA)))

# placebo treatment, not pre-treated
gles_p_long <- 
  gles_p_long %>% 
  mutate(treat_placebo_np = ifelse(bild_init_placebo, ifelse(bild_init, F, T), ifelse(bild_ever == 0, F, NA)))



# define post-treatment date
gles_p_long$post <- gles_p_long$date_clean>= as.Date("2017-02-01")

# define post-treatment date - placebo
gles_p_long$post_placebo <- gles_p_long$date_clean>= as.Date("2018-01-01")

## save
gles_p_long %>% 
  fwrite(., here('data/raw/gles/Panel/long_cleaned.csv'))
