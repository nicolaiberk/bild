
library(here)
library(dplyr)
library(tidyverse)
library(data.table)
library(haven)
library(lubridate)
library(glue)
library(naniar)

# load raw data
gles_p_long <- fread(file = here('data/gles/Panel/long.csv')) %>% as.data.frame()

# define parameters
media_vars <- gles_p_long %>% select(contains('1661'), contains('1681')) %>% colnames()
newsm_vars <- gles_p_long %>% select(contains('1701')) %>% colnames()
issue_vars <- gles_p_long %>% select(contains('2880'), "1500", "1090", "1130", 
                                     "1290", "1210", "1140", "1220", "1300", 
                                     "1411", "1250", "1260") %>% colnames()


# clean variables

## date
gles_p_long$date_clean <- gles_p_long$date_new
gles_p_long$date_clean[gles_p_long$date_clean == ''] <- NA
gles_p_long$date_clean[gles_p_long$date_clean == '-95'] <- NA
gles_p_long$date_clean[gles_p_long$date_clean == '-95 nicht teilgenommen'] <- NA
gles_p_long$date_clean <- as.Date(gles_p_long$date_clean)

## media vars
for (varname in media_vars){
  varname_clean <- paste0(varname, '_clean')
  varname_bin <- paste0(varname, '_bin')
  gles_p_long[,varname_clean] <- gles_p_long[varname] - 1
  gles_p_long[(gles_p_long[,varname_clean] < 0 | is.na(gles_p_long[,varname_clean])), varname_clean] <- NA
  gles_p_long[,varname_bin] <- (gles_p_long[,varname] > 1)
}

for (varname in newsm_vars){
  varname_clean <- paste0(varname, '_clean')
  gles_p_long[,varname_clean] <- gles_p_long[,varname] == 1
}



## spiegel online - how many days last week
gles_p_long[,'1702_clean'] <- gles_p_long[,'1702'] - 1
gles_p_long[gles_p_long$`1702`< 0 & !is.na(gles_p_long$`1702`),'1702_clean'] <- NA





## issues [2880** coding 1-5], [1130, 1290, 1210, 1411, 1250 coding 1-7], [1140, 1220, 1300, 1-5 (issue importance), ignored for now]

### ego positions
ego_vars <- gles_p_long %>% select(contains("2880")) %>% colnames()

for (varname in ego_vars){
  varname_clean <- paste0(varname, '_clean')
  gles_p_long[,varname_clean] <- gles_p_long[, varname]
  gles_p_long[is.na(gles_p_long[,varname]), varname_clean] <- -1000
  gles_p_long[gles_p_long[,varname_clean] < 0, varname_clean] <- NA
  gles_p_long[,varname_clean] <- gles_p_long[,varname_clean] - 3
}

### general positions
issue_vars <- c(1130, 1290, 1210, 1411, 1250)

for (varname in issue_vars){
  varname <- as.character(varname)
  varname_clean <- paste0(varname, '_clean')
  gles_p_long[,varname_clean] <- gles_p_long[, varname]
  gles_p_long[is.na(gles_p_long[,varname]), varname_clean] <- -1000
  gles_p_long[gles_p_long[,varname_clean] < 0, varname_clean] <- NA
  gles_p_long[,varname_clean] <- gles_p_long[,varname_clean] - 4
}

gles_p_long <- gles_p_long %>% 
  select(lfdn, wave, date_new, ends_with('_clean'), ends_with('_bin'))

## save
gles_p_long %>% 
  fwrite(., here('data/gles/Panel/long_cleaned.csv'))
