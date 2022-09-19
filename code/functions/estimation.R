# ______________________________________________
# Bild
# Goal: Functions for Estimation
# ______________________________________________
# Date:  Sun May 15 13:50:05 2022
# Author: Nicolai Berk
#  R version 4.1.1 (2021-08-10)
# ______________________________________________


# Setup ####
library(tidyverse)
library(dplyr)
library(here)
library(fixest)
library(modelsummary)

# Difference-in-Difference model

SingleDVModel <- function(varname = NULL){
  
  if(is.null(varname)){stop("Need to define varname!")}
  
  gles_p_long <- 
    fread(here('data/raw/gles/Panel/long_cleaned.csv')) %>% 
    as_tibble()
  
  postdate <- as.Date("2017-02-01")
  title <- "Mean migration attitude post-Reichelt takeover"
  
  gles_p_long["dv"] <- gles_p_long[varname]
  
  raw_model <- 
    fixest::feglm(dv ~ post*treat, 
                  data = gles_p_long)
  
  id_fes <- 
    fixest::feglm(dv ~ post*treat | ID, 
                  data = 
                    gles_p_long %>% 
                    filter(!is.na(dv)) %>% 
                    mutate(
                      ID = lfdn
                    ) %>% 
                    select(dv, post, treat, ID))
  
  id_fes_clustered <- 
    fixest::feglm(dv ~ post*treat | ID, 
                  data = 
                    gles_p_long %>% 
                    filter(!is.na(dv)) %>% 
                    mutate(
                      ID = lfdn
                    ) %>% 
                    select(dv, post, treat, ID),
                  cluster = c("ID"))
  
  twfe_no_clustering <- 
    fixest::feglm(dv ~ post*treat | ID + Wave, 
                  data = 
                    gles_p_long %>% 
                    filter(!is.na(dv)) %>% 
                    mutate(
                      Wave = 
                        ifelse(
                          wave %in% unique(.$wave[c(1,2)]),
                          "reference",
                          wave
                        ),
                      ID = lfdn
                    ) %>% 
                    select(dv, post, treat, ID, Wave))
  
  full_model <- 
    fixest::feglm(dv ~ post*treat | ID + Wave, 
                  data = 
                    gles_p_long %>% 
                    filter(!is.na(dv)) %>% 
                    mutate(
                      Wave = 
                        ifelse(
                          wave %in% unique(.$wave[c(1,2)]),
                          "reference",
                          wave
                        ),
                      ID = lfdn
                    ) %>% 
                    select(dv, post, treat, ID, Wave),
                  cluster = c("ID", "Wave"))
  
  modelsummary::modelsummary(
    list(
      raw_model,
      id_fes,
      id_fes_clustered,
      twfe_no_clustering,
      full_model
    ), 
    output = "latex",
    stars = T) %>% 
    kableExtra::kable_styling() %>% 
    kableExtra::row_spec(6, bold = T) %>% 
    return()
  
}


MigCrimeCorTable <- function(){
  
  gles_p_long <- data.table::fread(here('data/raw/gles/Panel/long_cleaned.csv'))
  
  # define dv and conditioning variable
  gles_p_long[["dv"]] <- gles_p_long[["1130_clean"]]
  gles_p_long[["crime_att"]] <- gles_p_long[["2880h_clean"]]
  gles_p_long$ID <- gles_p_long$lfdn
  
  inter <- 
    feglm(dv ~ 
            crime_att * post * treat, 
          cluster = c("ID"), 
          data = gles_p_long %>% 
            filter(!is.na(dv), !is.na(treat), !is.na(post), !is.na(crime_att)))
  
  modelsummary(
    inter, 
    coef_map = 
      c(
        "crime_att" = "Crime Attitude",
        "crime_att:postTRUE" = "Crime Attitude X Post",
        "crime_att:treatTRUE" = "Crime Attitude X Bild Reader",
        "crime_att:postTRUE:treatTRUE" = "Crime Attitude X Post X Bild"
      ),
    stars = T,
    gof_omit = "R2*|AIC|BIC|Log.Lik.",
    output = "markdown"
    ) %>% 
    return()
}


SelectionModel <- function() {
  
  ## load data
  gles_p_long <- 
    fread(here('data/raw/gles/Panel/long_cleaned.csv')) %>% 
    as_tibble()
  
  ## define parameters
  postdate <- as.Date("2017-02-01")
  
  ## select relevant sample
  gles_p_long <- 
    gles_p_long %>% 
    filter(
      bild_init, ## restrict to Bild readers
      init_mig_groups != "", ## exclude missings in first wave
      !is.na(date_clean),
      wave %in% c(1, 3, 4, 6:8)) %>%
    mutate(init_mig_groups = 
             factor(init_mig_groups, 
                    levels = c("Liberal", 
                               "Neutral",
                               "Conservative")),
           dv = `1661a_bin`,
           post = date_clean >= postdate
    )
    
  
  
  ## model
  lm(
    dv ~ init_mig_groups,
    data = 
      gles_p_long %>% 
      filter(wave != "1")
  ) %>% 
    return()
  
  
}

