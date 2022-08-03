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
DiDModels <- function(dv_name, 
                      cond_var = "init_mig", 
                      cond_name = "\nImmigration Attitude W1",
                      ctrl_var = F, 
                      restrict_period = F,
                      restrict_sample = F,
                      estimate = "{estimate}{stars}",
                      ctrl_name,
                      gof_omit = "^(?!R2.Pseudo|Num|Std|FE)") {
  
  gles_p_long <- data.table::fread(here('data/raw/gles/Panel/long_cleaned.csv'))
  
  # restrict to relevant time frame
  if(restrict_period){
    
    gles_p_long <-
      gles_p_long %>%
      filter(date_clean >= as.Date("2016-01-01"),
             date_clean <  as.Date("2018-01-01"))
  
  }
  
  if (restrict_sample == "conservative_outlets"){
    gles_p_long <-
      gles_p_long %>%
      filter(bild_init | faz_init | welt_init)
  }
  
  # define dv and conditioning variable
  gles_p_long[["dv"]] <- gles_p_long[[dv_name]]
  gles_p_long[["cond"]] <- gles_p_long[[cond_var]]
  
  gles_p_long$ID <- as.factor(gles_p_long$lfdn)
  
  # model 1: direct effect
  ate <- 
    feglm(dv ~ post * treat, 
          cluster = c("ID"), 
          data = 
            gles_p_long %>% 
            filter(!is.na(dv), !is.na(treat), !is.na(post)))
  
  if (!isFALSE(ctrl_var)){
    
    if (ctrl_var == "cond"){
      ate_ctrl <- 
        feglm(dv ~ post * treat + cond, 
              cluster = c("ID"), 
              data = 
                gles_p_long %>% 
                filter(!is.na(dv), !is.na(treat), !is.na(post), !is.na(cond)))
      
    } else {
      
      gles_p_long[["ctrl"]] <- gles_p_long[[ctrl_var]]
      
      ate_ctrl <- 
        feglm(dv ~ post * treat + ctrl, 
              cluster = c("ID"), 
              data = 
                gles_p_long %>% 
                filter(!is.na(dv), !is.na(treat), !is.na(post), !is.na(ctrl)))
      
    }
    
  }
  
  ate_fe <- 
    feglm(dv ~ post * treat | ID, # treat gets dropped due to MC with pre-treatment differences (exactly what it should capture)
          cluster = c("ID"), 
          data = 
            gles_p_long %>% 
            filter(!is.na(dv), !is.na(treat), !is.na(post)))
  
  inter <- 
    feglm(dv ~ post * treat * cond, 
          cluster = c("ID"), 
          data = gles_p_long %>% 
            filter(!is.na(dv), !is.na(treat), !is.na(post), !is.na(cond)))
  
  inter_fe <- 
    feglm(dv ~ post * treat * cond | ID, 
          cluster = c("ID"), 
          data = gles_p_long %>% 
            filter(!is.na(dv), !is.na(treat), !is.na(post), !is.na(cond)))
  
  coef_map <- 
    c("postTRUE:treatTRUE" = "Post X Treat", 
      "postTRUE:treatTRUE:cond" = paste0("Post X Treat X ", cond_name))
    
  if(exists("ate_ctrl")){
    
    if (ctrl_var != "cond"){
      coef_map <- 
        c(coef_map,
          "ctrl" = ctrl_name)
      
    } else {
      
      coef_map <- 
        c(coef_map,
          "cond" = cond_name)
      
    }
    
    
  }
  
   
  if(exists("ate_ctrl")){
    
    model_list <- list(ate, ate_ctrl, ate_fe, inter, inter_fe)
  
  } else {
    
    model_list <- list(ate, ate_fe, inter, inter_fe)
    
  }    

  return(
    modelsummary(
      
      model_list,
      stars = T,
      coef_map = coef_map,
      gof_omit = gof_omit, 
      output = "markdown"
    )
  )

}


MigCrimeCorTable <- function(){
  
  gles_p_long <- data.table::fread(here('data/raw/gles/Panel/long_cleaned.csv'))
  
  # define dv and conditioning variable
  gles_p_long[["dv"]] <- gles_p_long[["1130_clean"]]
  gles_p_long[["crime_att"]] <- gles_p_long[["2880h_clean"]]
  gles_p_long$ID <- gles_p_long$lfdn
  
  inter <- 
    feglm(dv ~ 
            crime_att + 
            post : crime_att + 
            treat : crime_att +
            post : treat : crime_att, 
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
    gof_omit = ".*",
    output = "markdown"
    ) %>% 
    return()
}



TWFEModels <- function(dv_name, dataset = "bert", 
                       cond_var = "init_mig", 
                       cond_name = "\nImmigration Attitude W1"){
  
  if (str_to_lower(dataset) == "bert"){
    merged_data <- data.table::fread(file = here("data/processed/merged_bert.csv"))
  } else if (str_to_lower(dataset) == "stm"){
    merged_data <- data.table::fread(file = here("data/processed/merged_stm.csv"))
  }else{
    stop("Dataset needs to be 'bert' or 'stm'.")
  }
  
  merged_data <- 
    merged_data %>%  
    filter(lag == 7)
  
  merged_data[["dv"]] <- merged_data[[dv_name]]
  merged_data[["cond"]] <- merged_data[[cond_var]]
  
  merged_data$ID <- merged_data$lfdn
  merged_data$Wave <- merged_data$wave
  merged_data <- 
    merged_data %>% 
    mutate(mig_share = n_mig/n_tot)
  
  # direct crime
  ind_ate <- 
    feglm(dv ~ crime_label_share | Wave + ID,
          cluster = c("ID"), data = merged_data)
  
  # direct crime + migration exposure
  ind_ate_ctrl <- 
    feglm(dv ~ crime_label_share + mig_share | Wave + ID,
          cluster = c("ID"), data = merged_data)
  
  # crime X initial imm_att
  ind_inter <- 
    feglm(dv ~ crime_label_share:cond + crime_label_share | Wave + ID,  
          cluster = c("ID"), data = merged_data)
  
  # crime X initial imm_att + mig exposure
  ind_inter_ctrl <- 
    feglm(dv ~ crime_label_share:cond + crime_label_share + mig_share | Wave + ID,  
          cluster = c("ID"), data = merged_data)
  
coef_map <- 
  c("Crime Share", 
    "Migration Salience",
    paste0("Crime Share X ", cond_name))
  
names(coef_map) <- 
  c("crime_label_share",
    "mig_share",
    paste0("crime_label_share:cond",
           ifelse(
             is.numeric(merged_data$cond),
             "",
             ifelse(
               is.logical(merged_data$cond),
               "TRUE",
               stop(paste("Cannot handle conditional variables of class", class("cond"), "at the moment."))
               )
             )
           )
    )
  return(
    modelsummary(
      list(
        ind_ate, ind_ate_ctrl, ind_inter, ind_inter_ctrl
      ),
      stars = T,
      coef_map = coef_map,
      gof_omit = "^(?!R2.Pseudo|Num|Std|FE)", 
      output = "markdown"
    )
  )
  
  
}


TWFEReadership <- function(dataset = "bert"){
  
  # load data
  if (str_to_lower(dataset) == "bert"){
    merged_data <- data.table::fread(file = here("data/processed/merged_bert.csv")) %>% 
      as_tibble()
  } else if (str_to_lower(dataset) == "stm"){
    merged_data <- data.table::fread(file = here("data/processed/merged_stm.csv")) %>% 
      as_tibble()
  }else{
    stop("Dataset needs to be 'bert' or 'stm'.")
  }
  
  merged_data <- 
    merged_data %>%  
    filter(lag == 7)
  
  # independent var
  merged_data$iv <- merged_data$`1130_clean`
  
  models <- list()
  
  for (i in letters[c(1, 3:6)]){
    
    if (i == "a"){
      paper <-  "Bild"
    }else if(i == "c"){
      paper <- "FAZ"
    }else if(i == "d"){
      paper <- "SZ"
    }else if(i == "e"){
      paper <- "taz"
    }else if(i == "f"){
      paper <- "Welt"
    }
    
    # define dv
    merged_data[, "dv"] <- merged_data[, paste0("1661", i, "_bin")]
    
    # direct crime
    model <- 
      feglm(dv ~ iv | wave + lfdn,
            cluster = c("lfdn"), data = merged_data)
    
    models[[paper]] <- model
    
  }
  
  return(modelsummary(models,
                      coef_map = c("iv" = "Migration Attitude"),
                      gof_omit = "^(?!Std|FE)",
                      stars = T, 
                      output = "markdown"))
  
}

TobitDiD <- function(dv_name, 
                     cond_var = "init_mig", 
                     cond_name = "\nImmigration Attitude W1") {
  
  gles_p_long <- data.table::fread(here('data/raw/gles/Panel/long_cleaned.csv'))
  
  # define dv and conditioning variable
  gles_p_long[["dv"]] <- gles_p_long[[dv_name]]
  gles_p_long[["cond"]] <- gles_p_long[[cond_var]]
  
  
  
  # model 1: direct effect
  ate <- 
    censReg::censReg(
      dv ~ post * treat, 
          left = -3, right = 3, 
          data = 
            gles_p_long %>% 
            filter(!is.na(dv), !is.na(treat), !is.na(post)))
  
  inter <- 
    censReg::censReg(
      dv ~ post * treat * cond, 
      left = -3, right = 3,
          data = gles_p_long %>% 
            filter(!is.na(dv), !is.na(treat), !is.na(post), !is.na(cond)))
  
  
  coef_map <- 
    c("postTRUE:treatTRUE" = "ATE", 
      "postTRUE:treatTRUE:cond" = paste0("ATE X ", cond_name))
  
  model_list <- list(ate, inter)
    
  
  return(
    modelsummary(
      model_list,
      stars = T,
      coef_map = coef_map,
      gof_omit = "^(?!R2.Pseudo|Num|Std|FE)", 
      output = "markdown"
    )
  )
  
  
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

