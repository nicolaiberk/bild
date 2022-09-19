# ______________________________________________
# Bild
# Goal: Generate visualisations for Paper and appendix
# ______________________________________________
# Date:  Fri May 13 11:32:01 2022
# Author: Nicolai Berk
#  R version 4.1.1 (2021-08-10)
# ______________________________________________


# Setup ####
library(tidyverse)
library(dplyr)
library(here)
library(data.table)
library(ggrepel)


# Migration Salience ####

MigSaliencePlot <- function(aggregation = "months"){
  
  mig_salience_plot <- 
    fread(here("data/processed/bert_crime_daily.csv")) %>% 
    select(date_clean, paper, mig_share) %>% 
    mutate(date_new = lubridate::floor_date(date_clean, aggregation)) %>% 
    group_by(date_new, paper) %>% 
    summarise_all(mean) %>% 
    filter(date_new < as.Date('2019-01-01')) %>%
    ggplot(aes(x = date_new, y = mig_share, col = paper, lty = paper, shape = paper)) +
    geom_line() + geom_point() +
    ggtitle('Migration salience in different newspapers') +
    xlab('') + ylab('Share of migration articles') +
    theme_minimal() +
    labs(color = "Outlet", lty = "Outlet", shape = "Outlet")
  
  return(mig_salience_plot)
}

TenMostCrime <- function(){
  fread(here('data/processed/bert_crime_clean.csv'), encoding = "UTF-8") %>% 
    filter(crime_label > 0, paper == "Bild") %>% 
    arrange(crime_prob) %>% 
    select(title) %>% tail(5) %>% 
    knitr::kable(col.names = "Five headlines with highest crime score") %>% 
    return()
}

# Treatment: shift in attention to crime

TreatmentTrendPlot <- function(size = 1, 
                               palette = MetBrewer::met.brewer("Archambault", 2)[c(2,1)],
                               maxdate = as.Date("2020-01-01"),
                               mindate = as.Date("2015-01-01"),
                               aggregation = "Quarter",
                               dpa_corrected = F) {
  
  ## load BERT data
  bert_ests_raw <- 
    fread(here('data/processed/bert_crime_clean.csv')) 
  
  if(dpa_corrected){
    
    bert_ests_raw <- 
      bert_ests_raw %>% 
      filter(!dpa)
    
    
  }
  
    bert_ests <- 
      bert_ests_raw %>% 
      mutate(month = lubridate::floor_date(date_clean, str_to_lower(aggregation))) %>% 
      group_by(paper, month) %>% 
      summarise(
        crime = mean(crime_label)
      ) %>% 
      mutate(paper = ifelse(paper %in% c("bild", "Bild"), "Bild", "Other")) %>%
      group_by(month, paper) %>%
      select(crime) %>% 
      summarise(
        crime = mean(crime)
      )
    
    survey_dates <- 
      fread(here("data/processed/SurveyDates.csv")) %>% 
      filter(as.Date(date) < as.Date(maxdate))
    
    ## visualise
    trendplot <- 
      bert_ests %>% 
      filter(month < maxdate, month >= mindate) %>% 
      ggplot(aes(x = month, y = crime, col = paper, lty = paper)) +
      geom_vline(xintercept = as.Date("2017-02-01"), lty = 2, col = "red", size = size) +
      geom_line(size = size) +
      geom_rug(data = survey_dates, aes(x = date), sides = 't', inherit.aes = F) +
      ggtitle(paste0(aggregation, "ly Share of Migration Content Devoted to Crime Frames"), 
              paste0("Bild vs. other major daily newspapers, 2016-", lubridate::year(maxdate))) + 
      xlab("Date") + 
      ylab(paste0("Share of Migration Articles", 
                  ifelse(size > 1,
                         "",
                         " Containing Crime Frame")
                  )
           ) +
      theme_minimal() +
      theme(legend.position = "none") +
      scale_color_manual(values = palette)
    
    return(trendplot)
  
}

TreatmentDiDPlot <- function(size = 1, 
                             palette = MetBrewer::met.brewer("Archambault", 2)[c(2,1)],
                             maxdate = as.Date("2020-01-01"),
                             mindate = as.Date("2015-01-01"),
                             dpa_corrected = F,
                             aggregation = "day"){
  
  
  ## load BERT data
  bert_ests_raw <- 
    fread(here('data/processed/bert_crime_clean.csv')) %>% 
    filter(
      # paper != "Spiegel", # as Spiegel not part of the analysis
           date_clean < maxdate,
           date_clean >= mindate) %>% 
    group_by(paper, date_clean) %>% 
    summarise(dv = mean(crime_label),
              dpa = mean(dpa)) %>%  
    mutate(post = ifelse(date_clean >= as.Date("2017-02-01"), T, F),
           quarter_id = lubridate::floor_date(date_clean, aggregation))
    
  
  ## DiD with placebo papers
  
  treatment_ests <- 
    data.frame()
  
  for (p in unique(bert_ests_raw$paper)){
    
    bert_ests_raw$treatment <-  bert_ests_raw$paper == p
    
    if(dpa_corrected){
      
      did_model <- 
        fixest::feglm(
          dv ~ post*treatment + dpa | quarter_id, 
          data = bert_ests_raw)
      
    }else{
      
      did_model <- 
        fixest::feglm(
          dv ~ post*treatment | quarter_id, 
          data = bert_ests_raw)
      
    }
    
    
    
    
    est <- did_model$coefficients["postTRUE:treatmentTRUE"][[1]]
    lower <- confint(did_model)["postTRUE:treatmentTRUE", 1]
    upper <- confint(did_model)["postTRUE:treatmentTRUE", 2]
    
    treatment_ests <- 
      rbind(treatment_ests,
            data.frame(
              paper = p,
              est   = est,
              lower = lower,
              upper = upper
            ))
  }
  
  
  ## vis
  treatment_ests$paper <-
    factor(treatment_ests$paper,
           levels = treatment_ests %>%
             arrange(est) %>%
             select(paper) %>%
             unique() %>%
             unlist()
    )
  
  effect_size <- 
    treatment_ests %>% 
    filter(paper == "Bild") %>% 
    select(est) %>% 
    unlist()
  
  treatment_ests %>% 
    ggplot(aes(x = est, xmin = lower, xmax = upper, 
               y = paper, 
               col = paper != "Bild", 
               shape = paper != "Bild")) +
    geom_pointrange(size = size) +
    geom_text(aes(x = effect_size, y = "Bild", 
                  label = as.character(round(effect_size, 3)),
                  hjust = 1.5, size = 20),
                  col = palette[1]) +
    geom_vline(xintercept = 0,
               col = "red", lty = 2) +
    theme_minimal() +
    xlab("Estimated DiD") + ylab("") +
    theme(legend.position = "None") +
    scale_color_manual(values = palette) +
    coord_flip() %>% 
    return()
  
  
}


# Effect ####

## Descriptive visualisation - cntrl vs treatment (by group)

DescPlot <- function(legend_position = "right",
                     colorful = F){
  
  gles_p_long <- fread(here('data/raw/gles/Panel/long_cleaned.csv'))
  
  postdate <- as.Date("2017-02-01")
  title <- "Mean migration attitude post-Reichelt takeover"
  subtitle <- "by readership"
  
  gles_p_long %>% 
    filter(!is.na(treat), !is.na(`1130_clean`)) %>%
    group_by(treat, post, wave) %>% 
    summarise(
      dv_mean = mean(`1130_clean`),
      date_min = min(date_clean)
    ) %>% 
    ggplot(aes(x = date_min, y = dv_mean, 
               col = !treat, shape = !treat, lty = !treat)) +
    geom_line() + geom_point() +
    geom_vline(xintercept = postdate, lty = 2, col = ifelse(colorful, "red", "black")) +
    xlab("") + ylab("Immigration Attitude") +
    scale_color_manual(name = "Group",
                       labels = c("Not Bild-reader", "Bild reader") %>% 
                         rev(),
                       values = if(colorful){
                         MetBrewer::met.brewer("Archambault", 2)
                       }else{
                         c("black", "darkgray")
                       }) +
    scale_linetype_discrete(name = "Group",
                              labels = c("Not Bild-reader", "Bild reader") %>% 
                         rev()) +
    scale_shape_discrete(name = "Group", 
                       labels = c("Not Bild-reader", "Bild reader") %>% 
                         rev()) +
    theme_minimal() +
    theme(legend.position = legend_position) +
    ggtitle(title, subtitle) %>% 
    return()
  
}



DiDPlot <- function(multi = F, 
                       dv_var = "1130_clean",
                       dv_name = "Migration Attitude",
                       scaled = T,
                       size = 1,
                       boundary_share = 0.25,
                       theoretical_effect_size = 0.6,
                       clustering = "lfdn") {
  
  gles_p_long <- 
    fread(here('data/raw/gles/Panel/long_cleaned.csv')) %>% 
    as_tibble()
  
  postdate <- as.Date("2017-02-01")
  title <- "Mean migration attitude post-Reichelt takeover"
  
  if(multi){
    
    if(scaled == "raw"){
      stop("scaled = 'raw' not supported for multi = T.")
    }else if(scaled){
      # standardised dv
      gles_p_long["dv"] <- scale(gles_p_long["1130_clean"])
      theoretical_effect_size <- theoretical_effect_size/sd(gles_p_long[["1130_clean"]], na.rm = T)
      
    }else if (!scaled){
      
      # else percentage of scale
      gles_p_long$dv <- scales::rescale(gles_p_long$`1130_clean`, to = c(0,1))

    }
    
    mig_att_model <- 
      fixest::feglm(dv ~ post*treat | lfdn + wave_ctrl, 
                    data = 
                      # add control for different post-waves
                      gles_p_long %>% 
                      filter(!is.na(dv)) %>% 
                      mutate(
                        wave_ctrl = 
                          ifelse(
                            wave %in% unique(.$wave[c(1,2)]),
                            "reference",
                            wave
                          ) 
                      ) %>% 
                      select(dv, post, treat, lfdn, wave_ctrl),
                    cluster = clustering
                    )
    
    if(scaled == "raw"){
      stop("scaled = 'raw' not supported for multi = T.")
    }else if(scaled){
      # standardised dv
      gles_p_long["dv"] <- scale(gles_p_long["1210_clean"])
      
    }else if (!scaled){
      
      # else percentage of scale
      gles_p_long$dv <- scales::rescale(gles_p_long$`1210_clean`, to = c(0,1))
      
    }
    
    int_att_model <- 
      fixest::feglm(dv ~ post*treat | lfdn + wave_ctrl, 
                    data = 
                      # add control for different post-waves
                      gles_p_long %>% 
                      filter(!is.na(dv)) %>% 
                      mutate(
                        wave_ctrl = 
                          ifelse(
                            wave %in% unique(.$wave[c(1,2)]),
                            "reference",
                            wave
                          ) 
                      ) %>% 
                      select(dv, post, treat, lfdn, wave_ctrl),
                    cluster = clustering)
    
    if(scaled == "raw"){
      stop("scaled = 'raw' not supported for multi = T.")
    }else if(scaled){
      # standardised dv
      gles_p_long["dv"] <- scale(gles_p_long["mip_mig"])
      
    }else if (!scaled){
      
      # else percentage of scale
      gles_p_long$dv <- scales::rescale(gles_p_long$mip_mig, to = c(0,1))
      
    }
    
    mip_model <- 
      fixest::feglm(dv ~ post*treat | lfdn + wave_ctrl, 
                    data = 
                      # add control for different post-waves
                      gles_p_long %>% 
                      filter(!is.na(dv)) %>% 
                      mutate(
                        wave_ctrl = 
                          ifelse(
                            wave %in% unique(.$wave[c(1,2)]),
                            "reference",
                            wave
                          ) 
                      ) %>% 
                      select(dv, post, treat, lfdn, wave_ctrl),
                    cluster = clustering)
    
    if(scaled == "raw"){
      stop("scaled = 'raw' not supported for multi = T.")
    }else if(scaled){
      # standardised dv
      gles_p_long["dv"] <- scale(gles_p_long["430i_clean"])
      
    }else if (!scaled){
      
      # else percentage of scale
      gles_p_long$dv <- scales::rescale(gles_p_long$`430i_clean`, to = c(0,1))
      
    }
    
    afd_model <- 
      fixest::feglm(dv ~ post*treat | lfdn + wave_ctrl, 
                    data = 
                      # add control for different post-waves
                      gles_p_long %>% 
                      filter(!is.na(dv)) %>% 
                      mutate(
                        wave_ctrl = 
                          ifelse(
                            wave %in% unique(.$wave[c(1,2)]),
                            "reference",
                            wave
                          ) 
                      ) %>% 
                      select(dv, post, treat, lfdn, wave_ctrl),
                    cluster = clustering)
    
    modelsummary::modelplot(
      list("AfD thermometer" = afd_model,
           "MIP: Migration" = mip_model,
           "Integration Attitude" = int_att_model,
           "Migration Attitude" = mig_att_model),
      coef_map = list("postTRUE:treatTRUE" = ""),
      facet = T, 
      ) +
      geom_vline(xintercept = 0, lty = 1, col = "red", size = size) +
      geom_vline(xintercept = theoretical_effect_size*boundary_share, lty = 3, col = "black", size = size) +
      geom_vline(xintercept = -1*theoretical_effect_size*boundary_share, lty = 3, col = "black", size = size) +
      geom_vline(xintercept = theoretical_effect_size, lty = 2, col = "black", size = size) +
      geom_vline(xintercept = -1*theoretical_effect_size, lty = 2, col = "black", size = size)

  }else{
    
    
    if (scaled != "raw"){
    
      if(scaled){
        # standardised dv
        gles_p_long[["dv"]] <- scale(gles_p_long[[dv_var]])
      }else{
        # else normalise to [0,1]
        gles_p_long[["dv"]] <- scales::rescale(gles_p_long[[dv_var]], to = c(0,1))
      }
      
    }else{
      
      gles_p_long["dv"] <- gles_p_long[dv_var]
      
    }
    
    single_model <- 
      fixest::feglm(dv ~ post*treat | lfdn + wave_ctrl, 
                    data = 
                      # add control for different post-waves
                      gles_p_long %>% 
                      filter(!is.na(dv)) %>% 
                      mutate(
                        wave_ctrl = 
                          ifelse(
                            wave %in% unique(.$wave[c(1,2)]),
                            "reference",
                            wave
                          ) 
                      ) %>% 
                      select(dv, post, treat, lfdn, wave_ctrl),
                    cluster = clustering)
    
    modelsummary::modelplot(
      list(dv_name = single_model),
      coef_map = list("postTRUE:treatTRUE" = ""),
      facet = T
    ) +
      # indicate either 0.5 SDs or 10% of the sclae (adjusted to scale in raw case)
      geom_vline(xintercept = 0, lty = 1, col = "red", size = size) +
      geom_vline(xintercept = boundary_share*theoretical_effect_size, 
                 lty = 3, col = "black", size = size) +
      geom_vline(xintercept = boundary_share*-1*theoretical_effect_size, 
                 lty = 3, col = "black", size = size) +
      geom_vline(xintercept = theoretical_effect_size, 
                 lty = 2, col = "black", size = size) +
      geom_vline(xintercept = -1*theoretical_effect_size, 
                 lty = 2, col = "black", size = size)
    
  }

}


DiDByWavePlot <- function(dv = "1130_clean", 
                             size = 1,
                             scaled = F,
                             boundary_share = 0.25,
                             theoretical_effect_size = 0.6,
                             return_ests = F) {
  
  gles_p_long <- 
    fread(here('data/raw/gles/Panel/long_cleaned.csv')) %>% 
    as_tibble()
  
  if(scaled){
    
    gles_p_long[["dv"]] <- scale(gles_p_long[[dv]])
    
  }else if (!scaled){

    gles_p_long[["dv"]] <- gles_p_long[[dv]]
    
  }else{
    stop("'dv' must be logical!")
  }
  
  postdate <- as.Date("2017-02-01")
  
  waves <- 
    gles_p_long %>% 
    filter(!is.na(dv), !wave %in% c("1", "", "a1", "a2")) %>%
    select(wave) %>% 
    unique() %>% 
    .[[1]]
  
  ests <- data.frame()
  
  for (w in waves){
    
    temp <- 
      gles_p_long %>% 
      filter(wave %in% c("1", w))
    
    single_model <- 
      fixest::feglm(dv ~ post*treat | lfdn, 
                    data = temp,
                    cluster = c("lfdn"))
    
    ests <- 
      rbind(ests,
            data.frame(
              wave = w,
              date = max(gles_p_long$date_clean[gles_p_long$wave == w], na.rm = T),
              point = single_model$coefficients[["postTRUE:treatTRUE"]],
              lower = confint(single_model)["postTRUE:treatTRUE", "2.5 %"],
              upper = confint(single_model)["postTRUE:treatTRUE", "97.5 %"]
              )
            )
   
    
  }
  
  if(return_ests){
    
    return(ests)
  
  }else{
    
    ests <- 
      rbind(
        ests,
        data.frame(
          wave = 1,
          date = max(gles_p_long$date_clean[gles_p_long$wave == 1], na.rm = T),
          point = 0,
          lower = 0,
          upper = 0
        )
      )
    
    ests %>% 
      ggplot(aes(x = date, y = point, ymin = lower, ymax = upper)) +
      geom_hline(yintercept = 0, col = "black", size = size) +
      geom_hline(yintercept = boundary_share*theoretical_effect_size,
                 lty = 3, col = "black", size = size) +
      geom_hline(yintercept = boundary_share*-1*theoretical_effect_size, 
                 lty = 3, col = "black", size = size) +
      geom_hline(yintercept = theoretical_effect_size,
                 lty = 2, col = "black", size = size) +
      geom_hline(yintercept = -1*theoretical_effect_size,
                 lty = 2, col = "black", size = size) +
      geom_vline(xintercept = as.Date(postdate), lty = 2, col = "black", size = size) +
      geom_pointrange(size = size) +
      xlab("Date") + ylab("Effect") +
      theme_minimal() %>% 
      return()
    
  }
}

## Pre-post correlation of migration and crime ####

### plot

MigCrimeCorPlot <- function(size = 1){
  
  cors <- 
    fread(here('data/raw/gles/Panel/long_cleaned.csv')) %>% 
    filter(!is.na(`1130_clean`), !is.na(`2880h_clean`), !is.na(treat)) %>% 
    group_by(treat, wave) %>% 
    summarise(mig_crime_cor = cor(`1130_clean`, `2880h_clean`),
              date_clean = min(date_clean)) %>% 
    mutate(Universe = "Factual")
  
  dates <- 
    cors %>%
    ungroup() %>% 
    select(wave, date_clean) %>%
    unique()
  
  cors <- 
    cors %>% 
    rbind(
      data.frame(
        treat = c(TRUE, TRUE, TRUE),
        wave = c("1", "3", "13"),
        date_clean = c(dates$date_clean[dates$wave == "1"],
                       dates$date_clean[dates$wave == "3"],
                       dates$date_clean[dates$wave == "13"]
                        ),
        mig_crime_cor = c(cors[cors$treat & cors$wave == "1", "mig_crime_cor"][[1]], 
                          cors[cors$treat & cors$wave == "1", "mig_crime_cor"][[1]] +  
                            cors[!cors$treat & cors$wave == "3", "mig_crime_cor"][[1]] -
                            cors[!cors$treat & cors$wave == "1", "mig_crime_cor"][[1]], 
                          cors[cors$treat & cors$wave == "1", "mig_crime_cor"][[1]] +  
                            cors[!cors$treat & cors$wave == "13", "mig_crime_cor"][[1]] -
                            cors[!cors$treat & cors$wave == "1", "mig_crime_cor"][[1]]),
        Universe = "Counterfactual"
      ), 
    ) %>% 
    mutate(Universe = factor(Universe, levels = c("Factual", "Counterfactual")))
  

  cors %>% 
    mutate(treat_universe = paste(treat, Universe, sep = "_")) %>% 
    ggplot(aes(x = date_clean, y = mig_crime_cor, lty = Universe, shape = treat, 
               group = treat_universe)) +
    geom_point(size = size*2) +
    geom_line(size = size) +
    theme_minimal() +
    xlab("") + ylab("Pearson's r") +
    scale_color_discrete(name = "Readership", 
                         labels = c("FALSE" = "Not Bild", 
                                    "TRUE" = "Bild")) +
    scale_shape_discrete(name = "Readership", 
                         labels = c("FALSE" = "Not Bild", 
                                    "TRUE" = "Bild")) %>% 
    return()
  
}


MigCrimeCorPlotBS <- function(size = 1, return_ests = F){
  
  load(here("data/processed/did_bootstraps_cor.RData"))
  
  if(return_ests){
    
    return(bs_cor_mig_crime)
    
  }else{
    
    cors <- 
      bs_cor_mig_crime %>% 
      group_by(treat, wave) %>% 
      summarise(lower = quantile(mig_crime_cor, 0.025),
                upper = quantile(mig_crime_cor, 0.975),
                point = mean(mig_crime_cor)) %>% 
      mutate(Universe = "Factual")
  
    
    
    cors <- 
      cors %>% 
      rbind(
        data.frame(
          treat = c(TRUE, TRUE, TRUE),
          wave = c("1", "3", "13"),
          point = c(cors[cors$treat & cors$wave == "1", "point"][[1]], 
                    cors[cors$treat & cors$wave == "1", "point"][[1]] +  
                      cors[!cors$treat & cors$wave == "3", "point"][[1]] -
                      cors[!cors$treat & cors$wave == "1", "point"][[1]], 
                    cors[cors$treat & cors$wave == "1", "point"][[1]] +  
                      cors[!cors$treat & cors$wave == "13", "point"][[1]] -
                      cors[!cors$treat & cors$wave == "1", "point"][[1]]
                    ),
          Universe = "Counterfactual"
        ),
      ) %>% 
      mutate(Universe = factor(Universe, levels = c("Factual", "Counterfactual")))
    
    ## merge with survey dates
    cors <- 
      fread(here('data/raw/gles/Panel/long_cleaned.csv')) %>% 
      filter(!is.na(date_clean)) %>% 
      select(date_clean, wave) %>% 
      group_by(wave) %>% 
      summarise(date_clean = min(date_clean, na.rm = T)) %>% 
      right_join(cors, by = "wave")
    
    cors %>% 
      mutate(treat_universe = paste(treat, Universe, sep = "_")) %>% 
      ggplot(aes(x = date_clean, 
                 y = point, ymin = lower, ymax = upper, 
                 col = treat, lty = Universe, shape = treat, 
                 group = treat_universe)) +
      geom_errorbar(size = size, width = 30) +
      geom_point(size = size) +
      geom_line(size = size) +
      theme_minimal() +
      xlab("") + ylab("Pearson's r") +
      scale_color_manual(name = "Readership", 
                           labels = c("FALSE" = "Not Bild", 
                                      "TRUE" = "Bild"),
                           values = c("gray", "black")) +
      scale_shape_discrete(name = "Readership", 
                           labels = c("FALSE" = "Not Bild", 
                                      "TRUE" = "Bild")) %>% 
      return()
    
  }
}


# Readership ####

OutSelection <- function(color_palette = MetBrewer::met.brewer("Archambault", 3)[c(1,3,2)]) {
  
  ## load data
  gles_p_long <- 
    fread(here('data/raw/gles/Panel/long_cleaned.csv')) %>% 
    as_tibble()
  
  ## define parameters
  postdate <- as.Date("2017-02-01")
  
  
  gles_p_long <- 
    gles_p_long %>% 
    filter(
      bild_init, ## restrict to Bild readers
      init_mig_groups != "", ## exclude missings in first wave
      !is.na(date_clean)) %>%
    mutate(init_mig_groups = 
             factor(init_mig_groups, 
                    levels = c("Liberal", 
                               "Neutral",
                               "Conservative"))
           ) %>% 
    ## group by wave and initial attitude
    group_by(wave, init_mig_groups) %>% 
    summarise(
      N_readers = n(),
      date_clean = min(date_clean),
      still_readers = sum(!is.na(`1661a_clean`) & 
                            `1661a_clean` > 0)
      ) %>% 
    mutate(reader_share = still_readers/N_readers)

  hist <- 
    gles_p_long %>% 
    filter(wave == 1) %>% 
    ggplot(aes(x = init_mig_groups,
               y = N_readers,
               fill = init_mig_groups)) +
    geom_col() +
    xlab("") + ylab("N") +
    guides(fill = "none", x = "none") +
    scale_fill_manual(values = color_palette) +
    theme_minimal()
  
  gles_p_long %>%
    filter(still_readers > 0) %>% 
    ggplot(aes(date_clean, 
               reader_share,
               col = init_mig_groups,
               shape = init_mig_groups,
               lty = init_mig_groups)) +
    geom_vline(xintercept = postdate, col = "red", lty = 2) +
    geom_line() +
    geom_point() +
    geom_label_repel(data = 
                gles_p_long %>% 
                ungroup() %>% 
                filter(reader_share > 0) %>% 
                filter(date_clean == max(date_clean)),
              aes(label = as.character(round(reader_share, 2)),
                  fill = init_mig_groups),
              col = "black",
              size = 4) +
    theme_minimal() +
    scale_color_manual(name = "Migration Attitude",
                         values = color_palette) +
    scale_fill_manual(values = color_palette) +
    scale_shape_discrete(name = "Migration Attitude") +
    guides(lty = "none", fill = "none") +
    ggtitle("Share of Initial Readers Reporting Readership in subsequent Waves", "by Initial Migration Attitude") +
    ylab("Share") + xlab("") +
    hist + 
    guide_area() +
    plot_layout(guides = "collect",
                design = 
                "
                AAAC
                AAAB
                ")
  
  
}

InSelection <- function() {
  
  ## load data
  gles_p_long <- 
    fread(here('data/raw/gles/Panel/long_cleaned.csv')) %>% 
    as_tibble()
  
  ## define parameters
  postdate <- as.Date("2017-02-01")
  
  gles_p_long <- 
    gles_p_long %>% 
    filter(
      init_mig_groups != "", ## exclude missings in first wave
      !is.na(date_clean),
      !is.na(`1661a_bin`),
      wave %in% c(1, 3, 4, 6:8)) %>% 
    mutate(dv = `1661a_bin`,
           placebo = `1661f_bin`,
           iv = `1130_clean`)
  
  ## show association for each wave
  
  ests <- data.frame()
  
  for (w in unique(gles_p_long$wave)){
    
    temp <- 
      gles_p_long %>% 
      filter(wave == w)
      
    bild_model <- lm(dv ~ iv, temp)
    placebo_model <- lm(placebo ~ iv, temp)
      
    
    
    rbind(
      ests,
      data.frame(
        
        date_clean = min(temp$date_clean),
        
        bild_point = bild_model$coefficients[["iv"]],
        bild_lower = confint(bild_model)["iv", "2.5 %"],
        bild_upper = confint(bild_model)["iv", "97.5 %"],
        
        placebo_point = placebo_model$coefficients[["iv"]],
        placebo_lower = confint(placebo_model)["iv", "2.5 %"],
        placebo_upper = confint(placebo_model)["iv", "97.5 %"]
        
      )
    ) -> ests
    
  }
  
  ests %>% 
    pivot_longer(
      cols = bild_point:placebo_upper,
      names_to = c("Model", ".value"),
      names_pattern = "(.*)_(.*)"
      ) %>%
    filter(Model != "placebo") %>% 
    ggplot(
      aes(
          x = date_clean,
          y = point, ymin = lower, ymax = upper,
          # col = Model, shape = Model
          )
      ) +
    geom_vline(xintercept = postdate, lty = 2, col = "red") +
    geom_pointrange() +
    geom_line() +
    theme_minimal() +
    scale_color_discrete(name = "Readership") +
    scale_shape_discrete(name = "Readership") +
    ggtitle("Association of Readership with Immigration Attitude", "Bild and Placebo Paper") +
    ylab("Coefficient") + xlab("")
  
}

# Crowding Out ####

CrowdOutEstimates <- function(vis = T) {
  
  gles_long <- 
    fread(here('data/raw/gles/Panel/long_cleaned.csv')) %>% 
    select(lfdn, date_clean, wave, 
           `1661a_clean`:`1661g_clean`, `1600_clean`, `1681f_clean`,
           `1130_clean`, post, treat) %>% 
    mutate(dv = `1130_clean`,
           treat = as.factor(treat),
           post = as.factor(post))
  
  gles_long <- 
    gles_long %>% 
    filter(wave == "1") %>% 
    mutate(
      other_print = 
        `1661b_clean` +
        `1661c_clean` +
        `1661d_clean` +
        `1661e_clean` +
        `1661f_clean` +
        `1661g_clean`,
      tv_use = `1681f_clean`,
      in_use = `1600_clean`
    ) %>% 
    mutate(other_print = 
             case_when(
               other_print == 0 ~ "0",
               other_print > 0  & 
                 other_print <= 3 ~ "1-3",
               other_print > 3  & 
                 other_print <= 9 ~ "4-9",
               other_print > 9  & 
                 other_print <= 15 ~ "10-15",
               other_print > 15 ~ ">15"
             )
    ) %>% 
    select(lfdn, in_use, tv_use, other_print) %>% 
    right_join(gles_long, by = c("lfdn"))
  
  ests <- 
    data.frame(
      CATE = NULL,
      CATE_lower = NULL,
      CATE_upper = NULL,
      value = NULL,
      moderator = NULL
    )
  
  for (mod in c("tv_use", "in_use", "other_print")){
    
    
    values <- 
      gles_long %>%
      filter(!is.na(get(mod))) %>% 
      select(mod) %>% 
      unique() %>% 
      .[[1]]
    
    for (value in values){
      
      temp <- 
        gles_long %>% 
        filter(get(mod) == value)
      
      m <- 
        fixest::feglm(dv ~ post*treat | ID + Wave, 
                      data = 
                        temp %>% 
                        filter(!is.na(dv)) %>% 
                        mutate(
                          Wave = 
                            ifelse(
                              wave %in% sort(unique(.$wave))[c(1, 2)],
                              "reference",
                              wave
                            ),
                          ID = lfdn
                        ) %>% 
                        select(dv, post, treat, ID, Wave),
                      cluster = c("ID", "Wave"))
      
      ests <- 
        rbind(
          ests,
          data.frame(
            CATE = m$coefficients["postTRUE:treatTRUE"],
            CATE_lower = confint(m)["postTRUE:treatTRUE", "2.5 %"],
            CATE_upper = confint(m)["postTRUE:treatTRUE", "97.5 %"],
            value = value,
            moderator = mod
          )
        )
      
    }
    
  }
  
  if(vis){
    
    ests <- 
      ests %>% 
      mutate(
        effect_label = 
          ifelse(
            value == "0",
            as.character(round(CATE, 3)),
            ""
          ),
        
      )
    
    varnames <- 
      list(
        "in_use" = "Internet use", 
        "other_print" = "Days other print consumed", 
        "tv_use" = "TV use")
    
    labeller <- function(variable,value){
      return(varnames[value])
    }
    
    ests %>%
      ggplot(aes(x = factor(value,
                            levels = as.character(c(0, 1, "1-3", 2, 3, 4, "4-9", 
                                                    5, 6, 7, 8, 9, 10, "10-15",
                                                    ">15"))), 
                 y = CATE, ymin = CATE_lower, ymax = CATE_upper,
                 col = value != "0")) +
      geom_hline(yintercept = 0) +
      geom_hline(yintercept = c(0.15, -0.15), lty = 3) +
      geom_hline(yintercept = c(0.6, -0.6), lty = 2) +
      geom_pointrange() +
      geom_text(aes(x = "0", 
                    y = CATE, 
                    label = effect_label,
                    hjust = -.3)
      ) +
      facet_wrap(~moderator, scales = "free_x",
                 labeller = labeller) +
      theme_minimal() +
      xlab("") +
      theme(legend.position = "None") %>% 
      return()
    
  }else{
    return(ests)
  }
  
}

# Parallel Trends ####

ParallelTrendsPlot <- function(){
  
  color_palette = MetBrewer::met.brewer("Archambault", 2)
  
  gles_ltp <- 
    haven::read_dta(here("data/raw/gles/LongTermPanel/ZA5770_v1-0-0.dta")) %>% 
    pivot_longer(
      cols = jdatum:nwkrnr3,
      names_to = c("Erhebung", ".value"),
      names_pattern = "(.)(.*)"
    )
  
  ## clean migration and readership variable
  gles_ltp <- 
    gles_ltp %>% 
    mutate(
      `285a_clean` = ifelse(`285a` < 0, NA, `285a` - 1),
      `174b_clean` = ifelse(`174b` < 0, NA, `174b` - 6),
      date_clean = 
        as.Date(
          case_when(
            Erhebung == "j" ~ "2013-12-23",
            Erhebung == "k" ~ "2014-12-11",
            Erhebung == "l" ~ "2016-01-07",
            Erhebung == "m" ~ "2016-12-15",
            Erhebung == "n" ~ "2017-12-08",
            ),
          format = "%Y-%m-%d"
          )
    )
  
  ## define treatment group
  gles_ltp <- 
    gles_ltp %>% 
    filter(Erhebung == "l") %>% 
    mutate(bild_reader = `285a_clean` > 0) %>% 
    select(lfdn, bild_reader) %>% 
    right_join(gles_ltp, by = "lfdn")
  
  hist <- 
    gles_ltp %>% 
    filter(Erhebung == "l" & !is.na(bild_reader)) %>% 
    ggplot(aes(bild_reader, fill = bild_reader)) +
    geom_bar() + 
    theme_minimal() +
    xlab("") + ylab("N") +
    guides(fill = "none", x = "none") +
    scale_fill_manual(values = color_palette) 
  
  ## plot
  gles_ltp %>% 
    filter(!is.na(bild_reader),
           Erhebung != "k") %>% 
    group_by(bild_reader, Erhebung) %>% 
    summarise(
      imm_att = mean(`174b_clean`, na.rm = T),
      imm_att_sd = sd(`174b_clean`, na.rm = T),
      date_clean = max(date_clean, na.rm = T),
      N = n()
    ) %>% 
    mutate(
      imm_att_lower = imm_att + qt(.025, df = N-1) * (imm_att_sd/sqrt(N)),
      imm_att_upper = imm_att + qt(.975, df = N-1) * (imm_att_sd/sqrt(N))
    ) %>% 
    ggplot(aes(date_clean, imm_att, 
               ymin = imm_att_lower, ymax = imm_att_upper,
               col = bild_reader, shape = bild_reader)) +
    geom_vline(xintercept = as.Date("2017-02-01"), lty = 2, col = "red") +
    geom_pointrange() +
    geom_line() +
    theme_minimal() +
    scale_shape_discrete(name = "Bild Readership", 
                       labels = c("No", "Yes")) +
    scale_color_manual(name = "Bild Readership", 
                       labels = c("No", "Yes"),
                       values = color_palette) +
    scale_fill_manual(values = color_palette) +
    guides(lty = "none", fill = "none") +
    ggtitle("Immigration Attitude by Bild Readership", "GLES Longterm Panel") +
    ylab("< More liberal | more restrictive >") + xlab("") +
    hist + 
    guide_area() +
    plot_layout(guides = "collect",
                design = 
                  "
                AAAC
                AAAB
                ")
  
  
}



# Change in Migration Attitudes ####

MigChangePlot <- function(return_table = F, 
                          line_plot = T,
                          color_palette = MetBrewer::met.brewer("Archambault", 2)) {
  
  ## load GLES LTP data, wide to long
  gles_ltp <- 
    haven::read_dta(here("data/raw/gles/LongTermPanel/ZA5770_v1-0-0.dta")) %>% 
    pivot_longer(
      cols = jdatum:nwkrnr3,
      names_to = c("Erhebung", ".value"),
      names_pattern = "(.)(.*)"
    )
  
  ## clean migration and readership variable
  gles_ltp <- 
    gles_ltp %>% 
    mutate(
      `285a_clean` = ifelse(`285a` < 0, NA, `285a` - 1),
      `174b_clean` = ifelse(`174b` < 0, NA, `174b` - 6),
      date_clean = 
        as.Date(
          case_when(
            Erhebung == "j" ~ "2013-12-23",
            Erhebung == "k" ~ "2014-12-11",
            Erhebung == "l" ~ "2016-01-07",
            Erhebung == "m" ~ "2016-12-15",
            Erhebung == "n" ~ "2017-12-08",
          ),
          format = "%Y-%m-%d"
        )
    )
  
  
  ## estimate attitude change since last wave
  gles_ltp <- 
    gles_ltp %>% 
    select(lfdn, date_clean, Erhebung, `174b_clean`) %>% 
    pivot_wider(lfdn, names_from = Erhebung, values_from = `174b_clean`, names_prefix = "mig_") %>% 
    mutate(
      change_m_1 = abs(mig_m - mig_l),
      change_n_1 = abs(mig_n - mig_m),
      change_l_2 = abs(mig_l - mig_j),
      change_n_2 = abs(mig_n - mig_l)
    ) %>% 
    pivot_longer(
      cols = change_m_1:change_n_2,
      names_to = c(".value", "Erhebung", "Distance"),
      names_pattern = "(.*)_(.)_(.)"
    ) %>% 
    right_join(gles_ltp, by = c("lfdn", "Erhebung"))
  
  
  if(!return_table){
    
    if (line_plot){
      
      gles_ltp %>% 
        filter(!Erhebung %in% c("j", "k")) %>% 
        mutate(change_label = 
                 case_when(
                   Erhebung == "l" & Distance == 2 ~ "2014-2016",
                   Erhebung == "n" & Distance == 2 ~ "2016-2018",
                   Erhebung == "m" & Distance == 1 ~ "2016-2017",
                   Erhebung == "n" & Distance == 1 ~ "2017-2018")
        ) %>% 
        group_by(change_label, Distance) %>% 
        summarise(
          
          change_mean = mean(change, na.rm = T),
          change_sd = sd(change, na.rm = T),
          N = n()
          
        ) %>% 
        mutate(
          change_lower = change_mean + qt(.025, df = N-1) * (change_sd/sqrt(N)),
          change_upper = change_mean + qt(.975, df = N-1) * (change_sd/sqrt(N))
        ) %>% 
        ggplot(aes(x = change_label, 
                   y = change_mean, ymin = change_lower, ymax = change_upper,
                   group = Distance)) +
        geom_line() + 
        geom_pointrange() +
        scale_fill_manual(values = color_palette) +
        ylab("Change in immigration attitude") +
        xlab("") +
        facet_wrap(~Distance == 1, scales = "free_x") +
        theme_minimal() + 
        # removes facet labels
        theme(
          strip.background = element_blank(),
          strip.text.x = element_blank()
        ) %>% 
        return()
    
    }else{
      
      ## plot
      gles_ltp %>% 
        filter(!Erhebung %in% c("j", "k")) %>% 
        mutate(change_label = 
                 case_when(
                   Erhebung == "l" & Distance == 2 ~ "2014-2016",
                   Erhebung == "n" & Distance == 2 ~ "2016-2018",
                   Erhebung == "m" & Distance == 1 ~ "2016-2017",
                   Erhebung == "n" & Distance == 1 ~ "2017-2018")
        ) %>% 
        ggplot(aes(x = change, fill = Erhebung == "n")) +
        geom_histogram(aes(y = after_stat(count/ sum(count))), binwidth = 1, alpha = 0.5) +
        geom_boxplot(width = 0.05, outlier.alpha = 0, notch = T, ) +
        scale_fill_manual(values = color_palette) +
        xlab("Change in immigration attitude") +
        scale_x_continuous(minor_breaks = seq(0, 10, 1),
                           breaks = seq(0, 10, 2)) +
        ylab("Relative frequency") +
        theme_minimal() +
        theme(legend.position = "None") +
        facet_grid(Erhebung=="n"~Distance) %>% 
        return()
    
    }
    
  
  }else{
    
    # table
    gles_table <- 
      gles_ltp %>% 
      filter(!Erhebung %in% c("j", "k")) %>% 
      mutate(change_label = 
               case_when(
                 Erhebung == "l" & Distance == 2 ~ "2014-2016",
                 Erhebung == "n" & Distance == 2 ~ "2016-2018",
                 Erhebung == "m" & Distance == 1 ~ "2016-2017",
                 Erhebung == "n" & Distance == 1 ~ "2017-2018")
      ) %>% 
      group_by(change_label) %>% 
      summarise(
        
        change_mean = mean(change, na.rm = T),
        change_sd = sd(change, na.rm = T),
        N = n()
        
      ) %>% 
      mutate(
        change_lower = change_mean + qt(.025, df = N-1) * (change_sd/sqrt(N)),
        change_upper = change_mean + qt(.975, df = N-1) * (change_sd/sqrt(N))
      )
    
    rownames(gles_table) <- gles_table$change_label
    
    return(gles_table)
    
  }
    
  
  
}

# DiD estimates in Longterm Panel ####

LTPDiD <- function(return_table = F,
                   treatment_timing = "2017-02-01",
                   boundary_share = 0.25,
                   theoretical_effect_size = 1,
                   size = 2) {
  
  ## load GLES LTP data, wide to long
  gles_ltp <- 
    haven::read_dta(here("data/raw/gles/LongTermPanel/ZA5770_v1-0-0.dta")) %>% 
    pivot_longer(
      cols = jdatum:nwkrnr3,
      names_to = c("Erhebung", ".value"),
      names_pattern = "(.)(.*)"
    )
  
  ## clean migration and readership variable
  gles_ltp <- 
    gles_ltp %>% 
    mutate(
      `285a_clean` = ifelse(`285a` < 0, NA, `285a` - 1),
      `174b_clean` = ifelse(`174b` < 0, NA, `174b` - 6),
      date_clean = 
        as.Date(
          case_when(
            Erhebung == "j" ~ "2013-12-23",
            Erhebung == "k" ~ "2014-12-11",
            Erhebung == "l" ~ "2016-01-07",
            Erhebung == "m" ~ "2016-12-15",
            Erhebung == "n" ~ "2017-12-08",
          ),
          format = "%Y-%m-%d"
        )
    )
  
  ## define treatment group and timing (still need to filter out ever-readers)
  gles_ltp <- 
    gles_ltp %>% 
    filter(Erhebung == "l") %>% 
    mutate(bild_reader = `285a_clean` > 0) %>% # misdefined - exclude ever-readers
    select(lfdn, bild_reader) %>% 
    right_join(gles_ltp, by = "lfdn") %>% 
    mutate(
      post = date_clean >= as.Date(treatment_timing),
      dv = `174b_clean`,
      Wave = 
        case_when(
          Erhebung == "j" ~ "a",
          Erhebung == "k" ~ "b",
          Erhebung == "l" ~ "c",
          Erhebung == "m" ~ "d",
          Erhebung == "n" ~ "d" # post-vs prewave as relevant comparison (only one postwave)
        ),
      ID = lfdn
    )
  
  
  ## model
  single_model <- 
    fixest::feglm(
    
      dv ~ post*bild_reader | Wave + ID,
      data = gles_ltp,
      cluster = "lfdn"
      
    )
  
  
  ## plot
  if(!return_table){
    modelsummary::modelplot(
        list(dv_name = single_model),
        coef_map = list("postTRUE:bild_readerTRUE" = ""),
        facet = T
      ) +
        geom_vline(xintercept = 0, lty = 1, col = "red", size = size) +
        geom_vline(xintercept = boundary_share*theoretical_effect_size, 
                   lty = 3, col = "black", size = size) +
        geom_vline(xintercept = boundary_share*-1*theoretical_effect_size, 
                   lty = 3, col = "black", size = size) +
        geom_vline(xintercept = theoretical_effect_size, 
                   lty = 2, col = "black", size = size) +
        geom_vline(xintercept = -1*theoretical_effect_size, 
                   lty = 2, col = "black", size = size) %>% 
      return()
  
  }else{
    
    ## return table
    return(single_model)
    
  }
  
}



# Variable Descriptives ####

GroupedDistPlot <- function(variable = NULL,
                            header = "") {
  
  if(is.null(variable)){stop("Must provde variable name")}
  
  gles_p_long <- fread(here('data/raw/gles/Panel/long_cleaned.csv'))
  
  gles_p_long[["dv"]] <- gles_p_long[[variable]]
  
  p_upper <- 
    gles_p_long %>% 
    filter(treat, !is.na(dv)) %>% 
    ggplot(aes(dv)) +
    geom_bar(aes(y = (..count..)/sum(..count..)),
             fill = MetBrewer::met.brewer("Archambault")[4]) +
    geom_vline(xintercept = 
                 gles_p_long %>% 
                 filter(treat, !is.na(dv)) %>% 
                 summarise(dv = mean(dv, na.rm = T)) %>% 
                 .[1,]) +
    theme_minimal() +
    xlab("") + ylab("Share") +
    ggtitle(header, "Bild Readers")
  
  p_lower <- 
    gles_p_long %>% 
    filter(!treat, !is.na(dv)) %>% 
    ggplot(aes(dv)) +
    geom_bar(aes(y = (..count..)/sum(..count..)),
             fill = MetBrewer::met.brewer("Archambault")[1]) +
    geom_vline(xintercept = 
                 gles_p_long %>% 
                 filter(!treat, !is.na(dv)) %>% 
                 summarise(dv = mean(dv, na.rm = T)) %>% 
                 .[1,]) +
    theme_minimal() +
    xlab("") + ylab("Share") +
    ggtitle("", "Never Bild Readers")
  
  return(p_upper / p_lower)
  
}

