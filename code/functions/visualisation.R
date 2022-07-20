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


# Migration Salience ####

MigSaliencePlot <- function(aggregation = "months"){
  
  mig_salience_plot <- 
    fread(here("data/processed/bert_crime_daily.csv")) %>% 
    select(date_clean, paper, mig_share) %>% 
    mutate(date_new = floor_date(date_clean, aggregation)) %>% 
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
                               palette = c("red", "black"),
                               maxdate = as.Date("2020-01-01"),
                               mindate = as.Date("2015-01-01"),
                               aggregation = "quarter",
                               dpa_corrected = T) {
  
  ## load BERT data
  bert_ests_raw <- 
    fread(here('data/processed/bert_crime_clean.csv')) 
  
  if(dpa_corrected){
    
    # filter Bild news based on DPA to avoid spurious association
    bert_ests_raw <- 
      bert_ests_raw %>% 
      filter(paper != "Bild" | !dpa)
    
    
  }
  
    bert_ests <- 
      bert_ests_raw %>% 
      mutate(month = lubridate::floor_date(date_clean, aggregation)) %>% 
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
      geom_line(size = size) +
      geom_rug(data = survey_dates, aes(x = date), sides = 't', inherit.aes = F) +
      geom_vline(xintercept = as.Date("2017-02-01"), lty = 2, col = "red", size = size) +
      ggtitle("Quarterly Share of Migration Content Devoted to Crime Frames", 
              paste0("Bild vs. other major daily newspapers, 2016-", lubridate::year(maxdate))) + 
      xlab("Date") + 
      ylab("Share of Migration Articles Containing Crime Frame") +
      theme_minimal() +
      theme(legend.position = "none") +
      scale_color_manual(values = palette)
    
    return(trendplot)
  
}

TreatmentDiDPlot <- function(size = 1, 
                             palette = c("red", "black"),
                             maxdate = as.Date("2020-01-01"),
                             mindate = as.Date("2015-01-01"),
                             dpa_corrected = T){
  
  
  ## load BERT data
  bert_ests_raw <- 
    fread(here('data/processed/bert_crime_clean.csv')) %>% 
    filter(paper != "Spiegel", # as Spiegel not part of the analysis
           date_clean < maxdate,
           date_clean >= mindate) %>% 
    group_by(paper, date_clean) %>% 
    summarise(dv = mean(crime_label),
              dpa = mean(dpa)) %>%  
    mutate(post = ifelse(date_clean >= as.Date("2017-02-01"), T, F),
           quarter_id = floor_date(date_clean, "quarter"))
    
  
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

DescPlot <- function(placebo = F, 
                    exclude_pretreated = F, 
                    legend_position = "right"){
  
  gles_p_long <- fread(here('data/raw/gles/Panel/long_cleaned.csv'))
  
  postdate <- as.Date("2017-02-01")
  title <- "Mean migration attitude post-Reichelt takeover"
  subtitle <- "by readership and initial migration attitude"
  
  if (placebo & !exclude_pretreated){
    
    gles_p_long$treat <- gles_p_long$treat_placebo
    gles_p_long$post  <- gles_p_long$post_placebo
    gles_p_long$init_mig_groups <- gles_p_long$init_mig_groups_placebo
    title <- "Mean migration attitudes post Placebo"
    subtitle <- "by readership and initial migration attitude"
    postdate <- as.Date("2018-01-01")
    
  }else if (placebo & exclude_pretreated){
    
    gles_p_long$treat <- gles_p_long$treat_placebo_np
    gles_p_long$post  <- gles_p_long$post_placebo
    gles_p_long$init_mig_groups <- gles_p_long$init_mig_groups_placebo
    title <- "Placebo estimate post BTW"
    subtitle <- "excluding Bild readers W1, by readership and initial migration attitude"
    postdate <- as.Date("2018-01-01")
    
  }
  
  gles_p_long %>% 
    filter(!is.na(treat), init_mig_groups != "", !is.na(`1130_clean`)) %>%
    group_by(treat, post, init_mig_groups, wave) %>% 
    summarise(
      dv_mean = mean(`1130_clean`),
      date_min = min(date_clean)
    ) %>% 
    ggplot(aes(x = date_min, y = dv_mean, 
               col = treat, shape = treat, 
               lty = init_mig_groups)) +
    geom_line() + geom_point() +
    geom_vline(xintercept = postdate, lty = 2, col = "red") +
    xlab("") + ylab("Immigration Attitude") +
    scale_color_discrete(name = "Group", labels = c("Not Bild-reader", "Bild reader")) +
    scale_shape_discrete(name = "Group", labels = c("Not Bild-reader", "Bild reader")) +
    scale_linetype_discrete(name = "Pre-treatment\nImmigration Att.") +
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
                       boundary_share = 1,
                       theoretical_effect_size = 0.1) {
  
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
                      select(dv, post, treat, lfdn, wave_ctrl)
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
                      select(dv, post, treat, lfdn, wave_ctrl))
    
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
                      select(dv, post, treat, lfdn, wave_ctrl))
    
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
                      select(dv, post, treat, lfdn, wave_ctrl))
    
    modelsummary::modelplot(
      list("AfD thermometer" = afd_model,
           "MIP: Migration" = mip_model,
           "Integration Attitude" = int_att_model,
           "Migration Attitude" = mig_att_model),
      coef_map = list("postTRUE:treatTRUE" = ""),
      facet = T, 
      ) +
      geom_vline(xintercept = 0, lty = 1, col = "red", size = size) +
      # geom_vline(xintercept = theoretical_effect_size*boundary_share, lty = 3, col = "orange", size = size) +
      # geom_vline(xintercept = -1*theoretical_effect_size*boundary_share, lty = 3, col = "orange", size = size) +
      geom_vline(xintercept = theoretical_effect_size, lty = 3, col = "black", size = size) +
      geom_vline(xintercept = -1*theoretical_effect_size, lty = 3, col = "black", size = size)

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
                      select(dv, post, treat, lfdn, wave_ctrl))
    
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
                             boundary_share = 1,
                             theoretical_effect_size = 0.1) {
  
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
                    data = temp)
    
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
  
  ests %>% 
    ggplot(aes(x = date, y= point, ymin = upper, ymax = lower)) +
    geom_hline(yintercept = 0, col = "red", size = size) +
    geom_hline(yintercept = boundary_share*theoretical_effect_size,
               lty = 3, col = "black", size = size) +
    geom_hline(yintercept = boundary_share*-1*theoretical_effect_size, 
               lty = 3, col = "black", size = size) +
    geom_hline(yintercept = theoretical_effect_size,
               lty = 2, col = "black", size = size) +
    geom_hline(yintercept = -1*theoretical_effect_size,
               lty = 2, col = "black", size = size) +
    geom_vline(xintercept = as.Date(postdate), lty = 2, col = "red", size = size) +
    geom_vline(xintercept = as.Date("2017-06-01"), lty = 2, col = "orange", size = size) +
    geom_pointrange(size = size) +
    xlab("Date") + ylab("Effect") +
    theme_minimal() %>% 
    return()
}

# Instrumental Variable Regression ####
# following Cunningham's Mixtape, p. 354

iv_model <- function(dv_name, iv = T, dpa_corrected = T){
  
  if (!exists("dv_name")){
    stop("Must define dependent variable (e.g. `dv_name = '1130_clean'`)")
  }
  
  merged_data[["dv"]] <- merged_data[[dv_name]]
  
  if(iv){
    
    if (dpa_corrected){
      
      first_stage <- feglm(crime_label_share ~ scale(mig_share) + post*treat | Wave + ID, 
                           data = merged_data)
      
      merged_data$Y2_hat <- predict(first_stage, 
                                    merged_data[, c("mig_share", "post", "treat", "Wave", "ID")])
      
      
      second_stage <- feglm(scale(dv) ~ scale(Y2_hat) + scale(mig_share) | Wave + ID,
                            data = merged_data)
      
    }else{
      
      first_stage <- feglm(crime_label_share ~ scale(mig_share) + scale(dpa_corrected) + post*treat | Wave + ID, 
                           data = merged_data)
      
      merged_data$Y2_hat <- predict(first_stage, 
                                    merged_data[, c("mig_share", "post", "treat", "Wave", "ID")])
      
      
      second_stage <- feglm(scale(dv) ~ scale(Y2_hat) + scale(dpa_corrected) + scale(mig_share) | Wave + ID,
                            data = merged_data)
      
    }
    
  
    return(second_stage)
    
  }else{
    
    twfe_model <- 
      feglm(scale(dv) ~ scale(crime_label_share) + scale(mig_share) + scale(dpa_corrected) | Wave + ID,
            data = merged_data)
    
    return(twfe_model)
    
  }
  
}

IVPlot <- function(dv_name = "1130_clean", 
                   dv_label = "Crime",
                   multi = F, iv = T,
                   theoretical_effect_size = 0.2,
                   size = 1,
                   dpa_corrected = T){
  
  # dpa_label <- 
  #   ifelse(
  #     dpa_corrected,
  #     "_nodpaBild",
  #     ""
  #   )
  
  # load data
  merged_data <- data.table::fread(file = here(paste0("data/processed/merged_bert.csv")))
  
  merged_data <- 
    merged_data %>%  
    filter(lag == 7)
  
  merged_data <- 
    merged_data %>% 
    mutate(mig_share = n_mig/n_tot,
           ID  = factor(lfdn),
           Wave = factor(wave))
  
  if (!multi){
  
    single_model <- iv_model(dv_name = dv_name, iv = iv, dpa_corrected = dpa_corrected)
    
    if(iv){
      
      modelsummary::modelplot(single_model,
                  coef_map = 
                      list("scale(Y2_hat)" = paste0("Exposure to/n", dv_label))
                  ) +
        geom_vline(xintercept = 0, lty = 1, col = "red", size = size) +
        geom_vline(xintercept = theoretical_effect_size, lty = 3, col = "black", size = size) +
        geom_vline(xintercept = -1*theoretical_effect_size, lty = 3, col = "black", size = size) %>% 
        return()
      
    }else{
      
      modelsummary::modelplot(single_model,
                              coef_map = list("scale(crime_label_share)" = paste0("Exposure to/n", dv_label))
                              ) +
        geom_vline(xintercept = 0, lty = 1, col = "red", size = size) +
        geom_vline(xintercept = theoretical_effect_size, lty = 3, col = "black", size = size) +
        geom_vline(xintercept = -1*theoretical_effect_size, lty = 3, col = "black", size = size) %>% 
        return()
      
    }
    
    
  }else{
    
    mig_model <- iv_model("1130_clean", iv = iv, dpa_corrected = dpa_corrected)
    int_model <- iv_model("1210_clean", iv = iv, dpa_corrected = dpa_corrected)
    afd_model <- iv_model("430i_clean", iv = iv, dpa_corrected = dpa_corrected)
    mip_model <- iv_model("mip_mig",    iv = iv, dpa_corrected = dpa_corrected)
    
    coef_map <- 
      if(iv){
        list("scale(Y2_hat)" = "")
      }else{
        list("scale(crime_label_share)" = "")
      }
    
    modelsummary::modelplot(
      list("AfD thermometer" = afd_model,
           "MIP: Migration" = mip_model,
           "Integration Attitude" = int_model,
           "Migration Attitude" = mig_model),
      coef_map = coef_map,
      facet = T, 
    ) +
      geom_vline(xintercept = 0, lty = 1, col = "red", size = size) +
      geom_vline(xintercept = theoretical_effect_size, lty = 3, col = "black", size = size) +
      geom_vline(xintercept = -1*theoretical_effect_size, lty = 3, col = "black", size = size) %>% 
      return()
    
    
  }
  
  
}


## Dependent on prior position ####

InterLinear <- function(placebo = F, exclude_pretreated = F) {
  
  gles_p_long <- fread(here('data/raw/gles/Panel/long_cleaned.csv'))
  title <- "Pre-post differences of treated and untreated"
  
  if (placebo & !exclude_pretreated){
    
    gles_p_long$treat <- gles_p_long$treat_placebo
    gles_p_long$post  <- gles_p_long$post_placebo
    gles_p_long$init_mig <- gles_p_long$init_mig_placebo
    title <- "Placebo estimate post BTW"
    
  }else if (placebo & exclude_pretreated){
    
    gles_p_long$treat <- gles_p_long$treat_placebo_np
    gles_p_long$post  <- gles_p_long$post_placebo
    gles_p_long$init_mig <- gles_p_long$init_mig_placebo
    title <- c("Placebo estimate post BTW, excluding pretreated")
    
  }
  
  diffs <- 
    gles_p_long %>% 
    filter(!is.na(treat), !is.na(init_mig)) %>%
    group_by(treat, post, init_mig) %>% 
    summarise(
      dv_point = mean(`1130_clean`, na.rm = T)
    ) %>% 
    filter(post) %>% 
    pivot_wider(
      names_from = treat,
      values_from = dv_point
    ) %>% 
    mutate(difference = `TRUE`-`FALSE`) 
  
  diffs %>% 
    ggplot(aes(x = init_mig, y = difference)) +
    geom_col() + 
    theme_minimal() + xlab("") + ylab("") +
    ylim(-.2, 1) +
    ggtitle(title, " by initial migration attitude")
  
}

## DiD Distribution ####

DiDDist <- function() {
  fread(here('data/raw/gles/Panel/long_cleaned.csv')) %>%
    filter(!is.na(treat), !is.na(post)) %>% 
    mutate(post = factor(ifelse(post, "POST", "PRE"), 
                         levels = c("PRE", "POST"))) %>% 
    ggplot(aes(x=`1130_clean`,
               fill=treat))+
    geom_histogram() +
    ylab("Immigration Attitude") +
    theme_minimal() + 
    facet_grid(treat~post, scales = "free")
}

## Pre-post correlation of migration and crime ####

### plot

MigCrimeCorPlot <- function(size = 1){
  
  cors <- 
    fread(here('data/raw/gles/Panel/long_cleaned.csv')) %>% 
    filter(!is.na(`1130_clean`), !is.na(`2880h_clean`), !is.na(treat)) %>% 
    group_by(treat, post) %>% 
    summarise(mig_crime_cor = cor(`1130_clean`, `2880h_clean`)) %>% 
    mutate(Universe = "Factual")
  
  cors <- 
    cors %>% 
    rbind(
      data.frame(
        treat = c(TRUE, TRUE),
        post = c(FALSE, TRUE),
        mig_crime_cor = c(cors[cors$treat & !cors$post, "mig_crime_cor"][[1]], 
                          cors[cors$treat & !cors$post, "mig_crime_cor"][[1]] +  
                            cors[!cors$treat & cors$post, "mig_crime_cor"][[1]] -
                                    cors[!cors$treat & !cors$post, "mig_crime_cor"][[1]]),
        Universe = "Counterfactual"
      ), 
    ) %>% 
    mutate(Universe = factor(Universe, levels = c("Factual", "Counterfactual")))
  
  
  cors %>% 
    mutate(treat_universe = paste(treat, Universe, sep = "_")) %>% 
    ggplot(aes(x = post, y = mig_crime_cor, 
               col = treat, lty = Universe, shape = treat, 
               group = treat_universe)) +
    geom_point(size = size*2) +
    geom_line(size = size) +
    theme_minimal() +
    xlab("") + ylab("Correlation") +
    # scale_color_discrete(name = "Group", labels = c("Control", "Treatment")) +
    # scale_shape_discrete(name = "Group", labels = c("Control", "Treatment")) +
    scale_x_discrete(labels = c("TRUE" = "Post", "FALSE" = "Pre"))%>% 
    return()
  
}


MigCrimeCorPlotBS <- function(size = 1){
  
  load(here("data/processed/did_bootstraps_cor.RData"))
  
  cors <- 
    bs_cor_mig_crime %>% 
    group_by(treat, post) %>% 
    summarise(lower = quantile(mig_crime_cor, 0.025),
              upper = quantile(mig_crime_cor, 0.975),
              point = mean(mig_crime_cor)) %>% 
    mutate(Universe = "Factual")
  
  cors <- 
    cors %>% 
    rbind(
      data.frame(
        treat = c(TRUE, TRUE),
        post = c(FALSE, TRUE),
        point = c(cors[cors$treat & !cors$post, "point"][[1]], 
                          cors[cors$treat & !cors$post, "point"][[1]] +  
                            cors[!cors$treat & cors$post, "point"][[1]] -
                            cors[!cors$treat & !cors$post, "point"][[1]]),
        Universe = "Counterfactual"
      ),
    ) %>% 
    mutate(Universe = factor(Universe, levels = c("Factual", "Counterfactual")))
  
  
  cors %>% 
    mutate(treat_universe = paste(treat, Universe, sep = "_")) %>% 
    ggplot(aes(x = post, 
               y = point, ymin = lower, ymax = upper, 
               col = treat, lty = Universe, shape = treat, group = treat_universe)) +
    geom_pointrange(size = size) +
    geom_line(size = size) +
    theme_minimal() +
    xlab("") + ylab("Correlation") +
    scale_color_discrete(name = "Group", labels = c("Control", "Treatment")) +
    scale_shape_discrete(name = "Group", labels = c("Control", "Treatment")) +
    scale_x_discrete(labels = c("TRUE" = "Post", "FALSE" = "Pre")) %>% 
    return()
  
}
