# ______________________________________________
# Bild
# Goal: Functions for paper
# ______________________________________________
# Date:  Thu Apr 21 08:37:00 2022
# Author: Nicolai Berk
#  R version 4.1.1 (2021-08-10)
# ______________________________________________


# Setup ####
library(tidyverse)
library(dplyr)
library(here)
library(data.table)



DiD_direct <- function(dta, size = 1, col = "black") {
  
  
  
  # model 1: direct effect
  ate <- 
    feglm(dv ~ post * treat, 
          cluster = c("lfdn"), 
          data = dta)
  
  points <- ate$coefficients
  ests <- cbind(points, confint(ate))
  
  plot_did_ate <- 
    ests %>%
    filter(row.names(.) == "postTRUE:treatTRUE") %>% 
    ggplot(aes(y = row.names(.), x = points, xmin = `2.5 %`, xmax = `97.5 %`)) +
    geom_pointrange(size = size, col = col) +
    geom_vline(xintercept = 0, col = "red", lty = 2, size = size) +
    ylab("") + xlab("Estimate") +
    xlim(-0.1, 0.3) + 
    scale_y_discrete(labels = rev(c("DiD"))) +
    ggtitle("Average treatment effect") 
  
  
  return(plot_did_ate)
  
  

  
}


DiD_inter <- function(dta, size = 1, col = "black") {
  
  
  inter <- 
    feglm(dv ~ post * treat * init_mig, 
          cluster = c("lfdn"), 
          data = dta)
  
  points <- inter$coefficients
  ests <- cbind(points, confint(inter))
  
  plot_did_inter <- 
    ests %>% 
    filter(row.names(.) %in% c("postTRUE:treatTRUE", "postTRUE:treatTRUE:init_mig")) %>% 
    mutate(name = fct_reorder(row.names(.), desc(row.names(.)))) %>% 
    ggplot(aes(y = name, x = points, xmin = `2.5 %`, xmax = `97.5 %`)) +
    geom_pointrange(col = col, size = size) +
    geom_vline(xintercept = 0, col = "red", lty = 2, size = size) +
    ylab("") + xlab("Estimate") +
    ggtitle("DiD-estimate with interaction") +
    xlim(-0.1, 0.3) + 
    scale_y_discrete(labels = rev(c("DiD", "DiD X Initial Opinion"))) +
    ggtitle("Heterogenous treatment effects")
  
}


DiD_groups <- function(dta) {
  
  
  did_groups <- 
    dta %>% 
    filter(!is.na(treat), !is.na(init_mig)) %>%
    group_by(treat, post, init_mig) %>% 
    summarise(
      dv_point = mean(dv, na.rm = T)
    ) %>% 
    filter(post) %>% 
    pivot_wider(
      names_from = treat,
      values_from = dv_point
    ) %>% 
    mutate(difference = `TRUE` - `FALSE`) %>% 
    ggplot(aes(x = init_mig, y = difference)) +
    geom_col() + 
    xlab("") + ylab("") +
    ggtitle("Effect Size by Initial Opinion")
  
  return(did_groups)
  
}

bs_inter_vis <- function(dta, cond = "init_mig", axis_label = "Initial migration attitude", DiD = T, size = 1, col = "red",
                   path = 'C:/Users/nicol/Dropbox/PhD/Papers/Bild/github/bild/data/did_bootstraps.RData') {
  
  data_name <- load(path)
  
  dta$condition <- dta[[cond]]
  
  inter <- 
    ifelse(
      test = DiD,
      yes = feglm(dv ~ post * treat * condition, 
            cluster = c("lfdn"), 
            data = dta),
      no = feglm(dv ~ crime_share * condition | wave + lfdn, 
            cluster = c("lfdn"), 
            data = dta)
    )
  
  points <- inter$coefficients
  ests <- cbind(points, confint(inter))
  
  
  direct <- coef(inter)[ifelse(DiD, "postTRUE:treatTRUE", "crime_share")]
  interact <- coef(inter)[paste0(ifelse(DiD, "postTRUE:treatTRUE", "crime_share"), ":condition")]
  
  did_bootstrap <-
    ggplot() +
    geom_histogram(data = dta %>% filter(wave == "1"), mapping = aes(x = condition, after_stat(density)), alpha = 0.4, bins = 8) +
    geom_abline(data = get(data_name), aes(slope = IA, intercept = DiD, group = boots), alpha= 0.05) +
    geom_abline(slope = interact, intercept = direct, col = col, size = size) +
    geom_hline(yintercept = 0, col = "black") +
    scale_x_continuous(limits=c(min(dta$condition),max(dta$condition))) +
    # scale_y_continuous(limits=c(min, max)) +
    ggtitle(paste("Effect across", axis_label), "1000 Bootstrap Estimates") +
    ylab("Estimate") + xlab(axis_label)
  
  return(bootstrap_vis)
  
}



