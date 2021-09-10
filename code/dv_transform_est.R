
# estimate DiD for each issue-paper-wave-combination

library(here)
library(dplyr)
library(tidyverse)
library(data.table)
library(haven)
library(lubridate)
library(glue)
library(fixest)

gles_p <- read_dta(here('data/gles/Panel/GLESMerge/ZA6838_allwaves_sA_v4-0-0.dta'))

## id var
gles_p$lfdn <- as.factor(gles_p$lfdn)


## date
date_vars <- colnames(gles_p)[grep(pattern = 'datetime', colnames(gles_p))]
date_waves <- str_match(date_vars, 'kp(.*)_')[,2]

### generate cleaned variables
for (i in 1:length(date_vars)){
  
  new_var <- paste0('date_w', date_waves[i])
  
  gles_p[[new_var]] <- NA
  
  gles_p <- gles_p
  
  subset <- gles_p[[date_vars[i]]] > 0 & (!is.na(gles_p[[date_vars[i]]]))
  
  gles_p[[new_var]][subset] <- gles_p[[date_vars[i]]][subset]
  
}


date_vars <- colnames(gles_p)[grep(pattern = 'date_w', colnames(gles_p))]

## extract ego-issues
ego_issues <- 
  str_match(
    string = colnames(gles_p), 
    pattern = "kp.*\\d+_2880(.*)"
  )[,2] %>% 
  unique()
ego_issues <- paste0("2880", ego_issues[!is.na(ego_issues)])

coeflist_medium <- c()
coeflist_paper <- c()
coeflist_issue <- c()
coeflist_wave  <- c()
coeflist_date <- c()
coeflist_coef_lm <- c()
coeflist_p_lm <- c()
coeflist_coef_fe <- c()
coeflist_p_fe <- c()





mediumlist <- c('1681', '1661')

for (medium in mediumlist){
  
  if (medium == '1661'){
    papers_list <- letters[1:7]
    non_reader_var <- letters[8]
  }else{
    papers_list <- letters[1:5]
    non_reader_var <- letters[6]
  }
  
  for (paper in papers_list){
    for (issue in c(ego_issues, "1500", "1090", "1130", 
                    "1290", "1210", "1140", "1220", "1300", 
                    "1411", "1250", "1260")){
      
      tempdta <- gles_p
      
      ## papervar
      papervar_vars <- colnames(tempdta)[grep(pattern = paste0(medium, paper), colnames(tempdta))]
      papervar_waves <- str_match(papervar_vars, 'kp(.*)_')[,2]
      paper_label <- attr(tempdta[[papervar_vars[1]]], "label")
      
      ### generate cleaned variables
      for (i in 1:length(papervar_vars)){
        
        new_var <- paste0('papervar_w', papervar_waves[i])
        
        tempdta[[new_var]] <- NA
        
        subset <- (tempdta[[papervar_vars[i]]] > 0) & (!is.na(tempdta[[papervar_vars[i]]]))
        
        tempdta[[new_var]][subset] <- tempdta[[papervar_vars[i]]][subset] - 1
        
      }
      
      # impute data for waves with no questions regarding readership
      # for (i in (1:13)[(!(1:13) %in% papervar_waves)]){
      #   new_var <- paste0('papervar_w', i)
      #   
      #   prev_wave <- max(papervar_waves[as.integer(papervar_waves) < i])
      #   
      #   tempdta[[new_var]] <- tempdta[[paste0("papervar_w", prev_wave)]]
      # }
      # 
      # papervar_vars <- colnames(tempdta)[grep(pattern = 'papervar', colnames(tempdta))]
      # papervar_waves <- str_match(papervar_vars, 'papervar_w(.*)')[,2]
      
      
      
      
      ## attitude
      issue_vars <- colnames(tempdta)[grep(pattern = paste0(issue, '$'), colnames(tempdta))]
      issue_waves <- str_match(issue_vars, 'kp(.*)_')[,2]
      issue_label <- attr(tempdta[[issue_vars[1]]], "label")
      
      ### generate cleaned variables
      for (i in 1:length(issue_vars)){
        
        new_var <- paste0('issue_w', issue_waves[i])
        
        tempdta[[new_var]] <- NA
        
        subset <- tempdta[[issue_vars[i]]] > 0 & (!is.na(tempdta[[issue_vars[i]]]))
        
        tempdta[[new_var]][subset] <- tempdta[[issue_vars[i]]][subset]
        
        #normalize irregardless of scale
        tempdta[[new_var]] <- tempdta[[new_var]]/max(tempdta[[new_var]], na.rm = T)
        
      }
      
      
      issue_vars <- colnames(tempdta)[grep(pattern = 'issue_', colnames(tempdta))]
      
      
      ## wide -> long
      ### paper
      gles_p_long_paper <- tempdta %>% 
        select(c(lfdn, contains("papervar_w"))) %>% 
        pivot_longer(
          cols = contains("papervar_w"))
      
      gles_p_long_paper <- gles_p_long_paper %>% 
        mutate(
          wave = str_match(name, "papervar_w(.*)")[,2],
          paper = value
        ) %>% 
        select(lfdn, wave, paper)
      
      
      ### issue
      gles_p_long_issue <- tempdta %>% 
        select(c(lfdn, contains("issue_w"))) %>% 
        pivot_longer(
          cols = c(contains("issue_w")))
      
      gles_p_long_issue <- gles_p_long_issue %>% 
        mutate(
          wave = str_match(name, "issue_w(.*)")[,2],
          issue = value
        ) %>% 
        select(lfdn, wave, issue)

      ### general reader
      reader_vars <- grep(pattern = paste0(medium, non_reader_var, '$'), colnames(tempdta), value = T)
      gles_p_long_reader <- tempdta %>%
        select(lfdn, all_of(reader_vars)) %>%
        pivot_longer(
          cols = c(contains(paste0(medium, non_reader_var))))

      gles_p_long_reader <- gles_p_long_reader %>%
        mutate(
          wave = str_match(name, paste0("kp(.*)_", medium, non_reader_var))[,2],
          reader = ifelse(value > 0, value, NA)
        ) %>%
        select(lfdn, wave, reader) %>% 
        filter(!is.na(reader))
      
      ### date
      gles_p_long_date <- tempdta %>% 
        select(c(lfdn, contains("date_w"))) %>% 
        pivot_longer(
          cols = c(contains("date_w")))
      
      gles_p_long_date <- gles_p_long_date %>% 
        mutate(
          wave = str_match(name, "date_w(.*)")[,2],
          date_new = value
        ) %>% 
        select(lfdn, wave, date_new) %>% 
        filter(date_new != "-95 nicht teilgenommen")
      
      ### merge
      gles_p_long <- merge(gles_p_long_date, gles_p_long_paper, by = c("lfdn", "wave"))
      gles_p_long <- merge(gles_p_long, gles_p_long_issue, by = c("lfdn", "wave"))
      gles_p_long <- merge(gles_p_long, gles_p_long_reader, by = c("lfdn", "wave"))
      
      gles_p_long <- gles_p_long %>%
        mutate(paper_reader = paper > 0,
               general_reader = (reader == 2)
        ) %>% 
        filter(!is.na(paper_reader)) %>% 
          mutate(reader_fin = ifelse((paper_reader & general_reader),
                                 "This paper", NA)) %>%
          mutate(reader_fin = ifelse(!paper_reader & general_reader,
                                 "Only other papers", reader_fin)) %>%
          mutate(reader_fin = ifelse(!paper_reader & !general_reader,
                                 "Doesn't read any", reader_fin))

      
      wavelist <- sort(unique(as.integer(gles_p_long$wave)))
      if (length(wavelist) > 2){
      
        # ## aggregate
        # gles_p_agg <- gles_p_long %>%
        #   group_by(wave, reader_fin) %>%
        #   summarise(
        #     issue_m = mean(issue, na.rm = T),
        #     issue_sd = sd(issue, na.rm = T),
        #     n_respondents = n(),
        #     dateagg = max(as.Date(date_new))
        #   )
        # 
        # ### CI's
        # gles_p_agg$issue_se <-  gles_p_agg$issue_sd/sqrt(gles_p_agg$n_respondents)
        # gles_p_agg$issue_low_ci <- gles_p_agg$issue_m - qt(1 - (0.05 / 2), gles_p_agg$n_respondents-1) * gles_p_agg$issue_se
        # gles_p_agg$issue_upp_ci <- gles_p_agg$issue_m + qt(1 - (0.05 / 2), gles_p_agg$n_respondents-1) * gles_p_agg$issue_se
        # 
        # 
        # ## plot
        # ggplot(gles_p_agg, aes(x = dateagg,
        #                        y = issue_m,
        #                        ymin = issue_low_ci,
        #                        ymax = issue_upp_ci,
        #                        group = reader_fin,
        #                        fill = reader_fin,
        #                        col = reader_fin
        # )) +
        #   geom_line() +
        #   geom_ribbon(alpha = 0.5) +
        #   ggtitle(paste(issue_label),
        #           paste("Condition:", paper_label))
        # ggsave(filename = here(paste0('paper/ReportThomas/paneleffectplots/panel_', medium, '_',  paper, '_', issue, '.png')))
        
        ## did-model (fe model with long data too extensive to run in loop)
        
        for (wave_id in 2:length(wavelist)){
          gles_p_long <- gles_p_long %>%
            mutate(treat = ifelse(wave == as.character(wavelist[wave_id]), 1, NA)) %>%
            mutate(treat = ifelse(wave == as.character(wavelist[(wave_id-1)]), 0, treat))
          
          ## fes dropped bc takes too long
          sum_lm <- summary(lm(issue ~ treat*paper_reader, data = gles_p_long))
          sum_fe <- summary(feglm(issue ~ treat*paper_reader | lfdn, data = gles_p_long))
          
          coeflist_medium <- c(coeflist_medium, medium)
          coeflist_paper <- c(coeflist_paper, paper)
          coeflist_issue <- c(coeflist_issue, issue)
          coeflist_wave  <- c(coeflist_wave,  wavelist[wave_id])
          coeflist_date  <- c(coeflist_date,  max(gles_p_long$date_new[gles_p_long$wave == wavelist[wave_id]]))
          coeflist_coef_fe  <- c(coeflist_coef_fe,  sum_fe$coeftable['treat:paper_readerTRUE', 1])
          coeflist_p_fe     <- c(coeflist_p_fe, sum_fe$coeftable['treat:paper_readerTRUE', 4])
          coeflist_coef_lm  <- c(coeflist_coef_lm,  sum_lm$coefficients['treat:paper_readerTRUE',][1])
          coeflist_p_lm     <- c(coeflist_p_lm, sum_lm$coefficients['treat:paper_readerTRUE', ][4])
          
        }
      }
    }
  }
}

## same for news magazines (1701)
papers_list <- letters[1:4]

for (paper in papers_list){
  for (issue in c(ego_issues, "1500", "1090", "1130", 
                  "1290", "1210", "1140", "1220", "1300", 
                  "1411", "1250", "1260")){
      
      tempdta <- gles_p
      
      ## papervar
      papervar_vars <- colnames(tempdta)[grep(pattern = paste0('1701', paper, 'c'), colnames(tempdta))]
      papervar_waves <- str_match(papervar_vars, 'kp(.*)_')[,2]
      paper_label <- attr(tempdta[[papervar_vars[1]]], "label")
      
      ### generate cleaned variables
      for (i in 1:length(papervar_vars)){
        
        new_var <- paste0('papervar_w', papervar_waves[i])
        
        tempdta[[new_var]] <- NA
        
        subset <- (tempdta[[papervar_vars[i]]] > 0) & (!is.na(tempdta[[papervar_vars[i]]]))
        
        tempdta[[new_var]][subset] <- tempdta[[papervar_vars[i]]][subset]
        
      }
      # 
      # # emulate data for waves with no questions regarding readership
      # for (i in (1:13)[(!(1:13) %in% papervar_waves)]){
      #   new_var <- paste0('papervar_w', i)
      #   
      #   prev_wave <- max(papervar_waves[as.integer(papervar_waves) < i])
      #   
      #   tempdta[[new_var]] <- tempdta[[paste0("papervar_w", prev_wave)]]
      # }
      # 
      papervar_vars <- colnames(tempdta)[grep(pattern = 'papervar', colnames(tempdta))]
      papervar_waves <- str_match(papervar_vars, 'papervar_w(.*)')[,2]
      
      
      
      
      ## attitude
      issue_vars <- colnames(tempdta)[grep(pattern = paste0(issue, '$'), colnames(tempdta))]
      issue_waves <- str_match(issue_vars, 'kp(.*)_')[,2]
      issue_label <- attr(tempdta[[issue_vars[1]]], "label")
      
      ### generate cleaned variables
      for (i in 1:length(issue_vars)){
        
        new_var <- paste0('issue_w', issue_waves[i])
        
        tempdta[[new_var]] <- NA
        
        subset <- tempdta[[issue_vars[i]]] > 0 & (!is.na(tempdta[[issue_vars[i]]]))
        
        tempdta[[new_var]][subset] <- tempdta[[issue_vars[i]]][subset]
        
        #normalize irregardless of scale
        tempdta[[new_var]] <- tempdta[[new_var]]/max(tempdta[[new_var]], na.rm = T)
        
      }
      
      
      issue_vars <- colnames(tempdta)[grep(pattern = 'issue_', colnames(tempdta))]
      
      
      ## wide -> long
      ### paper
      gles_p_long_paper <- tempdta %>% 
        select(c(lfdn, contains("papervar_w"))) %>% 
        pivot_longer(
          cols = contains("papervar_w"))
      
      gles_p_long_paper <- gles_p_long_paper %>% 
        mutate(
          wave = str_match(name, "papervar_w(.*)")[,2],
          paper = value
        ) %>% 
        select(lfdn, wave, paper)
      
      
      ### issue
      gles_p_long_issue <- tempdta %>% 
        select(c(lfdn, contains("issue_w"))) %>% 
        pivot_longer(
          cols = c(contains("issue_w")))
      
      gles_p_long_issue <- gles_p_long_issue %>% 
        mutate(
          wave = str_match(name, "issue_w(.*)")[,2],
          issue = value
        ) %>% 
        select(lfdn, wave, issue)
      
      ### date
      gles_p_long_date <- tempdta %>% 
        select(c(lfdn, contains("date_w"))) %>% 
        pivot_longer(
          cols = c(contains("date_w")))
      
      gles_p_long_date <- gles_p_long_date %>% 
        mutate(
          wave = str_match(name, "date_w(.*)")[,2],
          date_new = value
        ) %>% 
        select(lfdn, wave, date_new) %>% 
        filter(date_new != "-95 nicht teilgenommen")
      
      ### merge
      gles_p_long <- merge(gles_p_long_date, gles_p_long_paper, by = c("lfdn", "wave"))
      gles_p_long <- merge(gles_p_long, gles_p_long_issue, by = c("lfdn", "wave"))
      
      gles_p_long <- gles_p_long %>%
        mutate(paper_reader = (paper == 2)
        ) %>% 
        filter(!is.na(paper_reader))
      
      wavelist <- sort(unique(as.integer(gles_p_long$wave)))
      if (length(wavelist) > 2){
      
      
        # ## aggregate
        # gles_p_agg <- gles_p_long %>%
        #   group_by(wave, paper_reader) %>%
        #   summarise(
        #     issue_m = mean(issue, na.rm = T),
        #     issue_sd = sd(issue, na.rm = T),
        #     n_respondents = n(),
        #     dateagg = max(as.Date(date_new))
        #   )
        # 
        # ### CI's
        # gles_p_agg$issue_se <-  gles_p_agg$issue_sd/sqrt(gles_p_agg$n_respondents)
        # gles_p_agg$issue_low_ci <- gles_p_agg$issue_m - qt(1 - (0.05 / 2), gles_p_agg$n_respondents-1) * gles_p_agg$issue_se
        # gles_p_agg$issue_upp_ci <- gles_p_agg$issue_m + qt(1 - (0.05 / 2), gles_p_agg$n_respondents-1) * gles_p_agg$issue_se
        # 
        # 
        # ## plot
        # ggplot(gles_p_agg, aes(x = dateagg,
        #                        y = issue_m,
        #                        ymin = issue_low_ci,
        #                        ymax = issue_upp_ci,
        #                        group = paper_reader,
        #                        fill = paper_reader,
        #                        col = paper_reader
        # )) +
        #   geom_line() +
        #   geom_ribbon(alpha = 0.5) +
        #   ggtitle(paste(issue_label),
        #           paste("Condition:", paper_label))
        # ggsave(filename = here(paste0('paper/ReportThomas/paneleffectplots/panel_1701',  paper, '_', issue, '.png')))
        
        ## fe-did-model
        
        for (wave_id in 2:length(wavelist)){
          gles_p_long <- gles_p_long %>%
            mutate(treat = ifelse(wave == as.character(wavelist[wave_id]), 1, NA)) %>%
            mutate(treat = ifelse(wave == as.character(wavelist[(wave_id-1)]), 0, treat))
          
          
          sum_lm <- summary(lm(issue ~ treat*paper_reader, data = gles_p_long))
          sum_fe <- summary(feglm(issue ~ treat*paper_reader | lfdn, data = gles_p_long))
          
          coeflist_medium <- c(coeflist_medium, medium)
          coeflist_paper <- c(coeflist_paper, paper)
          coeflist_issue <- c(coeflist_issue, issue)
          coeflist_wave  <- c(coeflist_wave,  wavelist[wave_id])
          coeflist_date  <- c(coeflist_date,  max(gles_p_long$date_new[gles_p_long$wave == wavelist[wave_id]]))
          coeflist_coef_fe  <- c(coeflist_coef_fe,  sum_fe$coeftable['treat:paper_readerTRUE', 1])
          coeflist_p_fe     <- c(coeflist_p_fe, sum_fe$coeftable['treat:paper_readerTRUE', 4])
          coeflist_coef_lm  <- c(coeflist_coef_lm,  sum_lm$coefficients['treat:paper_readerTRUE',][1])
          coeflist_p_lm     <- c(coeflist_p_lm, sum_lm$coefficients['treat:paper_readerTRUE', ][4])
          
        }
      }
  }
}



## print overview table
coeftable <- data.frame(medium = coeflist_medium,
                        paper = coeflist_paper,
                        issue = coeflist_issue,
                        wave  = coeflist_wave,
                        date = coeflist_date,
                        coef_lm  = coeflist_coef_lm,
                        p_value_lm = coeflist_p_lm,
                        coef_fe  = coeflist_coef_fe,
                        p_value_fe = coeflist_p_fe)

write.csv(coeftable, here('paper/ReportThomas/coeftable_noimp.csv'))
