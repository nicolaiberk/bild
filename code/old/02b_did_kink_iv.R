# foreach test


# 0. Setup ####
library(tidyverse)
library(dplyr)
library(here)
library(data.table)
library(here)
library(lubridate)
library(glue)
library(doParallel)
library(tcltk)

# 1. load ####

## get relevant waves and dates
gles_p_long <- 
  fread(here('data/gles/Panel/long_cleaned.csv')) %>% 
  as.data.frame()

wavelist <- 
  gles_p_long %>% 
  select(wave) %>% 
  filter(wave != 'a1', wave != 'a2') %>% 
  unique() %>% 
  unlist() %>% 
  as.numeric() %>% 
  sort()

datelist_max <- c()
datelist_min <- c()
for (wave_treat in wavelist){
  
  datelist_max <- 
    c(datelist_max,
      gles_p_long %>% 
        filter(wave == wave_treat) %>% 
        select(date_clean) %>% 
        unlist() %>% 
        max(na.rm = T)
    )
  datelist_min <- 
    c(datelist_min,
      gles_p_long %>% 
        filter(wave == wave_treat) %>% 
        select(date_clean) %>% 
        unlist() %>% 
        min(na.rm = T)
    )
}

datelist_max <- 
  datelist_max %>% 
  as.Date(origin = "1970-01-01")

datelist_min <- 
  datelist_min %>% 
  as.Date(origin = "1970-01-01")

survey_dates <- 
  data.frame(
    wave = wavelist,
    date_max = datelist_max,
    date_min = datelist_min
  )

rm(gles_p_long)

## load media data
load(here('data/media_daily_2021-11-19.Rdata'))

merged_media %>% 
  ggplot(aes(x = date_new, fill = paper)) + 
  geom_histogram()

## issue with post 2020 articles -> drop
merged_media <- 
  merged_media %>% 
  filter(date_new < as.Date('2020-01-01'))


# 2. transform ####

## add relevant treatment waves for did
merged_media$pre_wave_all <- NA # upcoming survey wave
merged_media$pre_wave_field <- NA # upcoming survey wave
merged_media$pre_wave_2w <- NA # upcoming survey wave
merged_media$pre_wave_1m <- NA # upcoming survey wave
merged_media$pre_wave_6m <- NA # upcoming survey wave

## calculate topic prevalence in between survey waves
for (wave_id in 1:nrow(survey_dates)){
  
  if (wave_id == 1){
    pre_wave_end <- as.Date('2016-01-01')
  }else{
    pre_wave_end <- survey_dates$date_max[wave_id-1]
  }
  
  wave_end   <- survey_dates$date_max[wave_id] # define last survey date within wave
  wave_start <- survey_dates$date_min[wave_id] # define first survey date within wave
  
  lower_all   <- pre_wave_end
  lower_field <- wave_start
  lower_2w    <- max((wave_start - weeks(2)),  pre_wave_end)
  lower_1m    <- max((wave_start - months(1)), pre_wave_end)
  lower_6m    <- max((wave_start - months(6)), pre_wave_end)
  
  wave <- survey_dates$wave[wave_id]
  
  merged_media <- merged_media %>% 
    mutate(is.wave = (date_new > lower_all & (date_new <= wave_end)),
           pre_wave_all = ifelse(is.wave, wave, pre_wave_all)) %>%
    mutate(is.wave = (date_new > lower_field & (date_new <= wave_end)),
           pre_wave_field = ifelse(is.wave, wave, pre_wave_field)) %>%  
    mutate(is.wave = (date_new > lower_2w & (date_new <= wave_end)),
           pre_wave_2w = ifelse(is.wave, wave, pre_wave_2w)) %>% 
    mutate(is.wave = (date_new > lower_1m & (date_new <= wave_end)),
           pre_wave_1m = ifelse(is.wave, wave, pre_wave_1m)) %>% 
    mutate(is.wave = (date_new > lower_6m & (date_new <= wave_end)),
           pre_wave_6m = ifelse(is.wave, wave, pre_wave_6m))
  
}

n.cores <- detectCores() - 2

## create the cluster
my.cluster <- parallel::makeCluster(
  n.cores,
  type = "PSOCK"
)

## check cluster definition
print(my.cluster)


#register it to be used by %dopar%
registerDoParallel(cl = my.cluster)


did_estimates <- # tot about 94k
  foreach(topic = colnames(merged_media)[c(3, 5, 13:72, 148:207)], .inorder = F, .combine = 'rbind', .packages = c('dplyr', 'tcltk')) %dopar% {
    
    pb <- tkProgressBar("DiD estimation progress",
                        paste0('Topic: ', topic, '...'),
                        min=1,
                        max=720)
    i <- 0

    .GlobalEnv$merged_media <- merged_media
    
    print(paste("Estimating specifications for topic:", topic))
    
    ## standardise DV
    merged_media$topic <- scale(merged_media[, topic], center = T, scale = T)
    
    did_temp <- 
      data.frame(
        est         = NA,
        lower       = NA,
        upper       = NA,
        cutoff      = NA,
        wave_id     = NA,
        reference   = NA,
        paper_treat = NA,
        lag         = NA,
        topic       = NA
      )
    

    for (lag in c("all", "field", "2w", "1m", "6m")){
      
      cat("\tLag: ", lag)
      
      ## define relevant treatment waves
      if (lag == "all"){
        merged_media$wave <- merged_media$pre_wave_all
      } else if (lag == "field"){
        merged_media$wave <- merged_media$pre_wave_field
      } else if (lag == "2w"){
        merged_media$wave <- merged_media$pre_wave_2w
      } else if (lag == "1m"){
        merged_media$wave <- merged_media$pre_wave_1m
      } else if (lag == "6m"){
        merged_media$wave <- merged_media$pre_wave_6m
      }
      
      for (wave_id in 
           merged_media %>% 
           filter(!is.na(wave)) %>% 
           dplyr::select(wave) %>% 
           unique() %>% 
           unlist() %>% 
           .[-1]) {
        
      ## define post var
      merged_media <- merged_media %>%
        mutate(post = ifelse(wave == wave_id, T, NA)) %>%
        mutate(post = ifelse(wave == (wave_id-1), F, post))
      
      ## subset to get rid of missings
      reg_dta <- 
        merged_media %>% 
        filter(!is.na(post), !is.na(paper), !is.na(topic))
      
        for (paper_treat in unique(merged_media$paper)) {
          for (reference in c("Pooled", "welt")){
            
            
    
            if (reference == paper_treat){
              
              i <- i + 1
              setTkProgressBar(pb, i)
              
              next # cant estimate DiD in reference to self
            }
            
            if (reference != 'Pooled'){
              fin_dta <- reg_dta %>%
                filter(paper %in% c(reference, paper_treat))
            }else{
              fin_dta <- reg_dta
            }
            
            fin_dta <- fin_dta %>%
              mutate(paper_reader = paper == paper_treat)
            
            # subset to get rid of missings
            fin_dta <- 
              fin_dta %>% 
              filter(!is.na(post), !is.na(paper_reader), !is.na(topic))
            
            # ensure all conditions are observable
            if (table(fin_dta$paper_reader, fin_dta$post) %>% min() > 0 &
                (table(fin_dta$paper_reader, fin_dta$post) %>% dim() %>% min() == 2)){
              
              ## estimate model binary treatment
              sum_lm <- lm(topic ~ post*paper_reader, data = fin_dta)
              
              est <- sum_lm$coefficients[["postTRUE:paper_readerTRUE"]]
              lower <- confint(sum_lm)["postTRUE:paper_readerTRUE", 1]
              upper <- confint(sum_lm)["postTRUE:paper_readerTRUE", 2]
              
              did_temp <- 
                did_temp %>% 
                rbind(
                  data.frame(
                    est = est,
                    lower = lower,
                    upper = upper,
                    cutoff = min(fin_dta$date_new[fin_dta$post], na.rm = T),
                    wave_id = wave_id, reference = reference, paper_treat = paper_treat,
                    lag = lag, topic = topic
                  )
                )
              
              i <- i + 1
              setTkProgressBar(pb, i, label = glue::glue('Current topic: {topic}; paper: {paper_treat}'))
                
            }else{
              
              i <- i + 1
              setTkProgressBar(pb, i, label = glue::glue('Current topic: {topic}; paper: {paper_treat} (skipped)'))
              
              next
            }
          }
        }
      }
      
    }
    
    
    close(pb)
    return(did_temp)
    
}

        
datum <- Sys.Date()
save(did_estimates, file = here(paste0("data/did_media_", datum, ".Rdata")))
rm(did_estimates)
        
print('Done.')



print("Estimating kink ;) ...")


kink_estimates <- # tot about 120k
  foreach(topic = colnames(merged_media)[c(3, 5, 13:72, 148:207)], .inorder = F, .combine = 'rbind', .packages = c('dplyr', 'rdrobust')) %dopar% {
  
    .GlobalEnv$merged_media <- merged_media
    
    pb <- tkProgressBar("Kink estimation progress", 
                        paste('Kink estimation', topic, 'in progress...'), 
                        min = 1, 
                        max = 1008)
    
    i <- 0
    
    merged_media$topic <- scale(merged_media[, topic], center = T, scale = T)
    
    
    kink_temp <- 
      data.frame(
        est    = NA,
        lower  = NA,
        upper  = NA,
        model  = NA,
        paper  = NA,
        topic  = NA,
        cutoff = NA
      )
    
    for (paper in unique(merged_media$paper)) {
      
      reg_dta <- 
        merged_media %>% 
        filter(paper == paper)
      
      ## subset to get rid of missings
      fin_dta <- 
        reg_dta %>% 
        filter(!is.na(date_new), !is.na(topic))
      
      for (cutoff in
            merged_media %>% 
            filter(date_new < as.Date("2020-01-01")) %>%
            filter(date_new >= as.Date("2013-02-01")) %>%
            select(date_new) %>%
            .[[1]] %>% 
            lubridate::floor_date('months') %>% 
            unique()
           ) {
        
        ## generate numeric forcing variable
        fin_dta$forcing <- as.numeric(fin_dta$date_new - cutoff)
      
        kink_model <- rdrobust(y = fin_dta$topic, x = fin_dta$forcing, deriv = 1)

        for (model in c('Conventional', 'Robust')){
          
          kink_temp <- 
            kink_temp %>% 
            rbind(
              data.frame(
                est   = kink_model$coef[model, ],
                lower = kink_model$ci[model, 1],
                upper = kink_model$ci[model, 2],
                model = model,
                paper = paper,
                topic = topic,
                cutoff = cutoff
              )
            )
          
          i <- i + 1
          setTkProgressBar(pb, i, label = glue::glue('Current topic: {topic}; paper: {paper}'))
          
        }
      }  
    }
    
    close(pb)
    return(kink_temp)
    
  }

stopCluster(cl = my.cluster)

save(kink_estimates, file = here(paste0("data/kink_media_", datum, ".Rdata")))

print("Done.")
