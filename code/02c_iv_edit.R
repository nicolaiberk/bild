# ______________________________________________
# Media framing effects
# Goal: Bild editorial change frame attention 
# Procedure: load data, estimate DiD, visualise
# ______________________________________________
# Date:  Tue Nov 16 14:36:30 2021
# Author: Nicolai Berk
#  R version 4.1.1 (2021-08-10)
# ______________________________________________


# Setup ####
library(tidyverse)
library(dplyr)
library(here)
library(lubridate)

# load data ####
load(here('data/media_daily_2021-10-03.Rdata'))
framenames <- colnames(merged_media)[6:12]

# estimate DiD ####

## aggregate
media_agg <- 
  merged_media %>% 
  filter(date_new < as.Date('2020-01-01')) %>% 
  mutate(month = floor_date(date_new, 'months')) %>% 
  group_by(paper, month) %>% 
  summarise(across(c(framenames, paste0(framenames, '_share'), share_mig), mean, na.rm = T))

## define treatment indicator
media_agg$edit_change <- 0
media_agg$edit_change[media_agg$month > as.Date('2017-01-31')] <- 1


# estimate model per newspaper per frame

if (exists('model_ests')){rm(model_ests)}

for (period in c('1617', 'all')){
  for (paper in unique(media_agg$paper)){
    for (frame in framenames){
      
      if(period == '1617'){
        temp <- 
          media_agg %>% 
          filter(year(month) >= 2016 & (year(month) <= 2017))
      }else{
        temp <- media_agg
      }
      
      
      temp$frame_share <- temp[[paste0(frame, '_share')]]
      temp$paper_bin   <- temp$paper == paper
      
      
      m_share <- lm(frame_share ~ edit_change * paper_bin, data = temp)
      
      if (!exists('model_ests')){
        model_ests <- 
          data.frame(
            frame = frame, 
            paper = paper, 
            period = period,
            est_share = m_share$coefficients[[4]],
            est_lower = confint(m_share)[4,][[1]],
            est_upper = confint(m_share)[4,][[2]]
              )
      }else{
        
        model_ests <- 
          rbind(
            data.frame(
              frame = frame, 
              paper = paper, 
              period = period,
              est_share = m_share$coefficients[[4]],
              est_lower = confint(m_share)[4,][[1]],
              est_upper = confint(m_share)[4,][[2]]
            ),
            model_ests
          )
        
        
      }
      
      
      
    }
  }
}

# visualise ####

## vis vars
ggplot(media_agg, aes(x = month, y = crime_share, col = paper, fill = paper))+
  geom_line() +
  geom_smooth() +
  geom_vline(xintercept = as.Date('2017-01-31')) +
  facet_wrap(~paper)

## vis effects

### all time
model_ests %>% 
  filter(period == 'all') %>% 
  ggplot(aes(x = est_share, 
             xmin = est_lower, 
             xmax = est_upper, 
             y = paper,
             col = (paper != 'bild')
             )
         ) +
    geom_pointrange()+
    geom_vline(xintercept = 0) +
    facet_wrap(~frame) +
  ggtitle('Estimated change in relative frame attention post-Diekmann departure', '2013-2019')
ggsave(here('paper/vis/did_papers_frames_all.png'))


### 2016-2017
model_ests %>% 
  filter(period == '1617') %>% 
  ggplot(aes(x = est_share, 
             xmin = est_lower, 
             xmax = est_upper, 
             y = paper,
             col = (paper != 'bild')
  )
  ) +
  geom_pointrange()+
  geom_vline(xintercept = 0) +
  facet_wrap(~frame) +
  ggtitle('Estimated change in relative frame attention post-Diekmann departure', '2016-2017')
ggsave(here('paper/vis/did_papers_frames_1617.png'))



