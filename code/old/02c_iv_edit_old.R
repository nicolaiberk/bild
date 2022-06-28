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
load(here('data/media_daily_2021-11-19.Rdata'))
framenames <- colnames(merged_media)[6:12]

# estimate DiD ####

## aggregate
media_agg <- 
  merged_media %>% 
  filter(date_new < as.Date('2020-01-01')) %>% 
  mutate(month = floor_date(date_new, 'months')) %>% 
  group_by(paper, month) %>% 
  summarise(across(share_mig:`60_share`, mean, na.rm = T))

## define treatment indicator
media_agg$edit_change <- 0
media_agg$edit_change[media_agg$month > as.Date('2017-01-31')] <- 1


# estimate model per newspaper per frame

if (exists('model_ests')){rm(model_ests)}

for (period in c('1617', 'all')){
  for (paper in unique(media_agg$paper)){
    for (frame in c(framenames, "share_mig")){
      for (dv in c("Share of migration content", "Share of all articles")){
        if(period == '1617'){
          temp <- 
            media_agg %>% 
            filter(year(month) >= 2016 & (year(month) <= 2017))
        }else{
          temp <- media_agg
        }
        
        if (dv == "Share of migration content" & (frame != "share_mig")){
          temp$frame_share <- temp[[paste0(frame, '_share')]]
        }else if (dv == "Share of all articles" & (frame != "share_mig")){
          temp$frame_share <- temp[[paste0(frame, '_share')]]*temp[["share_mig"]]
        }else if (dv == "Share of all articles" & (frame == "share_mig")){
          next
        }else if (frame == "share_mig"){
          temp$frame_share <- temp$share_mig
        }
        
        temp$paper_bin   <- temp$paper == paper
        
        
        m_share <- lm(frame_share ~ edit_change * paper_bin, data = temp)
        
        if (!exists('model_ests')){
          model_ests <- 
            data.frame(
              frame = frame, 
              paper = paper, 
              period = period,
              dv = dv,
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
                dv = dv,
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
}

# visualise ####

## vis effects

model_ests %>% 
  filter(period == '1617') %>% 
  ggplot(aes(x = est_share, 
             xmin = est_lower, 
             xmax = est_upper, 
             y = paper,
             col = !(est_lower > 0 | (est_upper < 0))
  )
  ) +
  geom_pointrange()+
  geom_vline(xintercept = 0) +
  facet_grid(dv~frame) +
  ggtitle('Estimated change in relative frame attention post-Diekmann departure', '2016-2017') +
  theme_minimal() +
  scale_color_discrete(name = 'p', labels = c('< 0.05', '> 0.05')) +
  theme(legend.position = "bottom")
ggsave(filename = here('paper/vis/did_papers_frames_1617.png'), 
       width = 10, height = 6)

model_ests %>% 
  arrange(est_share) %>% 
  filter(period == '1617') %>% 
  filter(dv == "Share of migration content") %>% 
  ggplot(
    aes(x = est_share, xmin = est_lower, xmax = est_upper, y = 1:nrow(.), 
        col = !(paper == 'bild' & (frame == 'crime')))
  ) +
  geom_vline(xintercept = 0, col = "red", lty = 2) +
  geom_pointrange() + 
  xlab('Estimated level change in attention post Diekmann-departure') +
  ylab('') +
  ggtitle('Estimated change in relative frame attention post-Diekmann departure', 
          'Change in relative Bild crime frame attention compared to changes in all other outlets and specific frames, 2016-2017') +
  theme_minimal() +
  scale_color_discrete(name = '', labels = c('Bild change on Crime', 'Other changes')) +
  theme(legend.position = "bottom")
ggsave(here('paper/vis/did_papers_frames_1617_scatter.png'),
       width = 10, height = 6)


## KINK ####
library(rdrobust)

if (exists('model_ests')){rm(model_ests)}

media_agg$forcing <- media_agg$month - as.Date('2017-01-31')

for (period in c('1617', 'all')){
  for (paper in unique(media_agg$paper)){
    for (frame in framenames){
      
      if(period == '1617'){
        temp <- 
          media_agg %>% 
          filter(year(month) >= 2016 & (year(month) <= 2017)) %>% 
          filter(paper == paper)
      }else{
        temp <- media_agg %>% 
          filter(paper == paper)
      }
      
      
      temp$frame_share <- temp[[paste0(frame, '_share')]]
      
      m_share <- 
        rdrobust(
          y = temp$frame_share,
          x = temp$forcing,
          c = 0,
          deriv = 1)
      
      if (!exists('model_ests')){
        model_ests <- 
          data.frame(
            frame = frame, 
            paper = paper, 
            period = period,
            est = m_share$coef[[1]],
            se = m_share$se[[1]]
          )
      }else{
        
        model_ests <- 
          rbind(
            data.frame(
              frame = frame, 
              paper = paper, 
              period = period,
              est = m_share$coef[[1]],
              se = m_share$se[[1]]
            ),
            model_ests
          )
        
        
      }
      
      
      
    }
  }
}




# ALL FRAMES ####

## estimate model per newspaper per frame for all frames

if (exists('model_ests')){rm(model_ests)}

for (period in c('1617', 'all')){
  for (paper in unique(media_agg$paper)){
    for (frame in as.character(1:60)){
      
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

### all time
model_ests %>% 
  filter(period == 'all') %>% 
  ggplot(aes(x = est_share, 
             xmin = est_lower, 
             xmax = est_upper, 
             y = paper,
             col = !(est_lower > 0 | (est_upper < 0))
  )
  ) +
  geom_pointrange()+
  geom_vline(xintercept = 0) +
  facet_wrap(~frame) +
  ggtitle('Estimated change in relative frame attention post-Diekmann departure', '2013-2019')
ggsave(here('paper/vis/did_papers_allframes_all.png'), width = 10, height = 6)

model_ests %>% 
  arrange(est_share) %>% 
  filter(period == 'all') %>% 
  ggplot(
    aes(x = est_share)
  ) +
  geom_histogram() + 
  geom_vline(xintercept = model_ests %>% filter(paper == 'bild', frame == '25', period == 'all') %>% select(est_share) %>% unlist, col = 'red') +
  xlab('Estimated level change in attention post Diekmann-departure') +
  ylab('') +
  ggtitle('Estimated change in relative frame attention post-Diekmann departure', 
          'Change in relative Bild crime frame attention compared to changes in all other outlets and frames, 2013-2019') +
  theme_minimal() +
  scale_color_discrete(name = '', labels = c('Bild change on Crime', 'Other changes'))
ggsave(here('paper/vis/did_papers_allframes_all_hist.png'), width = 10, height = 6)


### 2016-2017
model_ests %>% 
  filter(period == '1617') %>% 
  ggplot(aes(x = est_share, 
             xmin = est_lower, 
             xmax = est_upper, 
             y = paper,
             col = !(est_lower > 0 | (est_upper < 0))
  )
  ) +
  geom_pointrange()+
  geom_vline(xintercept = 0) +
  facet_wrap(~frame) +
  ggtitle('Estimated change in relative frame attention post-Diekmann departure', '2016-2017')
ggsave(here('paper/vis/did_papers_frames_1617.png'), width = 10, height = 6)


plot_1617_all_general <- 
  model_ests %>% 
  arrange(est_share) %>% 
  filter(period == '1617') %>% 
  ggplot(
    aes(x = est_share)
  ) +
  geom_histogram() + 
  geom_vline(xintercept = model_ests %>% filter(paper == 'bild', frame == '25', period == '1617') %>% select(est_share) %>% unlist, col = 'red') +
  xlab('Estimated level change in attention post Diekmann-departure') +
  ylab('') +
  ggtitle('Estimated change in relative frame attention post-Diekmann departure',
          'Change in relative Bild crime frame attention compared to changes in all other outlets and frames, 2016-2017') +
  theme_minimal() +
  scale_color_discrete(name = '', labels = c('Bild change on Crime', 'Other changes'))
ggsave(here('paper/vis/did_papers_allframes_1617_hist.png'), plot_1617_all_general, width = 10, height = 6)

