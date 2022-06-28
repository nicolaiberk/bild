# ______________________________________________
# Bild
# Goal: Estimate migration sentiment in nps
# ______________________________________________
# Date:  Fri May 21 08:30:00 2021
# Author: Nicolai Berk
#  R version 4.0.3 (2020-10-10)
# ______________________________________________


# Setup ####
library(tidyverse)
library(here)
library(data.table)
library(lubridate)
library(Hmisc)

# parameters ####
paper_list <- c("bild", "faz", "spon", 'sz')
aggregation <- "quarter"

for (paper in paper_list){
  print(paper)
    # load data ####
    dta <- fread(here(paste0("data/np_sentiment_bert/", paper, "_estimates.csv")))
    
    if (paper == "faz" | paper == "sz"){
      dta$date <- as.Date(dta$date, origin = "1970-01-01")
    }else if (paper == "spon"){
      dta$date <- as.Date(dta$date, format = "%d.%m.%Y")
    }
    
    dta$time_agg <- floor_date(dta$date, aggregation)
    
    
    ## aggregate to new dataframe
    temp <- dta %>%
      select(time_agg, mig_sent, mig_sent_n) %>%
      group_by(time_agg) %>%
      summarise(mean_mig_sent = mean(mig_sent, na.rm = T),
                wtd_mean_mig_sent = wtd.mean(mig_sent, weights = mig_sent_n, na.rm = T, normwt = T),
                sd_mig_sent = sd(mig_sent, na.rm = T),
                n_mig = n() - sum(is.na(mig_sent))
                )
    temp$se_mig_sent <- temp$sd_mig_sent/sqrt(temp$n_mig)
    
    x <- paste0("dta_sum_", paper)
    eval(call("<-", as.name(x), temp))
    
    ## add CIs
    
    ## single np graph [add CI]
    temp <- get(paste0("dta_sum_", paper)) %>% 
      ggplot(aes(x = time_agg)) +
      geom_line(aes(y = mean_mig_sent, col = "raw")) +
      geom_line(aes(y = wtd_mean_mig_sent, col = "weighted")) +
      ggtitle("Sentiment in migration-related sentences", paper)
    
    ggsave(filename = here(paste0("github/bild/paper/", paper, "_sentiment_migration.png")),
           plot = temp,
           device = "png", height = 6, width = 10)
      
    
}



# vis ####
linePlot <- function(paper, wtd = F) {
  
  if(wtd){
    geom_line(data = get(paste0("dta_sum_", paper)), aes(x = time_agg, y = wtd_mean_mig_sent, col = paper, lty = paper))
  }else{
    geom_line(data = get(paste0("dta_sum_", paper)), aes(x = time_agg, y = mean_mig_sent, col = paper, lty = paper))
    
  }
  
}

CIPlot <- function(paper, wtd = F) {
  
  if(wtd){
    geom_ribbon(data = get(paste0("dta_sum_", paper)), 
                   aes(x = time_agg, 
                       ymin = (wtd_mean_mig_sent - qt(1 - (0.05 / 2), n_mig-1)*se_mig_sent), 
                       ymax = (wtd_mean_mig_sent + qt(1 - (0.05 / 2), n_mig-1)*se_mig_sent), 
                       col = paper, fill = paper, lty = paper), alpha = 0.3)
  }else{
    geom_ribbon(data = get(paste0("dta_sum_", paper)), 
                   aes(x = time_agg, 
                       ymin = (mean_mig_sent - qt(1 - (0.05 / 2), n_mig-1)*se_mig_sent), 
                       ymax = (mean_mig_sent + qt(1 - (0.05 / 2), n_mig-1)*se_mig_sent), 
                       col = paper, fill = paper, lty = paper), alpha = 0.3)
    
  }
  
}

## define colors
my_colors <- c("red", rep("gray", 3))
names(my_colors) <- paper_list

## all np graph (weighted)
plot <- ggplot() +
  linePlot("faz") +
  CIPlot("faz") +
  linePlot("spon") +
  CIPlot("spon") +
  linePlot("sz") +
  CIPlot("sz") +
  linePlot("bild") +
  CIPlot("bild") +
  geom_vline(xintercept = as.Date(c("01.09.2015","01.01.2016", "01.02.2017", "01.03.2018"), 
                                  format = "%d.%m.%Y")) +
  geom_label(aes(y = -0.075, 
             x = as.Date("01.09.2015", format = "%d.%m.%Y")), 
             label = "Keletti") +
  geom_label(aes(y = -0.1, 
             x = as.Date("01.04.2016", format = "%d.%m.%Y")), 
             label = "Diekmann -> Koch") +
  geom_label(aes(y = -0.075, 
             x = as.Date("01.04.2017", format = "%d.%m.%Y")), 
             label = "Diekmann leaves") +
  geom_label(aes(y = -0.1, 
             x = as.Date("01.03.2018", format = "%d.%m.%Y")), 
             label = "Koch -> Reichelt") +
  ggtitle("Migration sentiment across time", 
          paste0("Average per ", aggregation, ", based on BERT estimates of all migration-related sentences")) +
  scale_fill_manual(name = "paper", values = my_colors, aesthetics = c("fill"))

## save
ggsave(filename = here("github/bild/pres/vis/comparative_sentiment_migration.png"), 
       plot = plot, height = 6, width = 10)


  
## all np graph (weighted)
ggplot() +
  linePlot("faz", wtd = T) +
  CIPlot("faz", wtd = T) +
  linePlot("spon", wtd = T) +
  CIPlot("spon", wtd = T) +
  linePlot("sz", wtd = T) +
  CIPlot("sz", wtd = T) +
  linePlot("bild", wtd = T) +
  CIPlot("bild", wtd = T) +
  geom_vline(xintercept = as.Date(c("01.09.2015","01.01.2016", "01.02.2017", "01.03.2018"), 
                                  format = "%d.%m.%Y")) +
  geom_label(x = 2.3, 
             y = as.Date("01.09.2015", format = "%d.%m.%Y"), 
             label = "Keletti") +
  geom_label(x = 2.5, 
             y = as.Date("01.04.2016", format = "%d.%m.%Y"), 
             label = "Diekmann -> Koch") +
  geom_label(x = 2.3, 
             y = as.Date("01.04.2017", format = "%d.%m.%Y"), 
             label = "Diekmann leaves") +
  geom_label(x = 2.5, 
             y = as.Date("01.03.2018", format = "%d.%m.%Y"), 
             label = "Koch -> Reichelt") +
  ggtitle("Migration sentiment across time", 
          paste0("Average per ", aggregation, ", based on BERT estimates of all migration-related sentences (weighted)"))
