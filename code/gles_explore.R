# ______________________________________________
# Effects of changing media frames - case of Bild
# Goal: Data overview GLES
# Procedure: load, explore
# ______________________________________________
# Date:  Wed Apr 07 16:32:03 2021
# Author: Nicolai Berk
#  R version 4.0.3 (2020-10-10)
# ______________________________________________


# Setup ####
library(tidyverse)
library(here)
library(data.table)
library(haven)
library(gridExtra)
library(magrittr)

#  Rolling cross section ####
gles_rc <- read_dta(here('data/gles/RCS/ZA6834_v1-0-0.dta'))

## BILD
### Lesen Sie gelegentlich oder regelmäßig die BILD-Zeitung? (Falls ja, an wie vielen Tagen pro Woche lesen Sie politische Berichte?)
gles_nna <- gles_rc[gles_rc$pre2401 > 0, ]
table(gles_nna$pre2401)

gles_nna$bild <- gles_nna$pre2401 < 7
sum(gles_nna$bild == 1) # 2049 Bild readers total
sum(gles_nna$bild == 1)/nrow(gles_nna) # ~10%

### unique readers per wave: 627 - 715
t1 <- table(gles_nna$bild, gles_nna$year)
t1
round(prop.table(t1, margin = 2), 2)
t1 %>% tableGrob() %>% grid.arrange()


## MIP open question: pre1001_org (not available, check .sav)

## MIP open question: pre1001_org (.sav; not available - mail went out to GLES)
# gles_rc <- read_sav('data/gles/ZA6834_v1-0-0.sav')

## Integration attitude: 
  # pre1401f (2013 pre: Einwanderer an dt. Kultur anpassen) 
  # pos1001a_2 (2013 post: Einwanderer an dt. Kultur anpassen) 
  # pos1001a_1 (2017: Einwanderer an dt. Kultur anpassen) 

## Immigration attitude:
  # Zuwanderung: ego position pre1302 (2017)
  # Zuwanderung: ego position pos0901 (2017)

## Wahlentscheidung: pos0303a, pos0303b, pos0304a, pos0304b


rm(list = ls())





# Panel ####
gles_p <- read_dta(here('data/gles/Panel/ZA6838_w1to9_sA_v3-0-0.dta')) # this is only w1-9, should merge w 10-14 later on

## Bild consumption (available in w1,3,4,5,6,7,8)
bild_vars <- colnames(gles_p)[grep(pattern = '1661a', colnames(gles_p))]
bild_waves <- str_extract(bild_vars, '\\d')

### generate cleaned variables
for (i in 1:length(bild_vars)){
  
  new_var <- paste0('bild_w', bild_waves[i])
  
  gles_p[[new_var]] <- NA
  gles_p[[new_var]][gles_p[[bild_vars[i]]] > 0] <- gles_p[[bild_vars[i]]][gles_p[[bild_vars[i]]] > 0] - 1

}


bild_vars <- colnames(gles_p)[grep(pattern = 'bild', colnames(gles_p))]

### plot, summarise
p <- list()
n_readers <- c()
n_respondents <- c()
share_readers <- c()

for (v in bild_vars){
  
  n_wave <- str_extract(v, '\\d')
  n_readers[n_wave] <- sum(gles_p[[v]] > 0, na.rm = T)
  n_respondents[n_wave] <- sum(!is.na(gles_p[[v]]))
  share_readers[n_wave] <- round(n_readers[n_wave]/n_respondents[n_wave], 2)*100
  
  p <- 
    ggplot(gles_p, aes_string(x = v)) +
    geom_histogram(binwidth = 1) + 
    ggtitle(label = paste0('How many days per week do you read Bild? (wave ', n_wave, ')'),
            subtitle = paste0(n_readers[n_wave], 
                              ' Bild readers out of ', 
                              n_respondents[n_wave], ' respondents; (', 
                              share_readers[n_wave], '%)')
            )
  
  ggsave(filename = paste0(here('data/gles/Panel/vis/BildReadersPanel_w'), n_wave, '.png'), p)
  
}

### plot reader share across waves
plot(str_extract(bild_vars, '\\d'), 
     share_readers, 
     main = 'Share of Bild readers across waves',
     xlab = 'Wave', ylab = '%', type = 'l',
     ylim = c(0, max(share_readers))) %>% 
  ggsave(filename = here('data/gles/Panel/vis/ReadersWaves_rel.png'))

### plot absolute readers across waves
plot(str_extract(bild_vars, '\\d'), 
     n_readers, 
     main = 'Share of Bild readers across waves',
     xlab = 'Wave', ylab = '%', type = 'l',
     ylim = c(0, max(n_readers))) %>% 
  ggsave(filename = here('data/gles/Panel/vis/ReadersWaves_abs.png'))



## immigration attitude (), bild vs rest 
int_vars <- colnames(gles_p)[grep(pattern = '1210', colnames(gles_p))]
int_waves <- str_extract(bild_vars, '\\d')
int_waves <- int_waves[int_waves %in% bild_waves]

bild_vars <- bild_vars[bild_waves %in% int_waves]
bild_waves <- bild_waves[bild_waves %in% int_waves]

### generate cleaned variables
for (i in 1:length(int_vars)){
  
  new_var <- paste0('int_w', int_waves[i])
  
  gles_p[[new_var]] <- NA
  gles_p[[new_var]][gles_p[[int_vars[i]]] > 0] <- gles_p[[int_vars[i]]][gles_p[[int_vars[i]]] > 0] * (-1) + 8
  
}


int_vars <- colnames(gles_p)[grep(pattern = 'int_', colnames(gles_p))]


### filter, wide -> long
data_long_bild <- gles_p %>% 
  select(c(lfdn, contains("bild"))) %>% 
  gather(
    wave,
    bild_reader,
    bild_w1:bild_w8
    )


data_long_bild$wave <- data_long_bild$wave %>% str_replace(pattern = "bild_w", "")


data_long_int <- gles_p %>% 
  select(c(lfdn, contains("int_"))) %>% 
  gather(
    wave,
    int_att,
    int_w1:int_w7
  )

data_long_int$wave <- data_long_int$wave %>% str_replace(pattern = "int_w", "")

data_long_date <- gles_p %>% 
  select(c(lfdn, contains("datetime"))) %>% 
  gather(
    wave,
    datetime,
    kp1_datetime:kp9_datetime
  )

data_long_date$wave <- data_long_date$wave %>% str_replace(pattern = "_datetime", "")
data_long_date$wave <- data_long_date$wave %>% str_replace(pattern = "kp", "")

data_long_date$datetime <- data_long_date$datetime %>% str_replace(pattern = "-95", "")
data_long_date$datetime <- data_long_date$datetime %>% as.Date(format = "%Y-%m-%d %H:%M:%S")


### merge
data_long <- inner_join(data_long_bild, data_long_int, by = c("lfdn", "wave"))
data_long <- inner_join(data_long, data_long_date, by = c("lfdn", "wave"))

data_long$wave <- as.numeric(data_long$wave)
data_long$bild_reader <- data_long$bild_reader > 0

### summarise
data_sum <- 
  data_long %>% 
  filter(!is.na(int_att) & (!is.na(bild_reader))) %>% 
  group_by(wave, bild_reader) %>% 
  summarise(intatt = mean(int_att),
            intatt_sd = sd(int_att),
            responses = n(),
            surveydate = max(datetime))

### calculate CI's
data_sum$se <- data_sum$intatt_sd/sqrt(data_sum$responses)
data_sum$ci_low <- data_sum$intatt - qt(1 - (0.05 / 2), data_sum$responses-1)*data_sum$se
data_sum$ci_up <- data_sum$intatt + qt(1 - (0.05 / 2), data_sum$responses-1)*data_sum$se


## plot mean integration attitude across time, bild vs rest
ggplot(data_sum, 
       aes(x = surveydate, 
           y = intatt, 
           ymin = ci_low,
           ymax = ci_up,
           col = bild_reader,
           fill = bild_reader)) +
  geom_line() +
  geom_ribbon(alpha = 0.5) +
  scale_x_date(date_labels = "%b %Y") +
  geom_vline(xintercept = as.Date(c("01.09.2015","01.01.2016", "01.02.2017", "01.03.2018"), 
                                  format = "%d.%m.%Y")) +
  geom_label(inherit.aes = F,
             aes(y = 5, 
                 x = as.Date("01.04.2017", format = "%d.%m.%Y")), 
             label = "Diekmann leaves") +
  geom_label(inherit.aes = F,
             aes(y = 5, 
                 x = as.Date("01.03.2018", format = "%d.%m.%Y")), 
             label = "Koch -> Reichelt") +
  ggtitle("Should foreigners adapt to German culture?", 
          paste0("1 = 'should be able to live according to their culture'\n7 = 'should completely adapt'\nSource: GLES Panel"))


rm(list = ls())


# LT Tracking ####
gles_lt <- read_dta(here('data/gles/LongtermTracking/ZA6832_v1-1-0.dta'))

## Bild consumption
bild_vars <- colnames(gles_lt)[grep(pattern = 'k0400a', colnames(gles_lt))]
gles_lt$bild_reader <-  NA
for (var in bild_vars){
  gles_lt$bild_reader[gles_lt[var] > 1] <- T
  gles_lt$bild_reader[gles_lt[var] == 1] <- F
}

## migration & integration attitude (m4863 [migration], m4888 [integration])
gles_lt$mig_att <- NA
gles_lt$mig_att[gles_lt$m4863 > 0] <- gles_lt$m4863[gles_lt$m4863 > 0]

gles_lt$int_att <- NA
gles_lt$int_att[gles_lt$m4888 > 0] <- gles_lt$m4888[gles_lt$m4888 > 0]

## date
gles_lt$field_end <- as.Date(gles_lt$field_end)


### summarise
data_sum <- 
  gles_lt %>% 
  filter(!is.na(mig_att) & (!is.na(bild_reader))) %>% 
  group_by(field_end, bild_reader) %>% 
  summarise(mig = mean(mig_att),
            mig_sd = sd(mig_att),
            responses = n())

### calculate CI's
data_sum$mig_se <- data_sum$mig_sd/sqrt(data_sum$responses)
data_sum$mig_ci_low <- data_sum$mig - qt(1 - (0.05 / 2), data_sum$responses-1)*data_sum$mig_se
data_sum$mig_ci_up <- data_sum$mig + qt(1 - (0.05 / 2), data_sum$responses-1)*data_sum$mig_se


### plot mean migration attitude across time, bild vs rest
p1 <- ggplot(data_sum, 
       aes(x = field_end, 
           y = mig, 
           ymin = mig_ci_low,
           ymax = mig_ci_up,
           col = bild_reader,
           fill = bild_reader)) +
  geom_line() +
  geom_ribbon(alpha = 0.5) +
  scale_x_date(date_labels = "%b %Y", 
               limits = c(as.Date("2013-12-01"), 
                          as.Date("2018-01-01"))) +
  geom_vline(xintercept = as.Date(c("01.09.2015","01.01.2016", "01.02.2017"), 
                                  format = "%d.%m.%Y")) +
  geom_label(inherit.aes = F,
             x = as.Date("01.09.2015", format = "%d.%m.%Y"),
             y = 8.6,
             label = "Keletti") +
  geom_label(inherit.aes = F,
             x = as.Date("01.04.2016", format = "%d.%m.%Y"),
             y = 9,
             label = "Diekmann -> Koch") +
  geom_label(inherit.aes = F,
             x = as.Date("01.04.2017", format = "%d.%m.%Y"),
             y = 8.6,
             label = "Diekmann leaves") +
  ggtitle("Should it be made easier or harder for foreigners to come to Germany?", 
          paste0("1 = 'easier'\n11 = 'harder'\nSource: GLES LT Tracking"))


## integration

### summarise
data_sum <- 
  gles_lt %>% 
  filter((!is.na(int_att)) & (!is.na(bild_reader))) %>% 
  group_by(field_end, bild_reader) %>% 
  summarise(int = mean(int_att),
            int_sd = sd(int_att),
            responses = n())


### CIs
data_sum$int_se <- data_sum$int_sd/sqrt(data_sum$responses)
data_sum$int_ci_low <- data_sum$int - qt(1 - (0.05 / 2), data_sum$responses-1)*data_sum$int_se
data_sum$int_ci_up <- data_sum$int + qt(1 - (0.05 / 2), data_sum$responses-1)*data_sum$int_se


### plot mean integration attitude across time, bild vs rest
p2 <- ggplot(data_sum, 
       aes(x = field_end, 
           y = int, 
           ymin = int_ci_low,
           ymax = int_ci_up,
           col = bild_reader,
           fill = bild_reader)) +
  geom_line() +
  geom_ribbon(alpha = 0.5) +
  scale_x_date(date_labels = "%b %Y", 
               limits = c(as.Date("2013-12-01"), 
                          as.Date("2018-01-01"))) +
  geom_vline(xintercept = as.Date(c("01.09.2015","01.01.2016", "01.02.2017"), 
                                  format = "%d.%m.%Y")) +
  geom_label(inherit.aes = F,
             x = as.Date("01.09.2015", format = "%d.%m.%Y"),
             y = 5,
             label = "Keletti") +
  geom_label(inherit.aes = F,
             x = as.Date("01.04.2016", format = "%d.%m.%Y"),
             y = 4.75,
             label = "Diekmann -> Koch") +
  geom_label(inherit.aes = F,
             x = as.Date("01.04.2017", format = "%d.%m.%Y"),
             y = 5,
             label = "Diekmann leaves") +
  ggtitle("Should foreigners adapt to German culture?", 
          paste0("1 = 'fully adapt'\n11 = 'live according to own culture'\nSource: GLES LT Tracking"))

grid.arrange(p1, p2) %>% 
  ggsave(filename = here("github/bild/paper/LTT_attitudes.png"),
         height = 10, width = 10)
