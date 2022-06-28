# function preparing data for independent variable (STM)

## load & transform data frame attention
load(here("data/processed/stm_daily_2022-02-02.Rdata")); envlist <- c("merged_media")

merged_media <- 
  merged_media %>% 
  mutate(post = date_new >= as.Date("2017-02-01"),
         date_month = floor_date(as.Date(date_new), 'month')) %>% 
  mutate(
    date_month = floor_date(as.Date(date_new), 'month')
  ) %>%
  group_by(paper, date_month) %>%
  summarise(across(share_mig:hungary_referendum_share, mean, na.rm = T),
            across(n_mig_sal:n_tot, sum, na.rm = T)) %>%
  mutate(crime_share = crime_schlepp_share + sexual_assault_share,
         post = date_month >= as.Date("2017-02-01")) %>% 
  mutate(paper = ifelse(paper %in% c("bild", "Bild"), "Bild", "Other"),
         month = date_month) %>%
  group_by(month, paper) %>%
  select(crime_share) %>% 
  summarise(
    crime_share = mean(crime_share)
  ) %>% 
  mutate(estimate = "STM")

save(list = c("merged_media"), file = here("data/processed/stm_for_treatment.Rdata"))
rm(envlist)