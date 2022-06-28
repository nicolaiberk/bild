# pivot_longer test script
library(tidyverse)

df_wide <- 
  data.frame(id = 1:3, A_1 = 1:3, A_2 = 4:6, B_1 = 7:9, B_2 = 10:12)

# what I try
df_long <- 
  df_wide %>% 
  pivot_longer(
    cols = A_1:B_2,
    names_to = c(".value", "wave"),
    names_pattern = "(.*)_(.)",
    values_to = c("A", "B")
  )

# Error in standardise_join_by(by, x_names = x_names, y_names = y_names) : 
#   object 'wave' not found


# what I want to do, in multiple commands

## generate variable "A"
df_long <- 
  df_wide %>% 
  select(id, A_1, A_2) %>% 
  pivot_longer(
    cols = A_1:A_2,
    names_to = c("wave"),
    names_pattern = ".*_(.)",
    values_to = "A"
  )

## add variable "B"
df_long <- 
  df_wide %>% 
  select(id, B_1, B_2) %>% 
  pivot_longer(
    cols = B_1:B_2,
    names_to = c("wave"),
    names_pattern = ".*_(.)",
    values_to = "B"
  ) %>% 
  full_join(df_long, by = c("id", "wave")) %>% 
  select(id, wave, A, B)

df_long
