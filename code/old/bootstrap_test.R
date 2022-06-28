# test bootstrap
gles_p_long <- 
  fread(here('data/gles/Panel/long_cleaned.csv')) %>% as.data.frame()

gles_p_long$post <- 
  gles_p_long$date_clean > as.Date("2017-01-01")

gles_p_long$treat <- 
  gles_p_long$readership == "Bild"

gles_p_long <- 
  gles_p_long %>% 
  filter(wave == 1) %>% 
  mutate(imm_att = cut(`1130_clean`, breaks = c(-Inf, -1.5, 1.5, Inf), labels = c("liberal", "moderate", "conservative"))) %>% 
  select(imm_att, lfdn) %>% 
  right_join(gles_p_long, by = "lfdn")


boot_sample <- 
  gles_p_long %>% 
  mutate(
    dv = `1130_clean`,
    iv = treat
  ) %>% 
  select(lfdn, wave, iv, dv, post, imm_att) %>% 
  pivot_wider(id_cols = c(lfdn), values_from = c(iv, dv, imm_att, post), names_from = c(wave)) %>% 
  sample_n(1000)

  
library(boot)
set.seed(42)
B <- 100

effs1 <- function(dat, inds){
  
  # sample observations, then wide to long
  dta_model <- 
    dat[inds,] %>% 
    pivot_longer(cols = contains("iv"), names_to = "wave", values_to = "iv", names_prefix = "(.*)_") %>% 
    select(lfdn, wave, iv)
  
  dta_model <- 
    dat[inds,] %>% 
    pivot_longer(cols = contains("dv"), names_to = "wave", values_to = "dv", names_prefix = "(.*)_") %>% 
    select(c(contains("dv"), wave, lfdn)) %>% 
    left_join(., dta_model, by = c("wave", "lfdn"))
  
  dta_model <- 
    dat[inds,] %>% 
    pivot_longer(cols = contains("imm_att"), names_to = "wave", values_to = "imm_att", names_prefix = "(.*)_") %>% 
    select(c(contains("imm_att"), wave, lfdn)) %>% 
    left_join(., dta_model, by = c("wave", "lfdn"))
  
  
  dta_model <- 
    dat[inds,] %>% 
    pivot_longer(cols = contains("post"), names_to = "wave", values_to = "post", names_prefix = "(.*)_") %>% 
    select(c(contains("post"), wave, lfdn)) %>% 
    left_join(., dta_model, by = c("wave", "lfdn"))
  
  did_model <- lm(dv ~ post*iv*imm_att, data = dta_model)
  coe <- coef(did_model)
  c(liberal = coe[[paste0("postTRUE:ivTRUE")]],
    moderate = 
      coe[[paste0("postTRUE:ivTRUE")]] + 
      coe[[paste0("postTRUE:ivTRUE", ":imm_attmoderate")]],
    conservative = 
      coe[[paste0("postTRUE:ivTRUE")]] + 
      coe[[paste0("postTRUE:ivTRUE", ":imm_attconservative")]])
}

# run bootstrap
res1 <- boot(boot_sample, effs1, R = B, parallel = "snow")

hist(res1$t[,1])
mean(res1$t[,1])
hist(res1$t[,2])

hist(res1$t[,3])

# wide to long
dta_model <- 
  boot_sample %>% 
  pivot_longer(cols = contains("iv"), names_to = "wave", values_to = "iv", names_prefix = "(.*)_") %>% 
  select(lfdn, wave, iv)

dta_model <- 
  boot_sample %>% 
  pivot_longer(cols = contains("dv"), names_to = "wave", values_to = "dv", names_prefix = "(.*)_") %>% 
  select(c(contains("dv"), wave, lfdn)) %>% 
  left_join(., dta_model, by = c("wave", "lfdn"))

dta_model <- 
  boot_sample %>% 
  pivot_longer(cols = contains("imm_att"), names_to = "wave", values_to = "imm_att", names_prefix = "(.*)_") %>% 
  select(c(contains("imm_att"), wave, lfdn)) %>% 
  left_join(., dta_model, by = c("wave", "lfdn"))


dta_model <- 
  boot_sample %>% 
  pivot_longer(cols = contains("post"), names_to = "wave", values_to = "post", names_prefix = "(.*)_") %>% 
  select(c(contains("post"), wave, lfdn)) %>% 
  left_join(., dta_model, by = c("wave", "lfdn"))

# compare to linear model
summary(lm(dv ~ post*iv*imm_att, data = dta_model))

## moderate estimate
mean(res1$t[,1]) # fine

2.2481+(-2.2391)
mean(res1$t[,2]) # not too far

2.2481+(-2.3273)
mean(res1$t[,3], na.rm = T) # good





## check again with sample data (bad example, but doesn't matter)
nitro_long <- 
  nitrofen %>% 
  select(-total) %>% 
  pivot_longer(cols = brood1:brood3, names_to = "brood", names_prefix = "brood", values_to = "value")

effs2 <- function(dat, inds){
  
  # sample observations, then wide to long
  dta_model <- 
    dat[inds,] %>% 
    pivot_longer(cols = brood1:brood3, names_to = "brood", names_prefix = "brood", values_to = "value")
  
  did_model <- lm(value ~ brood*conc, data = dta_model)
  coe <- coef(did_model)
  c(liberal = coe[["conc"]],
    moderate = 
      coe[["conc"]] + 
      coe[["brood2:conc"]],
    conservative = 
      coe[["conc"]] + 
      coe[["brood3:conc"]])
}

res2 <- boot(nitrofen, effs2, R = 1000)

hist(res2$t[,2])
mean(res2$t[,2])
hist(res2$t[,3])
mean(res2$t[,3])

## compare to linear model
summary(lm(value ~ brood*conc, data = nitro_long))

-0.0001+(-0.0372)
mean(res2$t[,2]) # close

-0.0001+(-0.04592)
mean(res2$t[,3]) # close
