# preliminary analysis of panel dids


library(here)
library(dplyr)
library(tidyverse)
library(data.table)
library(haven)
library(lubridate)
library(glue)

dt <- read.csv(here('paper/ReportThomas/coeftable.csv'))

dt %>% 
  ggplot(aes(x = coef_fe, y = p_value_fe)) +
  geom_point()

dt %>% 
  ggplot(aes(x = coef_fe/sd(coef_fe))) +
  geom_histogram(aes(y = ..density..), bins = 100) +
  geom_function(fun = dnorm)

dt %>% 
  ggplot(aes(x = coef_fe/sd(coef_fe))) +
  geom_density(col = 'green') +
  geom_function(fun = dnorm) +
  geom_hline(aes(yintercept = 0.05), col = 'red', lty = 2)

dt %>% 
  ggplot(aes(x = sqrt(coef_fe^2), y = p_value_fe))+
  geom_point() +
  geom_rug() +
  geom_vline(xintercept = mean(dt$coef_fe^2), col = 'red', lty = 2)

dt$coef_fe_abs <- sqrt(dt$coef_fe^2)
coef_fe_abs_se <- sd(sqrt(dt$coef_fe^2))/sqrt(nrow(dt))


dt %>% 
  ggplot(aes(x = sqrt(coef_fe^2)))+
  geom_histogram(fill = 'lightblue') +
  geom_rug(col = 'darkgrey') +
  geom_vline(xintercept = mean(sqrt(dt$coef_fe^2)), col = 'red', lty = 2) +
  annotate('rect',
           xmin = mean(dt$coef_fe_abs) - qt(1 - (0.05 / 2), nrow(dt)-1)*coef_fe_abs_se, 
           xmax = mean(dt$coef_fe_abs) + qt(1 - (0.05 / 2), nrow(dt)-1)*coef_fe_abs_se,
           ymin = 0, ymax = 200, alpha = 0.2, fill = 'red'
           ) +
  xlab('Absolute coefficient size') + ylab('')+
  ggtitle("Distribution of absolute DiD-FE effect size",
          'Red area indicates mean with 95% confidence intervals')

ggsave(filename = here("paper/vis/DiD_model_coefs_abs.png"),
       width = 10, height = 8)

dt %>% 
  ggplot(aes(x = as.factor(medium), y = sqrt(coef_fe^2)))+
  geom_boxplot()




## p-values

dt$sig_fe <- (dt$p_value_fe < 0.05)
dt$sig_lm <- (dt$p_value_lm < 0.05)

prop.table(table(dt$sig_fe)) %>% round(2) # 21% instead of 5% -> significant effects overrepresented
prop.table(table(dt$sig_lm)) %>% round(2) # 12% instead of 5% -> significant effects overrepresented


fe_plot <- dt %>% 
  ggplot(aes(x = p_value_fe, y = ..count../nrow(dt))) +
  geom_histogram(bins = 20, col = 'gray', breaks = seq(from = 0, to = 0.95, 0.05)) +
  geom_hline(aes(yintercept = 0.05), col = 'red', lty = 2) +
  ylim(0, 0.25) +
  xlab('Density')+ylab('p')+
  ggtitle(glue("P-values from {nrow(dt)} fixed-effect DiD-models"), 
          "Treatment = wave, condition = media outlet consumption")

lm_plot <- dt %>% 
  ggplot(aes(x = p_value_lm, y = ..count../nrow(dt))) +
  geom_histogram(bins = 20, col = 'gray', breaks = seq(from = 0, to = 0.95, 0.05)) +
  geom_hline(aes(yintercept = 0.05), col = 'red', lty = 2) +
  ylim(0, 0.25) +
  xlab('Density')+ ylab('p')+
  ggtitle(glue("P-values from {nrow(dt)} DiD-models (average)"), 
          "Treatment = wave, condition = media outlet consumption")

theoretical_dist <- data.frame(p = runif(1336, 0, 1))

theoryplot <- theoretical_dist %>% 
  ggplot(aes(x = p, y = ..count../nrow(theoretical_dist))) +
  geom_histogram(bins = 20, col = 'gray', breaks = seq(from = 0, to = 0.95, 0.05)) +
  geom_hline(aes(yintercept = 0.05), col = 'red', lty = 2) +
  ylim(0, 0.25) +
  xlab('Density')+ylab('p')+
  ggtitle(glue("Theoretical distribution of p-values with no effect of media consumption"))

gridExtra::grid.arrange(fe_plot, theoryplot, nrow = 2) %>% 
  ggsave(plot = ., 
         filename = here("paper/vis/DiD_model_ps.png"),
         width = 10, height = 8)


# estimate correlation of migration frames in the media and attitudinal change

## load stm topic prevalence
load(here('data/daily_mig_topics.Rdata'))

issues <- 
  prevalence %>% 
  ungroup() %>% 
  select(ends_with('_m')) %>% 
  colnames() %>% 
  str_match(pattern = "(.*)_m") %>% 
  .[,2]

# add paper names to survey data
dt <- dt %>% 
  filter(medium == "1661")

dt$paper_name <- ''
dt$paper_name[dt$paper == 'a'] <- 'bild'
dt$paper_name[dt$paper == 'b'] <- 'falg'
dt$paper_name[dt$paper == 'c'] <- 'faz'
dt$paper_name[dt$paper == 'd'] <- 'sz'
dt$paper_name[dt$paper == 'e'] <- 'taz'
dt$paper_name[dt$paper == 'f'] <- 'welt'




## calculate topic prevalence in between survey waves
for (wave in sort(unique(dt$wave))[-1]){
  
  lower <- max(as.Date(dt$date[dt$wave == (wave-1)]))
  upper <- max(as.Date(dt$date[dt$wave == wave]))
  
  # if (upper-lower > months(6)){
  #   lower <- upper - months(6) # max period preceding 6 month
  # }
  
  temp <- 
      prevalence %>%
      filter(date_new > lower) %>% 
      filter(date_new < upper) %>% 
      summarise(across(ends_with('_m'), mean))
  
  temp <- 
    prevalence %>%
    filter(date_new > lower) %>% 
    filter(date_new < upper) %>% 
    summarise(across(ends_with('_s'), sum)) %>%
    merge(temp, by = "paper")
  
  temp$wave <- wave
  
  if (exists('prev_sum')){
    prev_sum <- rbind(prev_sum, temp)
  }else{
    prev_sum <-temp 
  }
}

## dv 1: 1130, Zuzugsmöglichkeiten für Ausländer
issue_mig <- dt %>% 
  filter(issue == "1130")
issue_mig$paper <- issue_mig$paper_name

prev_fin <- merge(prev_sum, issue_mig, by = c("paper", "wave"))
hist(prev_fin$p_value_fe) # still overrepresented

## correlate DiD with frame prevalence
plot_model <- function(dv, iv) {
  mod <- lm(dv ~ iv)
  mod.s <- summary(mod)
  plot.s <- plot(iv, dv)
  plot.a <- abline(a = mod$coefficients[1], b = mod$coefficients[2])
  return(list(mod.s, plot.s, plot.a))
}

# crime (neg, sig)
plot_model(prev_fin$coef_fe, log(prev_fin$crime_s))

# capital crime (neg)
plot_model(prev_fin$coef_fe, log(prev_fin$capcrime_s))

# refugee numbers (nonsig-neg)
plot_model(prev_fin$coef_fe, log(prev_fin$refnums_s))

# labour market (no effect really)
plot_model(prev_fin$coef_fe, log(prev_fin$labmar_s))

# mediterranean tragedy (neg)
plot_model(prev_fin$coef_fe, log(prev_fin$medit_s))

# deportations (nonsig-neg)
plot_model(prev_fin$coef_fe, log(prev_fin$deport_s))

# camps (nonsig-neg)
plot_model(prev_fin$coef_fe, log(prev_fin$camps_s))

# put all into same plot
plots <- list()
for (i in issues){
  prev_fin[paste0(i, "_s_log")] <- log(prev_fin[paste0(i, "_s")])
  mod <- summary(lm(prev_fin$coef_fe ~ prev_fin[,paste0(i, "_s_log")]))
  p_value <- mod$coefficients[2,4] %>% round(3)
  plots[[i]] <- ggplot(prev_fin, aes_string(paste0(i, "_s_log"), "coef_fe")) +
    geom_point() +
    geom_smooth(method = "lm") +
    geom_label(aes(x = Inf, y = Inf, hjust = 1, vjust = 1, 
                   label = paste("p:", p_value))) +
    ggtitle(i) + xlab("logged cum. frame attention") + ylab("Opinion shift")
}

gridExtra::grid.arrange(grobs = plots, 
                        top = "Change in immigration attitude",
                        bottom = paste("N = ", nrow(prev_fin))) %>% 
  ggsave(filename = "paper/vis/imm_eff.png", plot = .)

## seems like attention, not framing matters!


## control for papers
summary(lm(coef_fe ~ crime_s + paper, data = prev_fin))
summary(lm(coef_fe ~ capcrime_s + paper, data = prev_fin))
summary(lm(coef_fe ~ refnums_s + paper, data = prev_fin))
summary(lm(coef_fe ~ labmar_s + paper, data = prev_fin))
summary(lm(coef_fe ~ medit_s + paper, data = prev_fin))
summary(lm(coef_fe ~ deport_s + paper, data = prev_fin))
summary(lm(coef_fe ~ camps_s + paper, data = prev_fin))


# alternative dependent: should foreigners adapt to german culture?
issue_int <- dt %>% 
  filter(issue == "1210")
issue_int$paper <- issue_int$paper_name

prev_fin <- merge(prev_sum, issue_int, by = c("paper", "wave"))
hist(prev_fin$p_value_fe) # strongly overrepresented


# crime (neg sig for mean)
plot_model(prev_fin$coef_fe, log(prev_fin$crime_s)) # cum attention
plot_model(prev_fin$coef_fe, log(prev_fin$crime_m)) # sum attention


# capital crime (no effect)
plot_model(prev_fin$coef_fe, log(prev_fin$capcrime_s))
plot_model(prev_fin$coef_fe, log(prev_fin$capcrime_m))

# refugee numbers (no effect)
plot_model(prev_fin$coef_fe, log(prev_fin$refnums_s))
plot_model(prev_fin$coef_fe, prev_fin$refnums_m)

# labour market (pos sig for sum)
plot_model(prev_fin$coef_fe, log(prev_fin$labmar_s))
plot_model(prev_fin$coef_fe, log(prev_fin$labmar_m))

# mediterranean tragedy (non-sig pos for sum)
plot_model(prev_fin$coef_fe, log(prev_fin$medit_s))
plot_model(prev_fin$coef_fe, log(prev_fin$medit_m))

# deportations (positive)
plot_model(prev_fin$coef_fe, log(prev_fin$deport_s))
plot_model(prev_fin$coef_fe, log(prev_fin$deport_m))

# camps (nonsig-pos for cum)
plot_model(prev_fin$coef_fe, log(prev_fin$camps_s))
plot_model(prev_fin$coef_fe, log(prev_fin$camps_m))

# put all into same plot
plots <- list()
for (i in issues){
  prev_fin[paste0(i, "_s_log")] <- log(prev_fin[paste0(i, "_s")])
  mod <- summary(lm(prev_fin$coef_fe ~ prev_fin[,paste0(i, "_s_log")]))
  p_value <- mod$coefficients[2,4] %>% round(3)
  plots[[i]] <- ggplot(prev_fin, aes_string(paste0(i, "_s_log"), "coef_fe")) +
    geom_point() +
    geom_smooth(method = "lm") +
    geom_label(aes(x = Inf, y = Inf, hjust = 1, vjust = 1, 
                   label = paste("p:", p_value))) +
    ggtitle(i) + xlab("frame attention") + ylab("Opinion shift")
}

gridExtra::grid.arrange(grobs = plots, 
                        top = "Change in integration attitude",
                        bottom = paste("N = ", nrow(prev_fin))) %>% 
  ggsave(filename = "paper/vis/int_eff.png", plot = .)

## seems like attention, not framing matters!


## control for papers - welt seems to explain most effects?
summary(lm(coef_fe ~ crime_s + paper, data = prev_fin))
summary(lm(coef_fe ~ capcrime_s + paper, data = prev_fin))
summary(lm(coef_fe ~ refnums_s + paper, data = prev_fin))
summary(lm(coef_fe ~ labmar_s + paper, data = prev_fin))
summary(lm(coef_fe ~ medit_s + paper, data = prev_fin))
summary(lm(coef_fe ~ deport_s + paper, data = prev_fin))
summary(lm(coef_fe ~ camps_s + paper, data = prev_fin))


# plot effect size by paper
ggplot(dt, aes(p_value_fe, paper_name)) +
  geom_boxplot()
ggplot(dt, aes(coef_fe, paper_name)) +
  geom_boxplot()
ggplot(dt, aes(coef_fe_abs, paper_name)) +
  geom_boxplot()

prev_int <- prev_fin



# estimate effect of salience
## load BERT estimates
salience <- fread(here('data/BERT_estimates_cleaned.csv'), header = T)
salience <- salience[2:nrow(salience),] # remove duplicate header row

## use urls to identify paper
salience$urlhead <- substr(salience$link, 1, 20) # get different start urls
tb <- table(salience$urlhead, useNA = 'ifany')
urlheaders <- names(tb[tb > 500])

salience$paper <- NA_character_
salience$paper[salience$urlhead == "https://taz.de/Archi"] <- 'taz'
salience$paper[salience$urlhead == "https://www.bild.de/"] <- 'bild'
salience$paper[salience$urlhead == "https://www.faz.net/"] <- 'faz'
salience$paper[salience$urlhead == "https://www.jetzt.de"] <- 'sz'
salience$paper[salience$urlhead == "https://www.spiegel."] <- 'spon'
salience$paper[salience$urlhead == "https://www.sueddeut"] <- 'sz'
salience$paper[salience$urlhead == "https://www.welt.de/"] <- 'welt'
prop.table(table(salience$paper, useNA = 'ifany')) %>% round(3) # 99.9% assigned

## fix date
salience <- salience %>% 
  filter(!is.na(date)) %>% 
  filter(!is.na(paper))

salience$date_new <- NA_Date_
salience$date_new[salience$paper == "bild"] <- salience$date[salience$paper == "bild"] %>% as.Date()
salience$date_new[salience$paper == "faz"] <- salience$date[salience$paper=="faz"] %>% as.numeric() %>% as.Date(origin = "1970-01-01")
salience$date_new[salience$paper == "spon"] <- salience$date[salience$paper=="spon"] %>% as.Date(format = "%d.%m.%Y")
salience$date_new[salience$paper == "sz"] <- salience$date[salience$paper=="sz"] %>% as.integer() %>% as.Date(origin = "1970-01-01")
salience$date_new[salience$paper == "taz"] <- salience$date[salience$paper=="taz"] %>% as.integer() %>% as.Date(origin = "1970-01-01")
salience$date_new[salience$paper == "welt"] <- salience$date[salience$paper=="welt"] %>% as.integer() %>% as.Date(origin = "1970-01-01")
salience <- salience[salience$date_new > as.Date('2010-01-01'),]


## remove duplicates, select relevant columns, aggregate
sal_agg <- 
  salience %>% 
  filter(!duplicated(salience)) %>% 
  select(date_new, est, label, paper) %>% 
  filter(date_new >  as.Date('2010-01-01')) %>% 
  filter(!is.na(paper)) %>% 
  group_by(date_new, paper) %>% 
  summarise(
    estim = mean(as.numeric(est), na.rm = T),
    share = mean(as.logical(label), na.rm = T)
  )

rm(salience)

## calculate salience in between survey waves
for (wave in sort(unique(dt$wave))[-1]){
  
  lower <- max(as.Date(dt$date[dt$wave == (wave-1)]))
  upper <- max(as.Date(dt$date[dt$wave == wave]))
  
  # if (upper-lower > months(1)){
  #   lower <- upper - months(1) # max period preceding 1 month
  # }
  
  temp <- 
    sal_agg %>%
    group_by(paper) %>% 
    filter(date_new > lower) %>% 
    filter(date_new < upper) %>% 
    summarise(
      est = sum(estim),
      sha = sum(share)
    )
  
  temp$wave <- wave
  
  if (exists('sal_sum')){
    sal_sum <- rbind(sal_sum, temp)
  }else{
    sal_sum <-temp 
  }
}


# effect on immigration attitude
sal_fin <- merge(sal_sum, issue_mig, by = c("paper", "wave"))

# plot effect of migration salience - weak evidence
plot_model(sal_fin$coef_fe, log(sal_fin$est))
plot_model(sal_fin$coef_fe, log(sal_fin$sha))

mod <- summary(lm(coef_fe ~ log(sha), data = sal_fin))
p_value <- mod$coefficients[2,4] %>% round(3)

plot_imm_l <- 
  ggplot(sal_fin, aes(log(sha), coef_fe)) +
  geom_point() +
  geom_smooth(method = "lm") +
  geom_label(aes(x = Inf, y = Inf, hjust = 1, vjust = 1, 
                 label = paste("p:", p_value))) +
  xlab("immigration salience") + ylab("Opinion shift") + 
  ggtitle("Effect of salience on imm. att.: linear effect")


## quadratic term
mod <- summary(lm(coef_fe ~ log(sha) + I(log(sha)^2), data = sal_fin))
p_value_l <- mod$coefficients[2,4] %>% round(3)
p_value_q <- mod$coefficients[3,4] %>% round(3)

plot_imm_q <- 
  ggplot(sal_fin, aes(log(sha), coef_fe)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x + I(x^2)) +
  geom_label(aes(x = Inf, y = Inf, hjust = 1, vjust = 1, 
                 label = paste("p (x):", p_value_l, 
                               "\np (x^2):", p_value_q))) +
  xlab("immigration salience") + ylab("Opinion shift") + 
  ggtitle("Effect of salience on imm. att.: quadratic term")

# effect on integration attitudes
sal_fin <- merge(sal_sum, issue_int, by = c("paper", "wave"))

## plot effect of migration salience
plot_model(sal_fin$coef_fe, log(sal_fin$est))
plot_model(sal_fin$coef_fe, log(sal_fin$sha))

mod <- summary(lm(coef_fe ~ log(sha), data = sal_fin))
p_value <- mod$coefficients[2,4] %>% round(3)
plot_int_l <- 
  ggplot(sal_fin, aes(log(sha), coef_fe)) +
  geom_point() +
  geom_smooth(method = "lm") +
  geom_label(aes(x = Inf, y = Inf, hjust = 1, vjust = 1, 
                 label = paste("p:", p_value))) +
  xlab("immigration salience") + ylab("Opinion shift") + 
  ggtitle("Effect of salience on int. att.: linear effect")


## quadratic term seems to fit the model better
mod <- summary(lm(coef_fe ~ log(sha) + I(log(sha)^2), data = sal_fin))
p_value_l <- mod$coefficients[2,4] %>% round(3)
p_value_q <- mod$coefficients[3,4] %>% round(3)

plot_int_q <- 
  ggplot(sal_fin, aes(log(sha), coef_fe)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x + I(x^2)) +
  geom_label(aes(x = Inf, y = Inf, hjust = 1, vjust = 1, 
                 label = paste("p (x):", p_value_l, 
                               "\np (x^2):", p_value_q))) +
  xlab("immigration salience") + ylab("Opinion shift") + 
  ggtitle("Effect of salience on int. att.: quadratic term")

gridExtra::grid.arrange(plot_imm_l,
                        plot_imm_q,
                        plot_int_l,
                        plot_int_q)

############################################################################


## assess most significant issue-paper combinations more closely
gles_p_long <- fread(here('data/gles/Panel/long_cleaned.csv')) %>% as.data.frame()


### plot function
plot_shifts_news <- function(issue, medium, mag = F) {
  tempdta <- data.frame(row.names = 1:nrow(gles_p_long))
  if (mag){
    tempdta$condition <- gles_p_long[, paste0(medium, '_clean')]
  }else{
    tempdta$condition <- gles_p_long[, paste0(medium, '_bin')]
  }
  tempdta$dependent <- gles_p_long[, paste0(issue, '_clean')]
  tempdta$date_clean <- gles_p_long$date_clean
  tempdta$wave <- gles_p_long$wave
  
  tempdta <- tempdta %>% 
    filter(!is.na(condition) & !is.na(dependent)) %>% 
    group_by(wave, condition) %>% 
    summarise(
      date_clean = max(date_clean, na.rm = T),
      dependent_sd = sd(dependent),
      dependent = mean(dependent),
      n_respondents = n()
    )
  
  ### CI's
  tempdta$dependent_sd <-  tempdta$dependent_sd/sqrt(tempdta$n_respondents)
  tempdta$low_ci <- tempdta$dependent - qt(1 - (0.05 / 2), tempdta$n_respondents-1) * tempdta$dependent_sd
  tempdta$upp_ci <- tempdta$dependent + qt(1 - (0.05 / 2), tempdta$n_respondents-1) * tempdta$dependent_sd
  
  shift_plot <- tempdta %>% 
    ggplot(
      aes(
        x = date_clean, 
        y = dependent,
        ymin = low_ci,
        ymax = upp_ci,
        color = condition,
        fill = condition
        )) +
    geom_line() +
    geom_ribbon(alpha = 0.3) + 
    scale_x_date(date_labels = '%d.%m.%Y')
  
  return(shift_plot)
  
}




### 2880aa - Die Europäische Union sollte Mitgliedstaaten, die keine Flüchtlinge aufnehmen wollen, finanzielle Mittel kürzen. 
plot_shifts_news('2880aa', '1661a')
plot_shifts_news('2880aa', '1661g')
plot_shifts_news('2880aa', '1661b')
plot_shifts_news('2880aa', '1681c')

### 2880ae - Die EU-Beitrittsverhandlungen mit der Türkei sollten abgebrochen werden. 


### 2880y  - Die deutschen Verteidigungsausgaben sollten in den nächsten Jahren erhöht werden.


### 2880h  - Die staatlichen Befugnisse in der Kriminalitätsbekämpfung sollten ausgeweitet werden, auch wenn das zu einer stärkeren Überwachung der Bürger führt. 
plot_shifts_news('2880h', '1661a') 
plot_shifts_news('2880h', '1661c') 


### 1250   - Europäische Integration, Ego 


### 1130   - Zuzugsmöglichkeiten Ausländer, Ego 
plot_shifts_news('1130', '1681a')



## most commonly asked issues

### 2880e
plot_shifts_news('2880e', '1661a') # bild
plot_shifts_news('2880e', '1661c') # faz
plot_shifts_news('2880e', '1661d') # sz
plot_shifts_news('2880e', '1661e') # taz
plot_shifts_news('2880e', '1661f') # welt
plot_shifts_news('2880e', '1701aa', mag = T) # spiegel online

# ### 2880g - doesn't overlap much with news data
# plot_shifts_news('2880g', '1661a') # bild
# plot_shifts_news('2880g', '1661c') # faz
# plot_shifts_news('2880g', '1661d') # sz
# plot_shifts_news('2880g', '1661e') # taz
# plot_shifts_news('2880g', '1661f') # welt
# plot_shifts_news('2880g', '1701aa', mag = T) # spiegel online

### 2880j - good issue (rich should pay more taxes)
plot_shifts_news('2880j', '1661a') # bild
plot_shifts_news('2880j', '1661c') # faz
plot_shifts_news('2880j', '1661d') # sz
plot_shifts_news('2880j', '1661e') # taz
plot_shifts_news('2880j', '1661f') # welt
plot_shifts_news('2880j', '1701aa', mag = T) # spiegel online

# ### 2880v - little overlap with media questions
# plot_shifts_news('2880v', '1661a') # bild
# plot_shifts_news('2880v', '1661c') # faz
# plot_shifts_news('2880v', '1661d') # sz
# plot_shifts_news('2880v', '1661e') # taz
# plot_shifts_news('2880v', '1661f') # welt
# plot_shifts_news('2880v', '1701aa', mag = T) # spiegel online


### 2880w - good issue (allowing cars with petrol/diesel motor)
plot_shifts_news('2880w', '1661a') # bild
plot_shifts_news('2880w', '1661c') # faz
plot_shifts_news('2880w', '1661d') # sz
plot_shifts_news('2880w', '1661e') # taz
plot_shifts_news('2880w', '1661f') # welt
plot_shifts_news('2880w', '1701aa', mag = T) # spiegel online

### 2880y - good issue (defense spending)
plot_shifts_news('2880y', '1661a') # bild
plot_shifts_news('2880y', '1661c') # faz
plot_shifts_news('2880y', '1661d') # sz
plot_shifts_news('2880y', '1661e') # taz
plot_shifts_news('2880y', '1661f') # welt
plot_shifts_news('2880y', '1701aa', mag = T) # spiegel online


# ### 2880ac - sufficient waves, but less interesting topic (abgasmanipulationen)
# plot_shifts_news('2880ac', '1661a') # bild
# plot_shifts_news('2880ac', '1661c') # faz
# plot_shifts_news('2880ac', '1661d') # sz
# plot_shifts_news('2880ac', '1661e') # taz
# plot_shifts_news('2880ac', '1661f') # welt
# plot_shifts_news('2880ac', '1701aa', mag = T) # spiegel online


### 1130 - good issue (Immigration)
plot_shifts_news('1130', '1661a') # bild
plot_shifts_news('1130', '1661c') # faz
plot_shifts_news('1130', '1661d') # sz
plot_shifts_news('1130', '1661e') # taz
plot_shifts_news('1130', '1661f') # welt
plot_shifts_news('1130', '1701aa', mag = T) # spiegel online


### 1290 - good issue (battle climate change)
plot_shifts_news('1290', '1661a') # bild
plot_shifts_news('1290', '1661c') # faz
plot_shifts_news('1290', '1661d') # sz
plot_shifts_news('1290', '1661e') # taz
plot_shifts_news('1290', '1661f') # welt
plot_shifts_news('1290', '1701aa', mag = T) # spiegel online


### 1411 - good issue („Eingriffe des Staates bei der Terrorismusbekämpfung“)
plot_shifts_news('1411', '1661a') # bild
plot_shifts_news('1411', '1661c') # faz
plot_shifts_news('1411', '1661d') # sz
plot_shifts_news('1411', '1661e') # taz
plot_shifts_news('1411', '1661f') # welt
plot_shifts_news('1411', '1701aa', mag = T) # spiegel online

### 1210 - good issue (integration)
plot_shifts_news('1210', '1661a') # bild
plot_shifts_news('1210', '1661c') # faz
plot_shifts_news('1210', '1661d') # sz
plot_shifts_news('1210', '1661e') # taz
plot_shifts_news('1210', '1661f') # welt
plot_shifts_news('1210', '1701aa', mag = T) # spiegel online


### 1250 all waves but 1 close to election (EU integration)
# plot_shifts_news('1250', '1661a') # bild
# plot_shifts_news('1250', '1661c') # faz
# plot_shifts_news('1250', '1661d') # sz
# plot_shifts_news('1250', '1661e') # taz
# plot_shifts_news('1250', '1661f') # welt
# plot_shifts_news('1250', '1701aa', mag = T) # spiegel online


