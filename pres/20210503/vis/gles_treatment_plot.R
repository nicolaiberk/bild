library(tidyverse)

header <-  c("survey", "unit", "start_date", "end_date")
schedule <- t(data.frame(
  a = c("LT tracking", "T27", "27.02.2015", "07.03.2015"),
  b = c("LT tracking", "T28", "05.06.2015", "19.06.2015"),
  c = c("LT tracking", "T29", "11.09.2015", "25.09.2015"),
  d = c("LT tracking", "T30", "04.12.2015", "18.12.2015"),
  e = c("LT tracking", "T31", "26.02.2016", "11.03.2016"),
  f = c("LT tracking", "T32", "03.06.2016", "17.06.2016"),
  g = c("LT tracking", "T33", "19.08.2016", "02.09.2016"),
  h = c("LT tracking", "T34", "02.12.2016", "16.12.2016"),
  i = c("LT tracking", "T35", "17.03.2017", "31.03.2017"),
  j = c("LT tracking", "T36", "16.06.2017", "30.06.2017"),
  k = c("LT tracking", "T37", "12.09.2017", "23.09.2017"),
  l = c("LT tracking", "T38", "01.12.2017", "15.12.2017"),
  m = c("Panel", "Wave 1", "06.10.2016", "11.10.2016"),
  n = c("Panel", "Wave 2", "16.02.2017", "03.03.2017"),
  o = c("Panel", "Wave 3", "11.05.2017", "23.05.2017"),
  p = c("Panel", "Wave 4", "06.07.2017", "09.08.2017"),
  r = c("Panel", "Wave 5", "17.08.2017", "28.08.2017"),
  s = c("Panel", "Wave 6", "04.09.2017", "13.09.2017"),
  t = c("Panel", "Wave 7", "18.09.2017", "23.09.2017"),
  u = c("Panel", "Wave 8", "27.09.2017", "09.10.2017"),
  v = c("Panel", "Wave 9", "15.03.2018", "26.03.2018"),
  w = c("Panel", "Wave 10", "06.11.2018", "31.01.2019"),
  x = c("Panel", "Wave 11", "28.05.2019", "08.07.2019"),
  y = c("Panel", "Wave 12", "05.11.2019", "17.12.2019"),
  z = c("Panel", "Wave 13", "21.04.2020", "01.06.2020")
)) %>% 
  as.data.frame()

colnames(schedule) <- header

schedule$start_date <- schedule$start_date %>% as.Date(format = "%d.%m.%Y")
schedule$end_date <- schedule$end_date %>% as.Date(format = "%d.%m.%Y")

p1 <- ggplot(data = schedule) +
  geom_linerange(aes(x = survey, 
                     ymin = start_date, 
                     ymax = end_date,
                     color = survey),
                 size = 8) +
  geom_hline(yintercept = as.Date(c("01.09.2015","01.01.2016", "01.02.2017", "01.03.2018"), 
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
  xlab("") +
  ggtitle("Treatments and survey field times") +
  coord_flip() +
  theme(legend.position = 'none')

ggsave(p1, filename = "gles_treatments.png", width = 8, height = 4)
