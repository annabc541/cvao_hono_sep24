library(tidyverse)
library(lubridate)
library(zoo)
library(oce)
library(openair)

Sys.setenv(TZ = "UTC")

# Functions ----------------------------------------------------------------

tidy_rle = function(rleObj){
  
  require(dplyr)
  
  rleDf = tibble(lengths = rleObj$lengths,
                 values = rleObj$values) %>% 
    mutate(idxStart = cumsum(c(1,lengths))[1:(nrow(.))],
           idxEnd = cumsum(lengths))
  
  #
  rleDf
}

ppt <- function(ch1,ch2,se){
  
  x = (ch1/se) - (ch2-ch1*(1-se))
  return(x)
}

lod <- function(ch1_zero_3sd,ch2_zero_3sd,slope_cal1,slope_cal2){
  
  x = ((ch1_zero_3sd*slope_cal1)^2+(ch2_zero_3sd*slope_cal2)^2)^0.5
  return(x)
}

# Reading in raw data -----------------------------------------------------

raw_dat1 = read.table("raw_data/old_inlet_water_sep05/old_inlet_water_sep05.dat",sep = ";",header = TRUE,skip = 4) %>% 
  mutate(date = glue::glue("{Date} {Time}"),
         date = dmy_hms(date)) %>%
  filter(date > "2024-09-05 17:45" & date < "2024-09-05 20:15") %>% 
  select(date,ch1 = X550.6,ch2 = X550.0)

raw_dat2 = read.table("raw_data/reagents01/reagents01.dat",sep = ";",header = TRUE,skip = 4) %>% 
  mutate(date = glue::glue("{Date} {Time}"),
         date = dmy_hms(date)) %>%
  filter(date < "2024-09-06 15:28") %>%
  select(date,ch1 = X550.6,ch2 = X550.0)

raw_dat3 = read.table("raw_data/reagents02/reagents02.dat",sep = ";",header = TRUE,skip = 4) %>% 
  mutate(date = glue::glue("{Date} {Time}"),
         date = dmy_hms(date)) %>%
  filter(date < "2024-09-06 21:30") %>%
  select(date,ch1 = X550.6,ch2 = X550.0)

raw_dat4 = read.table("raw_data/water_sep07/water_sep07.dat",sep = ";",header = TRUE,skip = 4) %>% 
  mutate(date = glue::glue("{Date} {Time}"),
         date = dmy_hms(date)) %>% 
  filter(date > "2024-09-07 16:15") %>%
  select(date,ch1 = X550.6,ch2 = X550.0)

raw_dat = bind_rows(raw_dat1,raw_dat2,raw_dat3,raw_dat4) %>% 
  arrange(date)

remove(raw_dat1,raw_dat2,raw_dat3,raw_dat4)

#date < "2024-09-05 20:15"
raw_dat %>% 
  filter(date > "2024-09-06 21:10" & date < "2024-09-06 21:47") %>%
  pivot_longer(c(ch1,ch2)) %>%
  ggplot(aes(date,value)) +
  geom_point() +
  facet_grid(rows = vars(name),scales = "free") +
  # scale_x_datetime(breaks = "10 min",date_labels = "%H:%M") +
  NULL

# Flagging data -----------------------------------------------------------
 
#can leftjoin to any df to have flags for zeroing or for issues (0 = good data,1 = zero,2 = cal,3 = issues)
flagged_data = raw_dat %>% 
  mutate(flag = case_when(date < "2024-09-05 18:45" ~ 1,
                          date > "2024-09-06 14:45" & date < "2024-09-06 15:28" ~ 1,
                          date > "2024-09-06 21:00" & date < "2024-09-06 21:29" ~ 1,
                          #date < "2024-09-07 17:30" ~ 3,
                          date > "2024-09-07 22:30" & date < "2024-09-07 23:15" ~ 1,
                          date > "2024-09-07 23:40" & date < "2024-09-08 11:15" ~ 3,
                          date > "2024-09-08 15:00" & date < "2024-09-08 16:00" ~ 3,
                          date > "2024-09-08 17:15" & date < "2024-09-08 18:40" ~ 1,
                          date > "2024-09-08 23:30" & date < "2024-09-09 00:15" ~ 1,
                          date > "2024-09-09 05:50" & date < "2024-09-09 13:15" ~ 3,
                          date > "2024-09-09 13:15" & date < "2024-09-09 13:50" ~ 1,
                          date > "2024-09-09 14:40" & date < "2024-09-09 18:15" ~ 2,
                          date > "2024-09-09 23:45" & date < "2024-09-10 00:25" ~ 1,
                          date > "2024-09-10 06:00" & date < "2024-09-10 06:40" ~ 1,
                          date > "2024-09-10 12:15" & date < "2024-09-10 13:00" ~ 1,
                          date > "2024-09-10 15:00" & date < "2024-09-10 17:00" ~ 3,
                          date > "2024-09-10 18:30" & date < "2024-09-10 19:15" ~ 1,
                          date > "2024-09-10 21:30" & date < "2024-09-11 10:00" ~ 3,#only ch1, ch2 good with zeroes
                          date > "2024-09-11 11:54" & date < "2024-09-11 14:45" ~ 3,
                          date > "2024-09-11 17:00" & date < "2024-09-11 18:45" ~ 3,
                          date > "2024-09-11 18:45" & date < "2024-09-11 19:25" ~ 1,
                          date > "2024-09-12 01:00" & date < "2024-09-12 01:45" ~ 1,
                          date > "2024-09-12 07:15" & date < "2024-09-12 08:15" ~ 1,
                          date > "2024-09-12 12:20" & date < "2024-09-12 14:30" ~ 3,
                          date > "2024-09-12 14:30" & date < "2024-09-12 15:10" ~ 1,
                          date > "2024-09-12 20:45" & date < "2024-09-12 21:20" ~ 1,
                          date > "2024-09-13 03:00" & date < "2024-09-13 03:40" ~ 1,
                          date > "2024-09-13 09:15" & date < "2024-09-13 10:00" ~ 1,
                          date > "2024-09-13 14:00" & date < "2024-09-13 16:00" ~ 2,
                          date > "2024-09-13 17:45" & date < "2024-09-13 18:50" ~ 1,
                          date > "2024-09-14 00:15" & date < "2024-09-14 01:00" ~ 1,
                          date > "2024-09-14 06:30" & date < "2024-09-14 08:17" ~ 1,#zero and then changing integration times
                          date > "2024-09-14 08:17" & date < "2024-09-14 10:15" ~ 2,#cal with new integration times
                          date > "2024-09-14 13:00" & date < "2024-09-14 13:45" ~ 1,
                          date > "2024-09-14 19:15" & date < "2024-09-14 19:55" ~ 1,
                          date > "2024-09-15 01:30" & date < "2024-09-15 02:15" ~ 1,
                          date > "2024-09-15 07:45" & date < "2024-09-15 08:30" ~ 1,
                          date > "2024-09-15 14:00" & date < "2024-09-15 14:51" ~ 1,
                          date > "2024-09-15 20:15" & date < "2024-09-15 21:00" ~ 1,
                          date > "2024-09-15 23:55" & date < "2024-09-16 16:40" ~ 3,
                          date > "2024-09-16 17:40" & date < "2024-09-16 18:30" ~ 1,
                          date > "2024-09-17 00:00" & date < "2024-09-17 00:40" ~ 1,
                          date > "2024-09-17 06:15" & date < "2024-09-17 06:55" ~ 1,
                          date > "2024-09-17 12:30" & date < "2024-09-17 13:15" ~ 1,
                          date > "2024-09-17 18:45" & date < "2024-09-17 19:25" ~ 1,
                          date > "2024-09-18 01:05" & date < "2024-09-18 01:41" ~ 1,
                          date > "2024-09-18 07:20" & date < "2024-09-18 08:03" ~ 1,
                          date > "2024-09-18 13:00" ~ 1,
                          TRUE ~ 0))

#plot of ch1 and ch2 in same facet without baseline correction (also without zeroes,cals or bad data)
# flagged_data %>% 
#   mutate(ch1 = ifelse(flag == 0,ch1,NA_real_),
#          ch2 = ifelse(flag == 0,ch2,NA_real_)) %>%
#   timeAverage("30 sec") %>% 
#   rename(`HONO + interferences (ch1)` = ch1,
#          `Interferences (ch2)` = ch2) %>% 
#   # filter(date > "2024-09-18 13:15" & date < "2024-09-18 13:32") %>% 
#   pivot_longer(c(`HONO + interferences (ch1)`,`Interferences (ch2)`)) %>%
#   ggplot(aes(date,value,col = name)) +
#   theme_bw() +
#   geom_path() +
#   labs(x = "Datetime (UTC)",
#        y = "Absorbance",
#        col = NULL) +
#   theme(legend.position = "top") +
#   # facet_grid(rows = vars(name),scales = "free") +
#   scale_x_datetime(breaks = "1 day",date_labels = "%d %b")

# ggsave('raw_ch1_ch2.png',
#        path = "output/analysis_plots",
#        width = 30,
#        height = 12,
#        units = 'cm')

# Night zeroing -----------------------------------------------------------

night_flag = flagged_data %>% 
  mutate(ch1 = ifelse(flag == 0 | flag == 2,ch1,NA_real_),
         ch2 = ifelse(flag == 0 | flag == 2,ch2,NA_real_),
         hour = hour(date),
         day = day(date),
         bad_nights = case_when(date > "2024-09-11 20:00" & date < "2024-09-12 05:00" ~ 1,
                           TRUE ~ 0),
         night_zero = ifelse(hour <= 4 & bad_nights == 0 | hour >= 21 & bad_nights == 0,1,0))

#creates a group for each zero and maintains row no. of main df so can easily left_join
night_zeroing = rle(night_flag$night_zero) %>%
  tidy_rle() %>% 
  filter(values != 0) %>% 
  mutate(id = 1:nrow(.)) %>% 
  as.list() %>% 
  purrr::pmap_df(~data.frame(idx = ..3:..4,id = ..5)) %>% 
  tibble() 

#join dfs with groups for each zero
night_zeroes_grouped = night_flag %>% 
  mutate(idx = 1:nrow(.)) %>% 
  left_join(night_zeroing, "idx") %>% #joins two dfs by their row number
  mutate(id = ifelse(is.na(id), 0, id)) #makes id (group) = 0 when not zeroing

#average zero value for each group
night_zero_avg = night_zeroes_grouped %>% 
  filter(id != 0) %>%
  group_by(id) %>% 
  summarise(ch1_zeroes = mean(ch1,na.rm = T),
            ch2_zeroes = mean(ch2,na.rm = T),
            ch1_zero_3sd = 3 * sd(ch1,na.rm = T),
            ch2_zero_3sd = 3 * sd(ch2,na.rm = T),
            idx = mean(idx)) %>% 
  ungroup() %>% 
  mutate(idx = round(idx))

#interpolate between zeroes and subtract zeroes from measurements
night_zeroed = night_flag %>% 
  mutate(idx = 1:nrow(.)) %>% 
  left_join(night_zero_avg) %>% 
  mutate(ch1_zeroes1 = ifelse(date < "2024-09-16 02:00",ch1_zeroes,NA_real_),
         ch2_zeroes1 = ifelse(date < "2024-09-16 02:00",ch2_zeroes,NA_real_),
         ch1_zeroes2 = ifelse(date > "2024-09-16 02:00",ch1_zeroes,NA_real_),
         ch2_zeroes2 = ifelse(date > "2024-09-16 02:00",ch2_zeroes,NA_real_),
         ch1_zeroes_inter1 = na.approx(ch1_zeroes1,na.rm = F),
         ch2_zeroes_inter1 = na.approx(ch2_zeroes1,na.rm = F),
         ch1_zeroes_inter2 = na.approx(ch1_zeroes2,na.rm = F),
         ch2_zeroes_inter2 = na.approx(ch2_zeroes2,na.rm = F)) %>% 
  fill(ch1_zeroes_inter2,ch2_zeroes_inter2,.direction = "down") %>%
  fill(ch1_zeroes_inter1,ch2_zeroes_inter1,.direction = "up") %>%
  mutate(ch1_zeroed = ifelse(date < "2024-09-16",ch1 - ch1_zeroes_inter1,ch1 - ch1_zeroes_inter2),
         ch2_zeroed = ifelse(date < "2024-09-16",ch2 - ch2_zeroes_inter1,ch2 - ch2_zeroes_inter2))

night_zeroed %>% 
  pivot_longer(c(ch2_zeroes_inter1,ch2_zeroes_inter2)) %>% 
  ggplot(aes(date,value,col = name)) +
  geom_point()

night_zeroed %>% 
  rename(`HONO + interferences (ch1)` = ch1_zeroes,
         `Interferences (ch2)` = ch2_zeroes) %>%
  pivot_longer(c(`HONO + interferences (ch1)`,`Interferences (ch2)`)) %>%
  ggplot(aes(date,value,col = name)) +
  theme_bw() +
  geom_point() +
  labs(x = "Datetime (UTC)",
       y = "Absorbance",
       col = NULL) +
  theme(legend.position = "top") +
  # facet_grid(rows = vars(name),scales = "free") +
  scale_x_datetime(breaks = "1 day",date_labels = "%d %b")

night_zeroed %>%
  mutate(ch1_zeroed = ifelse(flag == 0,ch1_zeroed,NA_real_),
         ch2_zeroed = ifelse(flag == 0,ch2_zeroed,NA_real_)) %>%
  timeAverage("30 sec") %>%
  rename(`HONO + interferences (ch1)` = ch1_zeroed,
         `Interferences (ch2)` = ch2_zeroed) %>%
  pivot_longer(c(`HONO + interferences (ch1)`,`Interferences (ch2)`)) %>%
  ggplot(aes(date,value,col = name)) +
  theme_bw() +
  geom_path() +
  labs(x = "Datetime (UTC)",
       y = "Absorbance",
       col = NULL) +
  theme(legend.position = "top") +
  # facet_grid(rows = vars(name),scales = "free") +
  scale_x_datetime(breaks = "1 day",date_labels = "%d %b")

# ggsave('night_zeroed_ch1_ch2.png',
#        path = "output/analysis_plots",
#        width = 30,
#        height = 12,
#        units = 'cm')

# Zeroing -----------------------------------------------------------------

zero_flag = flagged_data %>% 
  mutate(ch1 = ifelse(flag == 3,NA_real_,ch1),
         ch2 = ifelse(flag == 3,NA_real_,ch2),
         zeroing = case_when(date > "2024-09-05 17:45" & date < "2024-09-05 18:17" ~ 1,
                             date > "2024-09-06 14:55" & date < "2024-09-06 15:15" ~ 1,
                             date > "2024-09-06 21:10" & date < "2024-09-06 21:47" ~ 1,
                             date > "2024-09-07 16:30" & date < "2024-09-07 16:44" ~ 1,
                             date > "2024-09-07 22:40" & date < "2024-09-07 23:00" ~ 1,
                             date > "2024-09-08 05:00" & date < "2024-09-08 05:15" ~ 1,#ch2 only
                             date > "2024-09-08 11:15" & date < "2024-09-08 11:30" ~ 1,#seeing nothing in ch2
                             date > "2024-09-08 17:28" & date < "2024-09-08 17:48" ~ 1,#bump in the zero
                             date > "2024-09-08 23:45" & date < "2024-09-09 00:03" ~ 1,#bump in zero
                             date > "2024-09-09 05:58" & date < "2024-09-09 06:10" ~ 1,#just before losing both ch
                             date > "2024-09-09 13:25" & date < "2024-09-09 13:38" ~ 1,#ch2 looking dodge
                             date > "2024-09-09 17:40" & date < "2024-09-09 18:00" ~ 1,
                             date > "2024-09-09 23:55" & date < "2024-09-10 00:15" ~ 1,
                             date > "2024-09-10 06:10" & date < "2024-09-10 06:30" ~ 1,
                             date > "2024-09-10 12:25" & date < "2024-09-10 12:40" ~ 1,#very werid, big bump
                             date > "2024-09-11 00:56" & date < "2024-09-11 01:20" ~ 1,#just ch2
                             date > "2024-09-11 07:12" & date < "2024-09-11 07:35" ~ 1,#just ch2
                             date > "2024-09-11 13:50" & date < "2024-09-11 14:10" ~ 1,
                             date > "2024-09-11 13:47" & date < "2024-09-11 14:20" ~ 1,#longer zero a few bumps along the way
                             date > "2024-09-11 17:25" & date < "2024-09-11 17:40" ~ 1,#lost ch2 right after zero
                             #set ZA flow to 1.8 to combat fluctuating gas flow
                             date > "2024-09-11 18:55" & date < "2024-09-11 19:15" ~ 2,#after za flow upped, both chs going down in constantly in zero
                             date > "2024-09-12 01:15" & date < "2024-09-12 01:30" ~ 2,
                             date > "2024-09-12 07:29" & date < "2024-09-12 07:48" ~ 2,
                             date > "2024-09-12 14:45" & date < "2024-09-12 14:57" ~ 2,#basically invisible in ch2
                             date > "2024-09-12 21:00" & date < "2024-09-12 21:15" ~ 2,
                             date > "2024-09-13 03:15" & date < "2024-09-13 03:30" ~ 2,
                             date > "2024-09-13 09:30" & date < "2024-09-13 09:45" ~ 2,
                             date > "2024-09-13 18:20" & date < "2024-09-13 18:28" ~ 2,
                             date > "2024-09-14 00:25" & date < "2024-09-14 00:45" ~ 2,
                             date > "2024-09-14 06:40" & date < "2024-09-14 07:00" ~ 2,
                             date > "2024-09-14 08:17" & date < "2024-09-14 09:00" ~ 3,
                             date > "2024-09-14 13:10" & date < "2024-09-14 13:28" ~ 3,
                             date > "2024-09-14 19:35" & date < "2024-09-14 19:45" ~ 3,
                             date > "2024-09-15 01:43" & date < "2024-09-15 02:01" ~ 3,
                             date > "2024-09-15 07:57" & date < "2024-09-15 08:17" ~ 3,
                             date > "2024-09-15 14:15" & date < "2024-09-15 14:33" ~ 3,
                             date > "2024-09-15 20:30" & date < "2024-09-15 20:47" ~ 3,
                             date > "2024-09-16 17:55" & date < "2024-09-16 18:15" ~ 3,
                             date > "2024-09-17 00:13" & date < "2024-09-17 00:30" ~ 3,
                             date > "2024-09-17 06:28" & date < "2024-09-17 06:44" ~ 3,
                             date > "2024-09-17 12:43" & date < "2024-09-17 13:01" ~ 3,
                             date > "2024-09-17 19:00" & date < "2024-09-17 19:17" ~ 3,
                             date > "2024-09-18 01:17" & date < "2024-09-18 01:35" ~ 3,
                             date > "2024-09-18 07:35" & date < "2024-09-18 07:50" ~ 3,
                             date > "2024-09-18 13:15" & date < "2024-09-18 13:32" ~ 3,
                             TRUE ~ 0))

#creates a group for each zero and maintains row no. of main df so can easily left_join
zeroing = rle(zero_flag$zeroing) %>%
  tidy_rle() %>% 
  filter(values != 0) %>% 
  mutate(id = 1:nrow(.)) %>% 
  as.list() %>% 
  purrr::pmap_df(~data.frame(idx = ..3:..4,id = ..5)) %>% 
  tibble() 

#join dfs with groups for each zero
zeroes_grouped = zero_flag %>% 
  mutate(idx = 1:nrow(.)) %>% 
  left_join(zeroing, "idx") %>% #joins two dfs by their row number
  mutate(id = ifelse(is.na(id), 0, id)) #makes id (group) = 0 when not zeroing

#average zero value for each group
zero_avg = zeroes_grouped %>% 
  filter(id != 0) %>%
  group_by(id) %>% 
  summarise(ch1_zeroes = mean(ch1),
            ch2_zeroes = mean(ch2),
            ch1_zero_3sd = 3 * sd(ch1),
            ch2_zero_3sd = 3 * sd(ch2),
            idx = mean(idx)) %>% 
  ungroup() %>% 
  mutate(idx = round(idx))

#interpolate between zeroes and subtract zeroes from measurements
zeroed = zero_flag %>% 
  mutate(idx = 1:nrow(.)) %>% 
  left_join(zero_avg) %>% 
  mutate(ch1_zeroes1 = ifelse(date < "2024-09-16 02:00",ch1_zeroes,NA_real_),
         ch2_zeroes1 = ifelse(date < "2024-09-16 02:00",ch2_zeroes,NA_real_),
         ch1_zeroes2 = ifelse(date > "2024-09-16 02:00",ch1_zeroes,NA_real_),
         ch2_zeroes2 = ifelse(date > "2024-09-16 02:00",ch2_zeroes,NA_real_),
         ch1_zeroes_inter1 = na.approx(ch1_zeroes1,na.rm = F),
         ch2_zeroes_inter1 = na.approx(ch2_zeroes1,na.rm = F),
         ch1_zeroes_inter2 = na.approx(ch1_zeroes2,na.rm = F),
         ch2_zeroes_inter2 = na.approx(ch2_zeroes2,na.rm = F)) %>% 
  fill(ch1_zeroes_inter2,ch2_zeroes_inter2,.direction = "down") %>%
  fill(ch1_zeroes_inter1,ch2_zeroes_inter1,.direction = "up") %>%
  mutate(ch1_zeroed = ifelse(date < "2024-09-16",ch1 - ch1_zeroes_inter1,ch1 - ch1_zeroes_inter2),
         ch2_zeroed = ifelse(date < "2024-09-16",ch2 - ch2_zeroes_inter1,ch2 - ch2_zeroes_inter2))
  
zeroed %>%
  rename(`HONO + interferences (ch1)` = ch1_zeroes,
         `Interferences (ch2)` = ch2_zeroes) %>%
  pivot_longer(c(`HONO + interferences (ch1)`,`Interferences (ch2)`)) %>%
  ggplot(aes(date,value,col = name)) +
  theme_bw() +
  geom_point() +
  labs(x = "Datetime (UTC)",
       y = "Absorbance",
       col = NULL) +
  theme(legend.position = "top") +
  # facet_grid(rows = vars(name),scales = "free") +
  scale_x_datetime(breaks = "1 day",date_labels = "%d %b")

zeroed %>%
  mutate(ch1_zeroed = ifelse(flag == 0,ch1_zeroed,NA_real_),
         ch2_zeroed = ifelse(flag == 0,ch2_zeroed,NA_real_),
         ch1 = ifelse(flag == 0,ch1,NA_real_),
         ch2 = ifelse(flag == 0,ch2,NA_real_)) %>%
  timeAverage("30 sec") %>%
  rename(`HONO + interferences (ch1)` = ch1_zeroed,
         `Interferences (ch2)` = ch2_zeroed) %>%
  pivot_longer(c(`HONO + interferences (ch1)`,`Interferences (ch2)`)) %>%
  ggplot(aes(date,value,col = name)) +
  theme_bw() +
  geom_path() +
  labs(x = "Datetime (UTC)",
       y = "Absorbance",
       col = NULL) +
  theme(legend.position = "top") +
  # facet_grid(rows = vars(name),scales = "free") +
  scale_x_datetime(breaks = "1 day",date_labels = "%d %b")

ggsave('ZA_zeroed_ch1_ch2.png',
       path = "output/analysis_plots",
       width = 30,
       height = 12,
       units = 'cm')





#cals applied in two separate R scripts, one for processing using ZA zeroes, one for processing using nighttime zeroes