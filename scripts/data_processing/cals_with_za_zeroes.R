#ZA zeroing

# Calibration 0 -----------------------------------------------------------

#date > "2024-09-09 14:53" & date < "2024-09-09 15:05" zero for first cal
#date > "2024-09-09 15:47" & date < "2024-09-09 16:04" first cal

#cal parameters
# time_corr = 22.75 * 60
liquid_flow1 = 5/27.73 
liquid_flow2 = 5/29.73

gas_flow = 890
sampling_efficiency0 = 1 - exp(-7768.943*1/gas_flow-0.116560784)

conc_cal = 1000/100000 #standard conc / dilution factor
hono_cal_conc_ch1 = conc_cal/1000 * liquid_flow1/1000 /46*6.022*10^23/(2.46*10^19* gas_flow) * 10^12
hono_cal_conc_ch2 = conc_cal/1000 * liquid_flow2/1000 /46*6.022*10^23/(2.46*10^19* gas_flow) * 10^12

cal0 = zeroed %>% 
  select(date,ch1_zeroed,ch2_zeroed) %>% 
  mutate(cal_zero_ch1 = ifelse(date > "2024-09-09 14:53" & date < "2024-09-09 15:05",ch1_zeroed,NA),
         cal_zero_ch2 = ifelse(date > "2024-09-09 14:53" & date < "2024-09-09 15:05",ch2_zeroed,NA),
         cal_ch1 = ifelse(date > "2024-09-09 15:47" & date < "2024-09-09 16:04",ch1_zeroed,NA),
         cal_ch2 = ifelse(date > "2024-09-09 15:47" & date < "2024-09-09 16:04",ch2_zeroed,NA)) %>%  
  summarise(cal_zero_ch1 = mean(cal_zero_ch1,na.rm = T),
            cal_zero_ch2 = mean(cal_zero_ch2,na.rm = T),
            cal_ch1 = mean(cal_ch1,na.rm = T),
            cal_ch2 = mean(cal_ch2,na.rm = T))

cal_df1 = data.frame(y = c(0,hono_cal_conc_ch1),
                     x = c(cal0$cal_zero_ch1,cal0$cal_ch1))

model_cal1 = lm(y ~ x,cal_df1)
# intercept_mfc = summary(model_mfc_cal)$coefficients[1,1]
slope_cal0_1 = summary(model_cal1)$coefficients[2,1]

cal_df2 = data.frame(y = c(0,hono_cal_conc_ch2),
                     x = c(cal0$cal_zero_ch2,cal0$cal_ch2))

model_cal2 = lm(y ~ x,cal_df2)
# intercept_mfc = summary(model_mfc_cal)$coefficients[1,1]
slope_cal0_2 = summary(model_cal2)$coefficients[2,1]

# Calibration 1 -----------------------------------------------------------

#date > "2024-09-09 14:53" & date < "2024-09-09 15:05" zero for first cal
#date > "2024-09-09 15:47" & date < "2024-09-09 16:04" first cal

#cal parameters
# time_corr = 22.75 * 60
liquid_flow1 = 5/27.73 
liquid_flow2 = 5/29.73

gas_flow = 1190
sampling_efficiency1 = 1 - exp(-7768.943*1/gas_flow-0.116560784)

conc_cal = 1000/100000 #standard conc / dilution factor
hono_cal_conc_ch1 = conc_cal/1000 * liquid_flow1/1000 /46*6.022*10^23/(2.46*10^19* gas_flow) * 10^12
hono_cal_conc_ch2 = conc_cal/1000 * liquid_flow2/1000 /46*6.022*10^23/(2.46*10^19* gas_flow) * 10^12

cal1 = zeroed %>% 
  select(date,ch1_zeroed,ch2_zeroed) %>% 
  mutate(cal_zero_ch1 = ifelse(date > "2024-09-13 14:22" & date < "2024-09-13 14:35",ch1_zeroed,NA),
         cal_zero_ch2 = ifelse(date > "2024-09-13 14:22" & date < "2024-09-13 14:35",ch2_zeroed,NA),
         cal_ch1 = ifelse(date > "2024-09-13 15:05" & date < "2024-09-13 15:23",ch1_zeroed,NA),
         cal_ch2 = ifelse(date > "2024-09-13 15:05" & date < "2024-09-13 15:23",ch2_zeroed,NA)) %>%  
  summarise(cal_zero_ch1 = mean(cal_zero_ch1,na.rm = T),
            cal_zero_ch2 = mean(cal_zero_ch2,na.rm = T),
            cal_ch1 = mean(cal_ch1,na.rm = T),
            cal_ch2 = mean(cal_ch2,na.rm = T))

cal_df1 = data.frame(y = c(0,hono_cal_conc_ch1),
                     x = c(cal1$cal_zero_ch1,cal1$cal_ch1))

model_cal1 = lm(y ~ x,cal_df1)
# intercept_mfc = summary(model_mfc_cal)$coefficients[1,1]
slope_cal1 = summary(model_cal1)$coefficients[2,1]

cal_df2 = data.frame(y = c(0,hono_cal_conc_ch2),
                     x = c(cal1$cal_zero_ch2,cal1$cal_ch2))

model_cal2 = lm(y ~ x,cal_df2)
# intercept_mfc = summary(model_mfc_cal)$coefficients[1,1]
slope_cal2 = summary(model_cal2)$coefficients[2,1]

# Calibration 2-------------------------------------------------------------

#cal parameters
gas_flow = 1350
sampling_efficiency2 = 1 - exp(-7768.943*1/gas_flow-0.116560784)

liquid_flow1 = 5/27.73 
liquid_flow2 = 5/29.73

conc_cal = 1000/100000 #standard conc / dilution factor
hono_cal_conc_ch1 = conc_cal/1000 * liquid_flow1/1000 /46*6.022*10^23/(2.46*10^19* gas_flow) * 10^12
hono_cal_conc_ch2 = conc_cal/1000 * liquid_flow2/1000 /46*6.022*10^23/(2.46*10^19* gas_flow) * 10^12

cal2 = zeroed %>% 
  select(date,ch1_zeroed,ch2_zeroed) %>% 
  mutate(cal_zero_ch1 = ifelse(date > "2024-09-14 08:17" & date < "2024-09-14 09:00",ch1_zeroed,NA),
         cal_zero_ch2 = ifelse(date > "2024-09-14 08:17" & date < "2024-09-14 09:00",ch2_zeroed,NA),
         cal_ch1 = ifelse(date > "2024-09-14 09:17" & date < "2024-09-14 09:35",ch1_zeroed,NA),
         cal_ch2 = ifelse(date > "2024-09-14 09:17" & date < "2024-09-14 09:35",ch2_zeroed,NA)) %>% 
  summarise(cal_zero_ch1 = mean(cal_zero_ch1,na.rm = T),
            cal_zero_ch2 = mean(cal_zero_ch2,na.rm = T),
            cal_ch1 = mean(cal_ch1,na.rm = T),
            cal_ch2 = mean(cal_ch2,na.rm = T))

cal2_df1 = data.frame(y = c(0,hono_cal_conc_ch1),
                      x = c(cal2$cal_zero_ch1,cal2$cal_ch1))

model_cal2_1 = lm(y ~ x,cal2_df1)
# intercept_mfc = summary(model_mfc_cal)$coefficients[1,1]
slope_cal2_1 = summary(model_cal2_1)$coefficients[2,1]

cal2_df2 = data.frame(y = c(0,hono_cal_conc_ch2),
                      x = c(cal2$cal_zero_ch2,cal2$cal_ch2))

model_cal2_2 = lm(y ~ x,cal2_df2)
# intercept_mfc = summary(model_mfc_cal)$coefficients[1,1]
slope_cal2_2 = summary(model_cal2_2)$coefficients[2,1]

# Applying cals------------------------------------------------------------

dat_calibrated0 = zeroed %>%
  filter(date < "2024-09-11") %>%
  # select(-c(night_zero,id,idx)) %>%
  mutate(ch1_ppt = ch1_zeroed * slope_cal0_1,
         ch2_ppt = ch2_zeroed * slope_cal0_2,
         hono = ppt(ch1_ppt,ch2_ppt,sampling_efficiency0))

dat_calibrated1 = zeroed %>% 
  filter(date > "2024-09-11" & date < "2024-09-14 08:17") %>%
  # select(-c(night_zero,id,idx)) %>% 
  mutate(ch1_ppt = ch1_zeroed * slope_cal1,
         ch2_ppt = ch2_zeroed * slope_cal2,
         hono = ppt(ch1_ppt,ch2_ppt,sampling_efficiency1))

dat_calibrated2 = zeroed %>% 
  filter(date > "2024-09-14 08:17") %>%
  # select(-c(night_zero,id,idx)) %>% 
  mutate(ch1_ppt = ch1_zeroed * slope_cal2_1,
         ch2_ppt = ch2_zeroed * slope_cal2_2,
         hono = ppt(ch1_ppt,ch2_ppt,sampling_efficiency2))

dat_za = bind_rows(dat_calibrated0,dat_calibrated1,dat_calibrated2) %>% 
  arrange(date) %>% 
  mutate(hono_za = ifelse(flag == 0,hono,NA_real_))

hono_hourly_za = dat_za %>% 
  select(date,hono_za) %>% 
  timeAverage("1 hour")

dat = left_join(hono_hourly_za,hono_hourly_night)

dat %>%
  # mutate(day = day(date)) %>% 
  # mutate(ch1_ppt = ifelse(flag == 0,ch1_ppt,NA_real_),
  #        ch2_ppt = ifelse(flag == 0,ch2_ppt,NA_real_)) %>% 
  # timeAverage("1 hour") %>%
  # filter(day == 12) %>%
  # filter(date > "2024-09-15 07:45" & date < "2024-09-15 08:30") %>%
  rename("Night zeroing" = hono_night,
         "ZA zeroing" = hono_za) %>% 
  pivot_longer(c("Night zeroing","ZA zeroing")) %>%
  ggplot(aes(date,value,col = name)) +
  theme_bw() +
  geom_path(size = 1) +
  labs(x = NULL,
       y = "HONO (ppt)",
       col = NULL) +
  theme(legend.position = "top")+
  scale_x_datetime(date_breaks = "1 day",date_labels = "%d %b") +
  # facet_grid(rows = vars(name),scales = "free") +
  NULL

# ggsave('initial_hono_night_ZA_timeseries.png',
#        path = "output/analysis_plots",
#        width = 30,
#        height = 12,
#        units = 'cm')
