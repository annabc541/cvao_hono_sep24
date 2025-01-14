zero_comparison = night_zeroed %>% 
  select(date,
         ch1_night_zeroes = ch1_zeroes,ch2_night_zeroes = ch2_zeroes,
         ch1_night_zeroes_inter = ch1_zeroes_inter,ch2_night_zeroes_inter = ch2_zeroes_inter) %>% 
  left_join(zeroed,by = "date") %>% 
  select(date,flag,ch1,ch2,ch1_night_zeroes:ch2_night_zeroes_inter,
         ch1_zeroes,ch2_zeroes,ch1_zeroes_inter,ch2_zeroes_inter)

zero_comparison %>% 
  mutate(hour = hour(date),
         flag = ifelse(hour <= 4 | hour >= 21,"Nighttime","Daytime")) %>% 
  rename(`Night zeroing` = ch1_night_zeroes,`ZA zeroing` = ch1_zeroes) %>% 
  pivot_longer(c(`Night zeroing`,`ZA zeroing`)) %>% 
  ggplot(aes(date,value,col = name)) +
  theme_bw() +
  labs(x = "Datetime (UTC)",
       y = "Absorbance",
       col = "Zero method") +
  theme(legend.position = "top") +
  geom_point(size = 2) +
  scale_x_datetime(breaks = "1 day",date_labels = "%d/%m")

ggsave('ch1_zero_comparison.png',
       path = "output/analysis_plots",
       width = 15,
       height = 12,
       units = 'cm')

zero_comparison = night_zeroed %>% 
  select(date,
         ch1_night_zeroes = ch1_zeroes,ch2_night_zeroes = ch2_zeroes,
         ch1_night_zeroed = ch1_zeroed,ch2_night_zeroed = ch2_zeroed) %>% 
  left_join(zeroed,by = "date") %>% 
  select(date,flag,ch1,ch2,ch1_night_zeroes:ch2_night_zeroed,
         ch1_zeroes,ch2_zeroes,ch1_zeroed,ch2_zeroed) %>% 
  mutate(ch1_zeroed = ifelse(flag == 0,ch1_zeroed,NA_real_),
         ch2_zeroed = ifelse(flag == 0,ch2_zeroed,NA_real_),
         ch1_night_zeroed = ifelse(flag == 0,ch1_night_zeroed,NA_real_),
         ch2_night_zeroed = ifelse(flag == 0,ch2_night_zeroed,NA_real_))

zero_comparison %>% 
  timeAverage("30 sec") %>% 
  pivot_longer(c(ch2_night_zeroes,ch2_zeroes)) %>% 
  ggplot(aes(date,value,col = name)) +
  geom_point()
