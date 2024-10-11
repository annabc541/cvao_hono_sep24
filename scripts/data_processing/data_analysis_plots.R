
#plotting comparison between ZA and nighttime zeroes
zeroed %>% 
  select(date,ch1_zeroes,ch2_zeroes) %>% 
  left_join(night_zeroed,by = "date") %>% 
  select(date:ch2_zeroes,ch1_zeroes_night,ch2_zeroes_night) %>% 
  rename(Night = ch2_zeroes_night,
         ZA = ch2_zeroes) %>% 
  pivot_longer(c(Night,ZA)) %>% 
  filter(is.na(value) == FALSE) %>% 
  ggplot(aes(date,value)) +
  theme_bw() +
  geom_path(aes(col = name),size = 1) +
  geom_point(col = "black") +
  # facet_grid(rows = vars(name),scales = "free") +
  scale_x_datetime(date_breaks = "1 day",date_labels = "%d/%m") +
  labs(x = NULL,
       y = NULL,
       col = "Zero method") +
  theme(legend.position = "top")

#plotting ch1 and ch2 ZA zeroes on the same plot, for comparison with zeroes from 2023 campaign
#code can just be copied and used with 2023 data if comparison needs to be plotted up again
zeroed %>% 
  rename(`HONO + interferences (ch1)` = ch1_zeroes,
         `Interferences (ch2)` = ch2_zeroes) %>%
  pivot_longer(c(`HONO + interferences (ch1)`,`Interferences (ch2)`)) %>%
  filter(is.na(value) == FALSE) %>% 
  ggplot(aes(date,value)) +
  theme_bw() +
  geom_path(aes(col = name),size = 1) +
  geom_point(col = "black") +
  # facet_grid(rows = vars(name),scales = "free") +
  scale_x_datetime(date_breaks = "1 day",date_labels = "%d %b %y") +
  labs(x = NULL,
       y = NULL,
       col = NULL) +
  theme(legend.position = "top")

# ggsave('ch2_zero_comparison.png',
#        path = "output/analysis_plots",
#        width = 15,
#        height = 12,
#        units = 'cm')
