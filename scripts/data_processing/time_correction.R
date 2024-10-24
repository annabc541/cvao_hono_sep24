#calculating the time delay and the time response

flagged_data %>% 
  mutate(day = day(date)) %>% 
  pivot_longer(c(ch1,ch2)) %>% 
  # filter(flag == 1) %>%
  filter(date > "2024-09-18 07:03" & date < "2024-09-18 07:30") %>% 
  # filter(day == 8) %>% 
  ggplot(aes(date,value)) +
  geom_point() +
  facet_grid(rows = vars(name),scales = "free")

# values = zeroes_grouped %>% 
#   select(-c(idx,flag)) %>% 
#   filter(zeroing != 0) %>% 
#   filter(id == 6)

finding_90 = flagged_data %>% 
  mutate(day = day(date)) %>% 
  filter(date > "2024-09-17 12:45" & date < "2024-09-17 13:17")

tail(finding_90)

variation = abs(0.5939 - 0.53)
ninty_variation = variation*0.9
ninty_variation2 = 0.5939 * 0.9

ninty_percent = 0.5939 - ninty_variation


