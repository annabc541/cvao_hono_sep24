gas_flow_dat = read.csv("raw_data/gas_flow_cvao.csv") %>% 
  mutate(date = glue::glue("{date} {time}"),
         date = dmy_hm(date)) %>% 
  select(date,gas_flow)

gas_flow_dat %>% 
  mutate(hour = hour(date),
         day = day(date)) %>% 
  ggplot(aes(hour,gas_flow,col = as.character(day))) +
  geom_point() +
  theme_bw()

ggsave('gas_flow_hour.png',
       path = "output/analysis_plots",
       width = 15,
       height = 12,
       units = 'cm')
