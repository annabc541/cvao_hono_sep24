library(tidyverse)
library(lubridate)
library(zoo)
library(oce)
library(openair)

Sys.setenv(TZ = "UTC")

air_masses = read.csv("~/Cape Verde/new_CVAO_sector_%_boxes_1.csv") %>% 
  mutate(date = ymd_hms(X))  %>% 
  rename_with(~ gsub("\\.", " ", .)) %>% 
  filter(date > "2024-09-07" & date < "2024-09-19") %>% 
  select(date,everything(),-X)

air_masses %>% 
  pivot_longer(cols = -c(date)) %>% 
  ggplot(aes(date,value,fill = name)) +
  theme_bw() +
  geom_area() +
  theme(legend.position = "top",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.margin = unit(c(0,0.1,0.1, 0.1), "cm")) +
  labs(x = NULL,
       y = "Air mass composition (%)",
       col = NULL,
       fill = NULL) +
  scale_x_datetime(date_breaks = "1 day",date_labels = "%d %b",
                   expand = expansion(mult = c(0,0))) +
  scale_y_continuous(expand = expansion(mult = c(0,0))) +
  scale_fill_manual(values = c("North Atlantic" = "navy",
                               "South Atlantic" = "steelblue1",
                               "Sahara" = "goldenrod1",
                               "Sahel" = "darkorange3",
                               "West Africa" = "firebrick4",
                               "Central Africa" = "khaki4",
                               "South America" = "darkseagreen1",
                               "North America" = "springgreen4",
                               "Europe" = "darkolivegreen3",
                               "Upwelling" = "deepskyblue3"))

ggsave('CVAO_airmasses_sep24.png',
       path = "output",
       width = 30,
       height = 12,
       units = 'cm')
