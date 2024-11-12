library(tidyverse)
library(zoo)
library(openair)

air_masses = read.csv("data/new_CVAO_sector_%_boxes_1.csv") %>% 
  mutate(date = ymd_hms(X)) %>% 
  clean_names() %>% 
  timeAverage("1 day") %>% 
  select(date,everything())

nitrate1 = read.csv("~/Cape Verde/peroxy_campaign/data/aerosol_data/nitrate_ammonium_CVAO_12-19.csv") %>% 
  clean_names() %>% 
  mutate(date = mdy_hm(start_local_time),
         date = round_date(date,"1 day")) %>% 
  timeAverage("1 day") %>% 
  select(date,nitrate = nitrate_mg_m)

nitrate2 = read.csv("~/Cape Verde/peroxy_campaign/data/aerosol_data/cvao_aerosols23.csv") %>% 
  clean_names() %>% 
  mutate(date = dmy_hm(start)) %>% 
  timeAverage("1 day") %>% 
  select(date,nitrate)

nitrate_air_masses = bind_rows(nitrate1,nitrate2) %>% 
  arrange(date) %>% 
  full_join(air_masses)

write.csv(nitrate_air_masses,"data/nitrate_air_masses.csv",row.names = F)

