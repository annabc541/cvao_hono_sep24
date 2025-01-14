library(tidyverse)
library(janitor)
library(zoo)
library(openair)

Sys.setenv(TZ = 'UTC')

# Constants ---------------------------------------------------------------

kp = 3.3 * 10^-11 #rate constant for oh + no -> hono (from Atkinson et al. 2004) in cm3 molecule-1 s-1
kl = 6 * 10^-12 #rate constant for oh + hono -> h2o + no2 (from Atkinson et al. 2004) in cm3 molecule-1 s-1
dv = 0.3 #deardroff velocity, value used by Simone, in m s-1
oh_molecules_cm3 = 2 * 10 ^6 #used for pss calculations for 2015,2019 and 2020 data

# Functions ---------------------------------------------------------------

ppt_to_molecules_cm3 <- function(x){y = x * 2.46 * 10^19 * 10^-12}
molecules_cm3_to_ppt <- function(x){y = x / ( 2.46 * 10^19 * 10^-12)}

# Reading in data ---------------------------------------------------------

#reading in hono in og time resolution (time corrected) and averaging up to 5 min for data merging
hono_5min = read.csv("output/processed_data/processed_hono_night_zeroes.csv") %>% 
  mutate(date = ymd_hms(date)) %>% 
  timeAverage("5 min") %>% 
  mutate(date = round_date(date,"5 min"))

hono_hourly = read.csv("output/processed_data/hourly_hono_night_zeroes.csv") %>% 
  mutate(date = ymd_hms(date)) 

#reading in nox in og time resolution (5 min) and rounding date for data merging
nox_5min = read.csv("output/processed_data/nox_5min.csv") %>% 
  mutate(date = ymd_hms(date),
         date = round_date(date,"5 min"))

nox_hourly = read.csv("output/processed_data/nox_hourly.csv") %>% 
  mutate(date = ymd_hms(date))

cvao_merge = read.csv("data/20240507_CV_merge.csv") %>% 
  mutate(date = ymd_hms(date),
         date = round_date(date,"1 hour")) %>% 
  clean_names() %>% 
  filter(date > "2023-01-01") %>% 
  mutate(jhono = ifelse(is.na(j_hono) == T,jhono_calc,j_hono),
         jhno3 = ifelse(is.na(j_hno3) == T,jhno3_calc,j_hno3)) %>% 
  select(date,ws,wd,temp = temp_10m_deg_c,rh = rh_10m,
         o3_ppb = o3_ppb_v,co_ppb = co_ppb_v,co2_ppm = co2_revised_ppm_v,
         jhono,jhno3)

air_masses = read.csv("data/new_CVAO_sector_%_boxes_1.csv") %>% 
  mutate(date = ymd_hms(X)) %>% 
  select(date,everything(),-X) %>% 
  janitor::clean_names() %>% 
  filter(date > "2024-08-31" & date < "2024-10-01")

nitrate_ml = read.csv("data/CVAO_Nitrate_Prediction_2024.csv") %>% 
  mutate(date = ymd(date)) %>% 
  select(date,nitrate) %>% 
  filter(date >= "2024-09-01" & date < "2024-10-01")

# Spec rad ----------------------------------------------------------------

#fill NAs with averages from hours where spec rad data is missing when reading data in
#should only be used for nighttime values, since calculated spec rad values are only for daytime
#missing daytime values should be replaced by calculated values
spec_rad_to_fix = cvao_merge %>% 
  select(date,jhono,jhno3) %>% 
  mutate(hour = hour(date))

#find average j-values for each hour
spec_rad_mean = spec_rad_to_fix %>% 
  group_by(hour) %>% 
  summarise(jhono_avg = mean(jhono,na.rm = T),
            jhno3_avg = mean(jhno3,na.rm = T))

#replace NAs with average value for that hour
spec_rad = left_join(spec_rad_to_fix,spec_rad_mean,by = "hour") %>% 
  mutate(jhono = ifelse(is.na(jhono),jhono_avg,jhono),
         jhno3 = ifelse(is.na(jhno3),jhno3_avg,jhno3)) %>% 
  select(-c(jhono_avg,jhno3_avg,hour)) %>%
  arrange(date)

cvao_merge_fixed_spec_rad = cvao_merge %>% 
  select(-c(jhono,jhno3)) %>% 
  left_join(spec_rad)

cvao_merge_fixed_spec_rad %>% 
  filter(date > "2024-09-01" & date < "2024-09-30") %>% 
  pivot_longer(c(jhono,jhno3)) %>%
  ggplot(aes(date,value,col = name)) +
  geom_path() +
  facet_grid(rows = vars(name),scales = "free")

# Merging data & timeseries plot ------------------------------------------------------------

df_list = list(nox_hourly,hono_hourly,cvao_merge_fixed_spec_rad,air_masses)

dat_5min = df_list %>% reduce(left_join,by = "date")
dat_hourly = df_list %>% reduce(left_join,by = "date") %>% 
  mutate(local_pollution = case_when(wd >= 100 & wd <= 340 ~ "Local pollution",
                                     ws <= 2 ~ "Local pollution",
                                     TRUE ~ "Clean"),
         hour = hour(date))

dat_hourly %>% 
  filter(date > "2024-09-08" & date < "2024-09-19") %>% 
  rename(`HONO~(ppt)` = hono_ppt,
         `NO~(ppt)` = no_ppt,
         `NO[2]~(ppt)` = no2_ppt,
         `CO~(ppb)` = co_ppb,
         `CO[2]~(ppm)` = co2_ppm,
         `O[3]~(ppb)` = o3_ppb,
         WS = ws,
         WD = wd) %>% 
  mutate(pollution_date = ifelse(date >= "2024-09-11 05:00" & date < "2024-09-13 13:00",1,0),
         WD_flag = case_when(WD <= 90 ~ "< 90",
                        WD > 90 & WD < 100 ~ "Between 90 and 100",
                        WD >= 100 & WD <= 340 ~ "Local pollution",
                        WD > 340 ~ "> 340")) %>% 
  filter(pollution_date == 0) %>%
  timeAverage("1 hour") %>%
  pivot_longer(c(`HONO~(ppt)`,`NO~(ppt)`,`NO[2]~(ppt)`,`CO~(ppb)`,`O[3]~(ppb)`,`CO[2]~(ppm)`)) %>% 
  # pivot_longer(c(`HONO~(ppt)`,`NO~(ppt)`,`NO[2]~(ppt)`,`CO~(ppb)`,`O[3]~(ppb)`,`CO[2]~(ppm)`)) %>% 
  ggplot(aes(date,value,col = name)) +
  theme_bw() +
  geom_path(group = 1,linewidth = 1) +
  facet_grid(rows = vars(name),scales = "free",labeller = label_parsed) +
  # scale_colour_manual(values = c("steelblue1","darkorange")) +
  scale_colour_manual(values = c("darkorange","steelblue1","navy","firebrick4","springgreen4","goldenrod1")) +
  labs(x = NULL,
       y = "Mixing ratio",
       col = NULL) +
  theme(legend.position = "None") +
  NULL

# ggsave('hono_nox_o3_co_co2_hourly_timeseries_filtered2.png',
#        path = "output/science_plots",
#        width = 31,
#        height = 14,
#        units = 'cm')

# Diurnals ----------------------------------------------------------------

diurnal = dat_hourly %>%
  rename(HONO = hono_ppt,NO = no_ppt,`NO[2]` = no2_ppt) %>% 
  filter(date > "2024-09-08 00:00" & date < "2024-09-11",
         local_pollution == "Clean") %>%
  timeVariation(pollutant = c("HONO","NO","NO[2]"))

diurnal_dat = diurnal$data$hour

diurnal_dat %>% 
  ggplot(aes(hour,Mean,col = variable)) +
  theme_bw() +
  geom_path(size = 0.8) +
  facet_wrap(~variable,scales = "free",labeller = label_parsed) +
  scale_colour_manual(values = c("darkorange","steelblue1","navy")) +
  labs(x = "Hour of day (UTC)",
       y = " Mixing ratio (ppt)",
       color = NULL) +
  scale_x_continuous(breaks = c(0,4,8,12,16,20)) +
  # ylim(-0.5,12) +
  theme(legend.position = "None")

# ggsave('hono_nox_diurnal_after_pollution.png',
#        path = "output/science_plots",
#        width = 30,
#        height = 12,
#        units = 'cm')

# PSS without nitrate -----------------------------------------------------

#dont have a measurement for oh, so just using oh = 2 * 10^6 molecules cm-3 (average from Feb 23)

hourly_pss_without_nitrate = dat_hourly %>%
  mutate(hour = hour(date)) %>% 
  filter(
    # hour >= 11 & hour <= 15, #only looking at daytime values
         date > "2024-09-08" & date < "2024-09-19") %>%  
  mutate(lifetime = case_when(between(hour,11,15) ~ 1/jhono, #& date < "2024-09-11"
                              # date>="2024-09-11 15:00"& date <="2024-09-12 15:00" ~ 1/jhono,
                              # between(hour,11,15) & date > "2024-09-13" ~ 1/jhono,
                              TRUE ~ NA_real_)) %>% 
  fill(lifetime) %>% 
  mutate(h = lifetime * dv,
         kp = 3.3*10^-11*((temp+273.15)/300)^-0.3,
         kdep = 0.01/h,
         production_without_nitrate = kp * oh_molecules_cm3 * ppt_to_molecules_cm3(no_ppt),
         loss = (jhono + (kl*oh_molecules_cm3) + kdep) * ppt_to_molecules_cm3(hono_ppt),
         hono_without_nitrate = molecules_cm3_to_ppt((production_without_nitrate) / (jhono + (kl*oh_molecules_cm3) + kdep)))

hourly_pss_without_nitrate %>% 
  rename("Measured HONO" = hono_ppt,
         "Measured NO" = no_ppt,
         "PSS HONO (without nitrate)" = hono_without_nitrate) %>% 
  pivot_longer(c("Measured HONO","Measured NO","PSS HONO (without nitrate)")) %>%
  # filter(date > "2024-09-11" & date < "2024-09-13") %>% 
  ggplot(aes(date,value,col = name)) +
  theme_bw() +
  geom_path(size = 1) +
  labs(x = NULL,
       y = "Mixing ratio (ppt)",
       col = NULL) +
  theme(legend.position = "top") +
  scale_colour_manual(values = c("steelblue1","navy","darkorange")) +
  # scale_x_datetime(date_breaks = "4 hours",date_labels = "%b %d %H:%M") +
  NULL

ggsave('hono_no_pss_wo_nitrate.png',
       path = "output/science_plots/pss_ml_nitrate",
       width = 30,
       height = 12,
       units = 'cm')


# PSS with nitrate (ML) ---------------------------------------------------

dat_hourly_nitrate = dat_hourly %>% 
  left_join(nitrate_ml) %>% 
  fill(nitrate) %>% 
  mutate(nitrate_molecules_cm3 = (nitrate * 10^-12 *6.022 * 10^23)/62.004,
         nitrate_nmol_m3 = (nitrate_molecules_cm3 * 10^15)/(6.022*10^23))

hourly_pss = dat_hourly_nitrate %>%
  left_join(nitrate_ml) %>% 
  fill(nitrate) %>% 
  filter(hour >= 11 & hour <= 15) %>%  #only looking at daytime values
  mutate(lifetime = 1/jhono,
         h = lifetime * dv,
         kdep = 0.01/h,
         production_without_nitrate = kp * oh_molecules_cm3 * ppt_to_molecules_cm3(no_ppt),
         loss = (jhono + (kl*oh_molecules_cm3) + kdep) * ppt_to_molecules_cm3(hono_ppt),
         missing_production = (loss - production_without_nitrate), #molecule cm-3 s-1
         f_calc = missing_production/(jhno3 * nitrate_molecules_cm3),
         f_para = 103706014.61/(1 + (83211.37 * nitrate_nmol_m3)), #matt's parameterisation
         # hono_para = molecules_cm3_to_ppt((production_without_nitrate + (jhno3 * nitrate_molecules_cm3 * f_para))
         # / (jhono + (kl*oh_molecules_cm3) + kdep))
  )

daily_f = hourly_pss %>% 
  timeAverage("1 day") %>% 
  select(date,f_para,f_calc,lifetime)

daily_pss = dat_hourly_nitrate %>%
  left_join(daily_f,by = "date") %>% 
  fill(f_para,f_calc,lifetime,.direction = "down") %>% 
  mutate(h = lifetime * dv,
         kdep = 0.01/h,
         production_without_nitrate = kp*oh_molecules_cm3 * ppt_to_molecules_cm3(no_ppt),
         loss = (jhono + (kl*oh_molecules_cm3) + kdep) * ppt_to_molecules_cm3(hono_ppt),
         missing_production = (loss - production_without_nitrate), #molecule cm-3 s-1
         hono_para = molecules_cm3_to_ppt((production_without_nitrate + (jhno3 * nitrate_molecules_cm3 * f_para))
                                          / (jhono + (kl*oh_molecules_cm3) + kdep)),
         hono_without_nitrate = molecules_cm3_to_ppt((production_without_nitrate)
                                                     / (jhono + (kl*oh_molecules_cm3) + kdep))) 
daily_pss %>% 
  filter(date > "2024-09-06" & date < "2024-09-19") %>%
  mutate(hono_para = ifelse(hono_para < 0,0,hono_para),
         hono_without_nitrate = ifelse(hono_without_nitrate < 0,0,hono_without_nitrate)) %>% 
  pivot_longer(c(hono_ppt,hono_para,hono_without_nitrate)) %>%
  ggplot(aes(date,value,col = name)) +
  theme_bw() +
  geom_path(size = 1) +
  labs(x = "Datetime (UTC)",
       y = "HONO (ppt)",
       col = NULL) +
  scale_x_datetime(breaks = "2 day",date_labels = "%b %d") +
  scale_colour_manual(values = c("darkorange","steelblue1","navyblue"),
                      labels = c("PSS HONO with f","Measured HONO","PSS HONO nitrate")) +
  theme(legend.position = "top") +
  ylim(-1,45) +
  NULL

# ggsave('hono_pss_with_and_without_nitrate.png',
#        path = "output/science_plots/pss_ml_nitrate",
#        width = 30,
#        height = 12,
#        units = 'cm')
