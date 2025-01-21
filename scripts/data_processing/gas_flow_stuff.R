library(tidyverse)
library(lubridate)
library(zoo)
library(oce)
library(openair)

Sys.setenv(TZ = "UTC")

# Field measurements ------------------------------------------------------

gas_flow_dat = read.csv("raw_data/gas_flow_cvao.csv") %>% 
  mutate(date = glue::glue("{date} {time}"),
         date = dmy_hm(date),
         gas_flow_corr = 0.9*gas_flow) %>% 
  select(date,gas_flow,gas_flow_corr)

gas_flow_mean_cvao = mean(gas_flow_dat$gas_flow_corr,na.rm = F)

gas_flow_dat %>% 
  rename("Measured gas flow" = gas_flow,
         "Corrected gas flow" = gas_flow_corr) %>% 
  pivot_longer(-date) %>% 
  ggplot(aes(date,value,col = name)) +
  geom_point(size = 2) +
  scale_colour_manual(values = c("steelblue1","darkorange")) +
  # scale_x_datetime(date_breaks = "12 hours",date_labels = "%d/%m %H:%M") +
  theme_bw() +
  labs(y = expression(Gas~flow~(mL~min^{-1})),
       x = "Date",
       col = NULL) +
  theme(text = element_text(size =  16),
        legend.position = "top")

gas_flow_dat %>% 
  mutate(hour = hour(date),
         day = day(date)) %>% 
  ggplot(aes(factor(hour,levels = c(6:19)),gas_flow_corr)) +
  geom_boxplot(col = "steelblue1") +
  # geom_point(size = 2,col = "navy") +
  # scale_x_datetime(date_breaks = "12 hours",date_labels = "%d/%m %H:%M") +
  theme_bw() +
  labs(y = expression(Gas~flow~(mL~min^{-1})),
       x = "Hour of day") +
  # scale_x_continuous(breaks = c(7:18)) +
  theme(text = element_text(size =  16))

#checking if there's a relationship between gas flow and outside temperature
cvao_merge = read.csv("~/Cape Verde/20240507_CV_merge.csv") %>% 
  mutate(date = ymd_hms(date),
         date = round_date(date,"1 hour")) %>% 
  filter(date >= "2024-09-12 13:00" & date <= "2024-09-17 17:00") %>% 
  select(date,temp = TEMP_10M_degC)

gas_flow_dat %>% 
  timeAverage("1 hour") %>% 
  left_join(cvao_merge) %>% 
  ggplot(aes(temp,gas_flow_corr)) +
  theme_bw() +
  geom_point(size = 2) +
  labs(x = "Ambient temperature (Celsius)",
       y = expression(Corrected~gas~flow~(mL~min^{-1}))) +
  # geom_smooth(method = "lm") +
  scale_colour_viridis_c()

ggsave('cvao_gas_flow_corr_temp.png',
       path = "output/analysis_plots/gas_flow",
       width = 15,
       height = 12,
       units = 'cm')

# Gas calibrations --------------------------------------------------------

gas_flows = read.csv("raw_data/mfc_pre_campaign.csv")

gas_flows %>% 
  pivot_longer(-set) %>% 
  ggplot(aes(set,value,col = name)) +
  theme_bw() +
  geom_point() +
  geom_smooth(se = F,method = "lm") +
  geom_abline(col = "red") +
  labs(x = "Setpoint",
       y = "Measured flow",
       col = NULL) +
  theme(legend.position = "top")

# ggsave('gas_flow_cals.png',
#        path = "output/analysis_plots/gas_flow",
#        width = 19,
#        height = 14,
#        units = 'cm')

model <- lm(during_campaign ~ set, data = gas_flows)
summary(model)
slope_gas <- summary(model)$coefficients[2,1]


# Lab logging -------------------------------------------------------------

lab_flow = read.csv("data/lopap_gas_flow_lab_check.csv") %>% 
  mutate(date = ymd_hms(Time),
         date = round_date(date,"1 minute")) %>% 
  mutate_at(vars(volumetric_flow:pressure),as.numeric) %>% 
  select(date,everything(),-Time)

gas_flow = mean(lab_flow$volumetric_flow,na.rm = T)

lab_flow %>% 
  timeAverage("15 min") %>%
  ggplot(aes(date,volumetric_flow,col = temp)) +
  geom_path(size = 0.8) +
  scale_colour_viridis_c() +
  # scale_x_datetime(date_breaks = "12 hours",date_labels = "%b %d %H:%M") +
  theme_bw() +
  geom_vline(xintercept = as.POSIXct("2025-01-10 16:00"),col = "red") +
  labs(x = NULL,
       y = expression(Gas~flow~(mL~min^{-1})),
       col = "Temp") +
  theme(text = element_text(size =  16))

# ggsave('gas_flow_lab_measurement.png',
#        path = "~/Writing/Thesis/Chapter 4 (HONO in CVAO)/Images",
#        width = 30,
#        height = 14,
#        units = 'cm')
