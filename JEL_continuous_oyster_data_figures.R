# Created by Selina Cheng
# Last modified 06 Sept 2024
# Looking at data from continuous experimental setup at JEL

# Load libraries
library(tidyverse)
library(data.table)
library(lubridate)
library(patchwork)

# List source dir
new_combined_dir <- "/Users/sel/Library/CloudStorage/OneDrive-USNH/Oyster Biosensor/0_InProgressExperiments/2023 - 2024 Continuous system on oysters and mussels/Data combined/Current version"

# Read in 1 minute data
dat_minute <- fread(list.files(new_combined_dir, pattern = "1min", full.names = T, include.dirs = F)) %>%
  pivot_longer(cols = contains("ID"), names_to = "oysterid", values_to = "volts")

# Read in 15 minute data
dat_15min <- fread(list.files(new_combined_dir, pattern = "15min", full.names = T, include.dirs = F)) %>%
  pivot_longer(cols = contains("ID"), names_to = "oysterid", values_to = "volts")

# Create plot
ggplot(data = dat_15min, aes(x = fifteenmin_floor, y = volts, color = oysterid)) +
  geom_line()

# Remove some oysters that have troubling behavior
dat_15min %>%
  filter(oysterid %in% c("ID11", "ID9", "ID2", "ID5", "ID8", "ID1") == F) %>%
  ggplot(aes(x = fifteenmin_floor, y = volts, color = oysterid))+
  geom_line()

# Look at some specific oysters that seem pretty consistent
dat_15min %>%
  filter(oysterid %in% c("ID7", "ID6", "ID12")) %>%
  ggplot(aes(x = fifteenmin_floor, y = volts, color = oysterid))+
  geom_line()

# Look at some specific oysters over the course of a month
dat_15min %>%
  filter(oysterid %in% c("ID7", "ID6", "ID12"),
         month(fifteenmin_floor) == 7) %>%
  ggplot(aes(x = fifteenmin_floor, y = volts, color = oysterid))+
  geom_line()

# Does the 1 min data look different?
dat_minute %>%
  filter(oysterid %in% c("ID7", "ID6", "ID12"),
         month(minute_floor) == 7) %>%
  ggplot(aes(x = minute_floor, y = volts, color = oysterid))+
  geom_line()


# Normalize data to 0-1 scale
dat_15min_norm <- dat_15min %>%
  group_by(oysterid) %>%
  mutate(norm_volts = (volts - min(volts)) / (max(volts) - min(volts)))

dat_15min_norm %>%
  filter(oysterid %in% c("ID7", "ID6", "ID12"),
         month(fifteenmin_floor) == 7) %>%
  ggplot(aes(x = fifteenmin_floor, y = norm_volts, color = oysterid))+
  geom_line()

dat_1min_norm <- dat_minute %>%  
  group_by(oysterid) %>%
  mutate(norm_volts = (volts - min(volts)) / (max(volts) - min(volts)))
  
dat_1min_norm %>%
  filter(oysterid %in% c("ID7", "ID6", "ID12"),
         date(minute_floor) == "2024-07-01") %>%
  ggplot(aes(x = minute_floor, y = norm_volts, color = oysterid))+
  geom_line()





