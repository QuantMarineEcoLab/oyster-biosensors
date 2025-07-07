# Read in manually cleaned data, remove maintenance times
# Round to 1min
# Selina Cheng
# Load libraries
library(tidyverse)
library(lubridate)
library(data.table)

mean_rm <- function(x){mean(x, na.rm = T)}

# Read in clean data
oyster_dat <- fread("~/Library/CloudStorage/OneDrive-USNH/Oyster Biosensor/0_InProgressExperiments/2024 JEL field/data/rockweed experiment_manual clean.txt")

# Remove times when there was maintenance happening
maintenance <- fread("~/Library/CloudStorage/OneDrive-USNH/Oyster Biosensor/0_InProgressExperiments/2024 JEL field/data/schema/sensor_maintenance_long.csv")

maintenance <- maintenance %>%
  select(-V7)

# Create date and time for maintenance
maintenance <- maintenance %>%
  mutate(start_timestamp = lubridate::ymd_hm(paste0(date, start_time_est), tz = "EST"),
         end_timestamp = lubridate::ymd_hm(paste0(date, end_time_est), tz = "EST"))

# Reformat date time in oyster_dat
oyster_dat <- oyster_dat %>%
  rename(timestamp_est_i = "timestamp_est",
         start_date_i = "start_date",
         end_date_i = "end_date")

oyster_dat <- oyster_dat %>%
  mutate(timestamp_est = as.POSIXct(timestamp_est_i, format = "%Y/%m/%d %I:%M:%S %p", 
                                    tz = "EST"))

oyster_dat <- oyster_dat %>%
  mutate(start_date = as.POSIXct(start_date_i, format = "%Y/%m/%d %I:%M:%S %p", 
                                 tz = "EST"))

oyster_dat <- oyster_dat %>%         
  mutate(end_date = as.POSIXct(end_date_i, format = "%Y/%m/%d %I:%M:%S %p", 
                               tz = "EST"))

# Join based on date ranges
start <- Sys.time()
oyster_dat <- left_join(oyster_dat, maintenance, 
                          join_by(vars == affected_sensor, timestamp_est >= start_timestamp,
                                  timestamp_est < end_timestamp))
end <- Sys.time()
end-start

# Remove maintenance times
oyster_dat <- oyster_dat %>%
  filter(is.na(category)) %>%
  select(-c(16:22))

# Average data to 1 min
# Round timestamps to average oyster gape data
oyster_dat <- oyster_dat %>%
  # Create new column for minute flooring
  mutate(minute_floor = floor_date(timestamp_est, "minute"),
         minute_floor_unixtime = unixtime - (unixtime %% 60))

# Create a 1-min rounded dataset
start <- Sys.time()
oyster_dat_1min <- oyster_dat %>%
  group_by(minute_floor_unixtime, minute_floor, oyster_id, vars,
           tidal_zone, treatment, sensor) %>%
  summarise(mean_voltage = mean_rm(value))
end <- Sys.time()
end-start

# Write file
min_date <- min(date(oyster_dat_1min$minute_floor))
max_date <- max(date(oyster_dat_1min$minute_floor))

# Check timestamps again (are there any midnight times that are dropped?)
which(!grepl("....-..-.. ..:..:..", oyster_dat_1min$minute_floor))

oyster_dat_1min$minute_floor <- format(oyster_dat_1min$minute_floor, format = "%Y-%m-%d %H:%M:%S")

# Check timestamps again (are there any midnight times that are dropped?)
which(!grepl("....-..-.. ..:..:..", oyster_dat_1min$minute_floor))

filename <- file.path("~/Library/CloudStorage/OneDrive-USNH/Oyster Biosensor/0_InProgressExperiments/2024 JEL field/data", paste0("rockweed_manual_clean_1min_", min_date, "_TO_", max_date, ".csv"))

oyster_dat_1min %>%
  # pivot_wider(id_cols = minute_floor, names_from = "oyster_id", values_from = "mean_voltage") %>%
  write_csv(file = filename)

# Do some QAQC checks?
summary <- oyster_dat_1min %>%
  ungroup() %>%
  select(oyster_id, vars, tidal_zone,treatment, sensor)

summary <- unique(summary)

duplicated(summary$oyster_id)
duplicated(summary$vars)

summary2 <- summary %>%
  group_by(treatment) %>%
  summarise(count = n())





