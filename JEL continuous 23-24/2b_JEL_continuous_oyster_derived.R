# Created by Selina Cheng
# Last modified 30 Nov 2024
# Create derived variables:
# How many times does an oyster open or close each day?
# During each event, how long is the oyster open or closed for?
rm(list = ls())
gc()

# ---------- SET UP ------------
# load libraries
library(data.table)
library(tidyverse)
library(lubridate)
library(zoo)

# Source dir
source_dir <- "~/Library/CloudStorage/OneDrive-USNH/Oyster Biosensor/0_InProgressExperiments/2023 - 2024 Continuous system on oysters and mussels/2a_processed sensor data"
output_dir <- "~/Library/CloudStorage/OneDrive-USNH/Oyster Biosensor/0_InProgressExperiments/2023 - 2024 Continuous system on oysters and mussels/2b_derived sensor data"

# --------- 1 minute dataset ----------
# Read data
oyster_dat_1min <- fread(list.files(source_dir, pattern = "fix", full.names=T))

# Remove data 
oyster_dat_1min <- oyster_dat_1min %>%
  filter(minute_floor_est <  as.POSIXct("2024-04-05 09:21:00", tz = "UTC") |
           minute_floor_est > as.POSIXct("2024-04-05 09:25:00", tz = "UTC"))

# Redo some statuses
oyster_dat_1min <- oyster_dat_1min %>%
  mutate(new_status = ifelse(minute_floor_est > as.POSIXct("2024-05-19 02:20:00", tz = "UTC") & 
                               minute_floor_est < as.POSIXct("2024-05-19 03:18:00", tz = "UTC"),
                             "closed", new_status))

#  More manipulation............
oyster_dat_1min$status_numeric <- ifelse(oyster_dat_1min$new_status == "open", 1, 0)
oyster_dat_1min$change <- c(diff(oyster_dat_1min$status_numeric),0)

# Get lengths and values of runs of equal values in a vector
runs <- rle(oyster_dat_1min$new_status)
runs <- data.frame(new_status = runs$values, length = runs$lengths)
runs$change_rows <- c(which(oyster_dat_1min$change != 0), NA)
runs_ignore <- runs %>% filter(length == 1)
runs_ignore$ignore <- "IGNORE"

# Create vector of rows expanded by run length that should be ignored
rows_lengths <- c()
for(n in 1:nrow(runs_ignore)){
  vector <- seq(from = (runs_ignore$change_rows[n]-(runs_ignore$length[n])+1), length = runs_ignore$length[n])
  rows_lengths <- c(rows_lengths, vector)
}
check_rows <- data.frame(index = rows_lengths, ignore = "IGNORE")

# Revise index and join
oyster_dat_1min$index <- as.numeric(row.names(oyster_dat_1min))
oyster_dat_1min <- left_join(oyster_dat_1min, check_rows)

# Filter out IGNORE rows
oyster_dat_1min <- oyster_dat_1min %>% filter(is.na(ignore))

# CHECK ON THIS WHEN YOU HAVE WIFI? -------------
# Just in case things have changed, redo the change diffs
oyster_dat_1min$change <- c(diff(oyster_dat_1min$status_numeric),0)
# Also redo index
oyster_dat_1min$index <- as.numeric(row.names(oyster_dat_1min))

# This is one version of data that will be used for analysis
oyster_dat_1min %>%
  mutate(minute_floor_est = format(minute_floor_est, format = "%Y-%m-%d %H:%M:%S")) %>%
  write.table(file = file.path(output_dir, "JEL-continuous-oyster6_1min.csv"),
              append = F, sep = ",", na = "NA", dec = ".", row.names = F, col.names = T)

# Diffs represent status of (n+1) being different from (n)
# A diff of 1 means an oyster changed from closed to open
# A diff of -1 means an oyster changed from open to closed
# When change != 0, it coincides with the end row of a status (not the beginning of a new status)

# -------------- Hourly dataset -----------------------
# Create some derived variables
# Count how many times oyster opened and closed per hour
oyster_dat_hourly <- oyster_dat_1min %>%
  mutate(hour_floor_est = floor_date(minute_floor_est, "hour"))

# Aggregate into hourly
oyster_dat_hourly <- oyster_dat_hourly %>%
  group_by(hour_floor_est, unique_id, species) %>%
  summarize(num_open_event = sum(change == 1),
            num_close_event = sum(change == -1),
            num_switch_event = sum(change != 0))

# Save hourly data
oyster_dat_hourly %>%
  mutate(hour_floor_est = format(hour_floor_est, format = "%Y-%m-%d %H:%M:%S")) %>%
  write.table(file = file.path(output_dir, "JEL-continuous-oyster6_hourly.csv"),
              append = F, sep = ",", na = "NA", dec = ".", row.names = F, col.names = T)

# --------------- Daily dataset --------------------
# Create one for daily?
oyster_dat_daily <- oyster_dat_1min %>%
  mutate(date = date(minute_floor_est))

oyster_dat_daily <- oyster_dat_daily %>%
  group_by(date, unique_id, species) %>%
  summarize(num_open_event = sum(change == 1),
            num_close_event = sum(change == -1),
            num_switch_event = sum(change != 0))

# Save daily data
oyster_dat_daily %>%
  # mutate(hour_floor_est = format(hour_floor_est, format = "%Y-%m-%d %H:%M:%S")) %>%
  write.table(file = file.path(output_dir, "JEL-continuous-oyster6_daily.csv"),
              append = F, sep = ",", na = "NA", dec = ".", row.names = F, col.names = T)

# -------------- Event dataset ------------------
# Assort oysters into events
# Use RLE to quickly calculate runs
runs <- rle(oyster_dat_1min$new_status)
runs <- data.frame(new_status = runs$values, length = runs$lengths)
runs$change_rows <- c(which(oyster_dat_1min$change != 0), NA)

# Create df for "open" runs
runs_open <- runs %>%
  filter(new_status == "open") %>%
  mutate(event_id = paste0(new_status, "_", row_number()))
runs_open$change_rows[nrow(runs_open)] <- nrow(oyster_dat_1min)

# Expand runs by length
rows <- c()
for(n in 1:nrow(runs_open)){
  temp <- seq(from = (runs_open$change_rows[n] - runs_open$length[n] + 1), 
              length.out = runs_open$length[n])
  rows <- c(rows, temp)
}

# Create df where rows are open, assign event_id
runs_open_df <- data.frame(event_id = rep(runs_open$event_id, runs_open$length),
                           change_rows = rows)

# Create df for "closed" runs
runs_close <- runs %>% 
  filter(new_status == "closed") %>%
  mutate(event_id = paste0(new_status, "_", row_number()))

rows <- c()
for(n in 1:nrow(runs_close)){
  temp <- seq(from = (runs_close$change_rows[n] - runs_close$length[n] + 1), 
              length.out = runs_close$length[n])
  rows <- c(rows, temp)
}

runs_closed_df <- data.frame(event_id = rep(runs_close$event_id, runs_close$length),
                           change_rows = rows)

# Combine "closed" and "open" event id dfs
runs_df <- rbind(runs_closed_df, runs_open_df)

# Bind runs_df with oyster_dat_1min
oyster_dat_events <- left_join(oyster_dat_1min, runs_df, by = c("index" = "change_rows"))

# Do events match up exactly with status? 
start <- Sys.time()
test <- c()
for(n in 1:nrow(oyster_dat_events)){
  temp <- grepl(oyster_dat_events$new_status[n], oyster_dat_events$event_id[n])
  test <- c(test, temp)
}
end <- Sys.time()
end-start

# yes. sick
sum(test) == nrow(oyster_dat_events) # TRUE

# Create events dataset
oyster_dat_events <- oyster_dat_events %>%
  group_by(event_id) %>%
  summarize(length_minute = n(),
            start_time = min(minute_floor_est),
            end_time = max(minute_floor_est))

# order by start time
oyster_dat_events <- oyster_dat_events %>%
  arrange(start_time)

# Save files
oyster_dat_events %>%
  mutate(start_time = format(start_time, format = "%Y-%m-%d %H:%M:%S")) %>%
  mutate(end_time = format(end_time, format = "%Y-%m-%d %H:%M:%S")) %>%
  write.table(file = file.path(output_dir, "JEL-continuous-oyster6_event.csv"),
              append = F, sep = ",", na = "NA", dec = ".", row.names = F, col.names = T)



