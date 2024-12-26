# Created by Selina Cheng
# Last modified 30 Nov 2024
# Join temp data with oyster data
# Create temp derived variables
# Do analysis
rm(list = ls())
gc()

# ---------- SET UP ------------
# load libraries
library(data.table)
library(tidyverse)
library(lubridate)

mean_rm <- function(x){mean(x, na.rm = T)}
max_rm <- function(x){max(x, na.rm = T)}

# Source dir
source_dir <- "~/Library/CloudStorage/OneDrive-USNH/Oyster Biosensor/0_InProgressExperiments/2023 - 2024 Continuous system on oysters and mussels/2b_derived sensor data"

output_dir <- "~/Library/CloudStorage/OneDrive-USNH/Oyster Biosensor/0_InProgressExperiments/2023 - 2024 Continuous system on oysters and mussels/2c_data for analysis"

# Load data
oyster_dat_1min <- fread(list.files(source_dir, pattern = "oyster6_1min", full.names=T))

oyster_dat_daily <- fread(list.files(source_dir, pattern = "daily", full.names=T))

oyster_dat_event <- fread(list.files(source_dir, pattern = "event", full.names=T))

temp_dat_1min <- fread(list.files(source_dir, pattern = "temp", full.names=T))

wq_dat_1min <- fread(list.files(source_dir, pattern = "wq", full.names=T))

# ------------ Autocorrelation ----------------
# Ignoring autocorrelation for this project...
# ACF plot suggests high autocorrelation?
# But PACF plot suggests the high autocorrelation is mostly due to successive measurements? (<10 min)
# Is that right?
# acf(oyster_dat_1min$mean_value_clean, type = "correlation")
# pacf(oyster_dat_1min$mean_value_clean)

# -------- Create additional temperature vars ---------------
# Create time ladder again and full join so that the lag performs correctly
# Create lag for 1-7 days of temperature
# 1 day = 60 min/hr * 24 hr
temp_dat_1min <- temp_dat_1min %>%
  mutate(lag_1day = lag(temp_c, n=(60*24*1)),
         lag_2day = lag(temp_c, n = (60*24*2)),
         lag_3day = lag(temp_c, n = (60*24*3)),
         lag_4day = lag(temp_c, n = (60*24*4)),
         lag_5day = lag(temp_c, n = (60*24*5)),
         lag_6day = lag(temp_c, n = (60*24*6)),
         lag_7day = lag(temp_c, n = (60*24*7)))

# Number of continuous rows? above previously identified awakening threshold = 3.27 C (Comeau 2014)
temp_dat_1min <- temp_dat_1min %>%
  mutate(above_threshold = temp_c >= 3.27,
         change = c(diff(above_threshold),0),
         above_threshold = ifelse(is.na(above_threshold), "no_temp", above_threshold))

# Diffs represent status of (n+1) being different from (n)
# A diff of 1 means temp changed from above to below threshold
# A diff of -1 means temp changed from below to above threshold
# TRUE means above threshold
# FALSE means below threshold
# When change != 0, it coincides with the end row of a status (not the beginning of a new status)

# Create runs again
runs <- rle(temp_dat_1min$above_threshold)
runs <- data.frame(above_threshold = runs$values, length = runs$lengths)
# Add in rows where the change occurs
runs$change_rows <- c(which(temp_dat_1min$change != 0), nrow(temp_dat_1min))

# Calculate amount of time above threshold since last 
# Expand runs by length to get series of rows for joining
row_id <- c()
for(n in 1:nrow(runs)){
  temp <- seq(from = (runs$change_rows[n] - runs$length[n] + 1), 
              length.out = runs$length[n])
  row_id <- c(row_id, temp)
}
# Expand runs by # of rows above threshold...which also represents number of minutes above threshold
threshold <- c()
for(n in 1:nrow(runs)){
  temp <- seq(from = 1, to = runs$length[n])
  threshold <- c(threshold, temp)
}
# Status
status <- c()
for(n in 1:nrow(runs)){
  temp <- rep(runs$above_threshold[n], runs$length[n])
  status <- c(status, temp)
}

# Create df where rows are open, assign event_id
temp_threshold_df <- data.frame(above_threshold = status,
                                rows_above = threshold,
                                change_rows = row_id)

# Create index of rows for temp_dat_1min
temp_dat_1min$index <- as.numeric(row.names(temp_dat_1min))

# Join temp_dat_1min with temp_threshold_df
temp_dat_1min <- left_join(temp_dat_1min, temp_threshold_df, by = c("index" = "change_rows",
                                                                    "above_threshold"))

# If above_threshold is FALSE, set rows_above to NA
temp_dat_1min <- temp_dat_1min %>%
  mutate(rows_above = ifelse(above_threshold == TRUE, rows_above, NA))

# ----- Binomial GLM for 1 minute data to model probability of open or closed ----
# Join temp, wq data with oyster data
oyster_dat_1min <- left_join(oyster_dat_1min, temp_dat_1min, by = c("minute_floor_est" = "timestamp_est"))
oyster_dat_1min <- left_join(oyster_dat_1min, wq_dat_1min, by = c("minute_floor_est" = "timestamp_est"))

# Select just the columns you need for analysis
oyster_dat_1min <- oyster_dat_1min %>%
  select(minute_floor_est, mean_value_clean, new_status, do_mgl, sal_ppt, ph,
         temp_c, contains("lag"), above_threshold, rows_above)

oyster_dat_1min %>%
  mutate(minute_floor_est = format(minute_floor_est, format = "%Y-%m-%d %H:%M:%S")) %>%
  write.table(file = file.path(output_dir, "oyster_dat_1min_analysis.csv"),
              append = F, sep = ",", na = "NA", dec = ".", row.names = F, col.names = T)

# ---- # of times open per day (or hour?) is count data, so use Poisson negative binomial glm ---
# Let's model # of times open per day because the per hour numbers are really low
# Calculate average temperature (and lags) during each day
# But maximum rows_above for amount of time since for each day
# Or....maybe sum up number of rows_above? for time above threshold each day???
temp_dat_daily <- temp_dat_1min %>%
  mutate(date = date(timestamp_est),
         above_threshold = as.logical(above_threshold)) %>%
  group_by(date) %>%
  summarize(mean_temp_c = mean_rm(temp_c),
            mean_lag1 = mean_rm(lag_1day),
            mean_lag2 = mean_rm(lag_2day),
            mean_lag3 = mean_rm(lag_3day),
            mean_lag4 = mean_rm(lag_4day),
            mean_lag5 = mean_rm(lag_5day),
            mean_lag6 = mean_rm(lag_6day),
            mean_lag7 = mean_rm(lag_7day),
            time_above_threshold = sum(above_threshold))

wq_dat_daily <- wq_dat_1min %>%
  mutate(date = date(timestamp_est)) %>%
  group_by(date) %>%
  summarize(mean_do = mean_rm(do_mgl),
            mean_sal = mean_rm(sal_ppt),
            mean_ph = mean_rm(ph))

# Join temp_dat_daily with oyster data
oyster_dat_daily$date <- as.Date(oyster_dat_daily$date)
oyster_dat_daily <- left_join(oyster_dat_daily, temp_dat_daily, by = "date")
oyster_dat_daily <- left_join(oyster_dat_daily, wq_dat_daily, by = "date")

oyster_dat_daily %>%
  # mutate(minute_floor_est = format(minute_floor_est, format = "%Y-%m-%d %H:%M:%S")) %>%
  write.table(file = file.path(output_dir, "oyster_dat_daily_analysis.csv"),
              append = F, sep = ",", na = "NA", dec = ".", row.names = F, col.names = T)

# ---- # Amount of time open per time open is also Poisson negative binomial glm ---------
# Calculate average temperature during each event
# Sum above_threshold for amount of time above threshold
temp_dat_event <- left_join(temp_dat_1min, oyster_dat_event,
                           join_by(timestamp_est >= start_time, 
                                   timestamp_est <= end_time))

temp_dat_event <- temp_dat_event %>% 
  filter(!is.na(event_id))

temp_dat_event <- temp_dat_event %>%
  mutate(above_threshold = as.logical(above_threshold)) %>%
  group_by(event_id) %>%
  summarise(mean_temp_c = mean_rm(temp_c),
            mean_lag1 = mean_rm(lag_1day),
            mean_lag2 = mean_rm(lag_2day),
            mean_lag3 = mean_rm(lag_3day),
            mean_lag4 = mean_rm(lag_4day),
            mean_lag5 = mean_rm(lag_5day),
            mean_lag6 = mean_rm(lag_6day),
            mean_lag7 = mean_rm(lag_7day),
            time_above_threshold = sum(above_threshold))

# Create wq_dat_event
wq_dat_event <- left_join(wq_dat_1min, oyster_dat_event,
                            join_by(timestamp_est >= start_time, 
                                    timestamp_est <= end_time))

wq_dat_event <- wq_dat_event %>% 
  filter(!is.na(event_id))

wq_dat_event <- wq_dat_event %>%
  group_by(event_id) %>%
  summarise(mean_do = mean_rm(do_mgl),
            mean_sal = mean_rm(sal_ppt),
            mean_ph = mean_rm(ph))

# Join temp dat event with oyster_dat_event
oyster_dat_event <- left_join(oyster_dat_event, temp_dat_event, by= "event_id")
oyster_dat_event <- left_join(oyster_dat_event, wq_dat_event, by = "event_id")

# Some length_minute values are wrong because there were NAs dropped during the event, I think.
which(oyster_dat_event$length_minute < oyster_dat_event$time_above_threshold)
oyster_dat_event$new_length_minute <- as.numeric(difftime(oyster_dat_event$end_time,
                                                      oyster_dat_event$start_time, 
                                                      units = "mins")+1)

check <- oyster_dat_event[oyster_dat_event$new_length_minute != oyster_dat_event$length_minute,]

check <- check %>%
  select(event_id, length_minute, new_length_minute, start_time, end_time)

# Set length_minute values to be new_length_minute?
oyster_dat_event <- oyster_dat_event %>%
  mutate(length_minute = ifelse(grepl("open", event_id), new_length_minute, length_minute))
# oyster_dat_event$length_minute <- oyster_dat_event$new_length_minute

oyster_dat_event %>%
  mutate(start_time = format(start_time, format = "%Y-%m-%d %H:%M:%S")) %>%
  mutate(end_time = format(end_time, format = "%Y-%m-%d %H:%M:%S")) %>%
  write.table(file = file.path(output_dir, "oyster_dat_event_analysis.csv"),
              append = F, sep = ",", na = "NA", dec = ".", row.names = F, col.names = T)

