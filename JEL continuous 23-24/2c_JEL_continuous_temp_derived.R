# Created by Selina Cheng
# Last modified 3 Dec 2024
# Create interpolated 1 minute datasets for temperature and water quality data
# Also fill in temperature and water quality data when missing
rm(list = ls())
gc()
# ---------- SET UP ------------
# load libraries
library(data.table)
library(tidyverse)
library(lubridate)
library(zoo)

# Set directories
source_dir <- "~/Library/CloudStorage/OneDrive-USNH/Oyster Biosensor/0_InProgressExperiments/2023 - 2024 Continuous system on oysters and mussels/3_other data"
output_dir <- "~/Library/CloudStorage/OneDrive-USNH/Oyster Biosensor/0_InProgressExperiments/2023 - 2024 Continuous system on oysters and mussels/2b_derived sensor data"
oyster_dir <- "~/Library/CloudStorage/OneDrive-USNH/Oyster Biosensor/0_InProgressExperiments/2023 - 2024 Continuous system on oysters and mussels/2b_derived sensor data"

# create mean_rm function
mean_rm <- function(x){ mean(x, na.rm = T)}

# ------- Compare internal temperature data and JEL winter sonde temperature data -----
# Read HOBO temp data
temp_dat <- fread(list.files(source_dir, pattern = "temperature", full.names=T)) %>%
  select(-light_lux)

# Read sonde WQ data
wq_dat <- fread(list.files(source_dir, pattern = "GRBAP winter sonde data.csv", full.names=T))

# Filter out WQ data that has codes
wq_dat <- wq_dat %>%
  filter(F_Sal != "<-3> [SQR]",
         F_Depth != "<-2> [GMC]")

# Select just do, salinity, ph, temp
wq_dat <- wq_dat %>%
  select(DateTimeStamp, DO_mgl, Sal, pH, Temp) %>%
  mutate(DateTimeStamp = mdy_hm(DateTimeStamp))

# Reassign names
names(wq_dat) <- c("timestamp_est", "do_mgl", "sal_ppt", "ph", "temp_c")

# Read in Great Bay sonde data
gb_dat <- fread(list.files(source_dir, pattern = "GRBGB", full.names=T), skip = 2, nrows = 19689)

# Remove rows where there are codes
gb_dat <- gb_dat %>%
  filter(F_Temp != "<-2> (CMC)",
         F_SpCond != "<-3> (CDB)",
         F_SpCond != "<-3> [SQR]",
         F_DO_mgl != "<1> [SDO] (CLT)")

# Select the columns that we want
gb_dat <- gb_dat %>%
  select(DateTimeStamp, Temp, Sal, DO_mgl, pH) %>%
  mutate(DateTimeStamp = mdy_hm(DateTimeStamp))

# Change colnames
names(gb_dat) <- c("timestamp_est", "temp_c", "sal_ppt", "do_mgl", "ph")

# For the range of Great Bay sonde data, how do the means compare?
# Get dataset of Great bay temp data that is same times as HOBO data
reduced_temp <- gb_dat %>%
  filter(timestamp_est >= range(temp_dat$timestamp_est)[1] & timestamp_est <= range(temp_dat$timestamp_est)[2])

reduced_temp2 <- temp_dat %>%
  filter(timestamp_est >= range(gb_dat$timestamp_est)[1] & timestamp_est <= range(gb_dat$timestamp_est)[2])
# Difference between mean of HOBO data and mean of GB sonde data
mean_rm(reduced_temp$temp_c) - mean_rm(reduced_temp2$temp_c)
# = -0.03518006

# Round timestamp to nearest 15 minute (floor)
# Group by same timestamps and average temperature
temp_dat_15min <- temp_dat %>%
  mutate(fifteenmin_est = floor_date(timestamp_est, "15 minutes")) %>%
  group_by(fifteenmin_est) %>%
  summarise(mean_temp_c = mean_rm(temp_c))

# Join internal HOBO data with sonde data and calculate differences between temp
temp_diff <- left_join(gb_dat, temp_dat_15min, by = c("timestamp_est" = "fifteenmin_est"))
temp_diff <- temp_diff %>%
  mutate(diff = mean_temp_c - temp_c)

# Create linear model 
lm(mean_temp_c~temp_c, data = temp_diff)
# Intercept is 1.3413, slope is 0.9207
ggplot(data = temp_diff, aes(x = temp_c, y = mean_temp_c)) +
  geom_point() + 
  geom_smooth(method = "lm")

# ---------- Use Great Bay sonde to fill in missing data for WQ and temp ------
# Now linearly interpolate between everything..
# Create timeladder (full time sequence that we want)
timeladder <- seq(from = min(gb_dat$timestamp_est), 
                  to = max(gb_dat$timestamp_est), 
                  by = "min")

timeladder_df <- data.frame(timestamp_est = timeladder)
# join "timeladder" with original data
gb_dat_1min <- full_join(timeladder_df, gb_dat, by = "timestamp_est")

# Linear interpolate between datapoints
gb_dat_1min <- gb_dat_1min %>%
  mutate(temp_c = zoo::na.approx(temp_c),
         do_mgl = zoo::na.approx(do_mgl),
         sal_ppt = zoo::na.approx(sal_ppt),
         ph = zoo::na.approx(ph))

# Read in oyster data
oyster_dat_1min <- fread(list.files(oyster_dir, pattern = "oyster6_1min", full.names=T))

# ----------- Temperature data ----------
# Interpolate temperature data to create baseline 1 min temp data
# Create sequence of 1 minute timestamps that you want to interpolate
# But do not create sequence that fills in MISSING data
# Looks like temp data is pretty continuous from start to 2024-05-12 14:10:00
# After that the data is at 1 min interval anyway. SO:
time_ladder <- seq(from = min(temp_dat$timestamp_est), 
                   to = as.POSIXct("2024-05-12 14:10:00", tz = "UTC"), 
                   by = "min")

timeladder_df <- data.frame(timestamp_est = time_ladder)
# Create full sequence
temp_dat_1min <- full_join(timeladder_df, temp_dat, by = "timestamp_est")

# Now NA.approx to linear interpolate between each 10 min measurement
temp_dat_1min$temp_c <- zoo::na.approx(temp_dat_1min$temp_c)

# Now create full sequence based on oyster data
timeladder <- seq(from = min(oyster_dat_1min$minute_floor_est), 
                  to =  max(oyster_dat_1min$minute_floor_est), 
                  by = "min")

timeladder_df <- data.frame(timestamp_est = timeladder)
# Create full sequence
temp_dat_1min <- full_join(timeladder_df, temp_dat_1min, by = "timestamp_est")

# Join with great bay data
temp_dat_1min <- left_join(temp_dat_1min, gb_dat_1min, by = "timestamp_est", suffix = c("", "_gb"))

# If temp dat is missing, fill with great bay data*0.9207 + intercept of lm =1.3413
temp_dat_1min <- temp_dat_1min %>%
  mutate(temp_c = ifelse(is.na(temp_c), (temp_c_gb*0.9207 + 1.3413), temp_c))

# Save 1 min temp data
temp_dat_1min %>%
  select(timestamp_est, temp_c) %>%
  mutate(timestamp_est = format(timestamp_est, format = "%Y-%m-%d %H:%M:%S")) %>%
  write.table(file = file.path(output_dir, "JEL_continuous_tempfilled_1min.csv"),
              append = F, sep = ",", na = "NA", dec = ".", row.names = F, col.names = T)

# -------- JEL water quality data ---------
# Linear interpolation
# Create timeladder
timeladder <- seq(from = min(wq_dat$timestamp_est), 
                  to = max(wq_dat$timestamp_est), 
                  by = "min")

timeladder_df <- data.frame(timestamp_est = timeladder)

# Create full sequence
wq_dat_1min <- full_join(timeladder_df, wq_dat, by = "timestamp_est")

# Linear interpolation
wq_dat_1min <- wq_dat_1min %>%
  select(-temp_c) %>%
  mutate(do_mgl = zoo::na.approx(do_mgl),
         sal_ppt = zoo::na.approx(sal_ppt),
         ph = zoo::na.approx(ph))

# Now create full sequence based on oyster data
timeladder <- seq(from = min(oyster_dat_1min$minute_floor_est), 
                  to =  max(oyster_dat_1min$minute_floor_est), 
                  by = "min")

timeladder_df <- data.frame(timestamp_est = timeladder)
# Create full sequence
wq_dat_1min <- full_join(timeladder_df, wq_dat_1min, by = "timestamp_est")

# Join with great bay data
wq_dat_1min <- left_join(wq_dat_1min, gb_dat_1min, by = "timestamp_est", suffix = c("", "_gb"))

# If WQ parameters are missing, fill with great bay data
wq_dat_1min <- wq_dat_1min %>%
  mutate(do_mgl = ifelse(is.na(do_mgl), do_mgl_gb, do_mgl),
         sal_ppt = ifelse(is.na(sal_ppt), sal_ppt_gb, sal_ppt),
         ph = ifelse(is.na(ph), ph_gb, ph))

# Save
wq_dat_1min %>%
  select(timestamp_est, do_mgl, sal_ppt, ph) %>%
  mutate(timestamp_est = format(timestamp_est, format = "%Y-%m-%d %H:%M:%S")) %>%
  write.table(file = file.path(output_dir, "JEL_continuous_wqfilled_1min.csv"),
              append = F, sep = ",", na = "NA", dec = ".", row.names = F, col.names = T)
