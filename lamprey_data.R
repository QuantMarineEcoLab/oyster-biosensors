# Read in and view Lamprey data

# Load libraries
library(tidyverse)
library(data.table)
library(lubridate)

mean_rm <- function(x){mean(x, na.rm = T)}


# Set source dir
raw_dir <- "~/Library/CloudStorage/OneDrive-USNH/QMEL/0_People/GradStudents/SelinaCheng/lamprey_deployment/data"

# other_dir <- "~/Documents/UNH MS Degree/Data/Lamprey Deployment 09-24"

# Get data files
i <- list.files(raw_dir, pattern = "txt", full.names = T)
  
# Read in data
dat <- fread(i)

# dat <- fread(file.path(other_dir, "data.txt"))

# Change first colname
names(dat)[1] <- "unixtime"

# Create timestamp_est column
# (time was set to)
dat$timestamp_est <- as.POSIXct(dat$unixtime, origin = "1970-01-01")

# Pivot longer
dat_pivot <- pivot_longer(dat, cols = names(dat)[grepl("-", names(dat))], 
                          names_to = "vars", values_to = "value")

# Remove extra cols
dat_pivot <- dat_pivot %>%
  filter(vars != "B-Volts") %>%
  select(-c(BoxTemp, V28))

# Set tz for dat_pivot
tz(dat_pivot$timestamp_est) <- "UTC"

# Select times after deployment start time and before I lifted the cages
start <- ymd_hms("2024-10-03 10:38:00")
end <- ymd_hms("2024-10-06 11:30:00")

dat_pivot <- dat_pivot %>%
  filter(timestamp_est > !!start & timestamp_est < !!end)

# Save
write.csv(dat_pivot, file = file.path(raw_dir, "2024-10-06_lamprey_data_long.csv"), row.names = F)

# ----- Round timestamps --------
# Add interval column
dat_pivot <- dat_pivot %>%
  mutate(interval = "15_sec")

# Create 1 min rounded dataset
dat_pivot_1min <- dat_pivot %>%
  mutate(timestamp_est = floor_date(timestamp_est, "minute")) %>%
  group_by(timestamp_est, vars) %>%
  summarise(value = mean_rm(value)) %>%
  mutate(interval = "1_min")

# Create a 15-min rounded dataset
dat_pivot_15min <- dat_pivot %>%
  mutate(timestamp_est = floor_date(timestamp_est, "15 minute")) %>%
  group_by(timestamp_est, vars) %>%
  summarise(value = mean_rm(value)) %>%
  mutate(interval = "15_min")

# Rbind all together
test_dat <- rbindlist(l = list(dat_pivot, dat_pivot_1min, dat_pivot_15min), fill = T)

which(!grepl("....-..-.. ..:..:..", test_dat$timestamp_est))

# Save, adding in any midnight timestamps
test_dat$timestamp_est <- format(test_dat$timestamp_est, format = "%Y-%m-%d %H:%M:%S")

write.csv(test_dat, file = file.path(raw_dir, "2024_10_06_lamprey_interval_comparison.csv"), row.names = F)
