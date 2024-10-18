# Read in and view Lamprey data
# Selina Cheng

# Add in some scenario somewhere where you save the last timestamp in the filename, strsplit the filename and check to see if the timestamp is in your time, filter it, etc. etc save

# There is almost certainly a timestamp shift issue with reading in old data and appending new data to it NEEDS TO BE FIXED NEXT TIME I GET NEW DATA

# --------- SET UP ------------
# Load libraries
library(tidyverse)
library(data.table)
library(lubridate)

# Write function
mean_rm <- function(x){mean(x, na.rm = T)}

# Set source directory
raw_dir <- "~/Library/CloudStorage/OneDrive-USNH/QMEL/0_People/GradStudents/SelinaCheng/lamprey_deployment/data/raw"

# Set output directory
combined_dir <- "~/Library/CloudStorage/OneDrive-USNH/QMEL/0_People/GradStudents/SelinaCheng/lamprey_deployment/data/combined"

# ----------- READ DATA ----------------
# Get data file names
i <- list.files(raw_dir, pattern = "txt", full.names = T)
  
dat <- rbindlist(lapply(i, fread))

# ---------- MANIPULATE COLUMNS ----------
# Change first colname
names(dat)[1] <- "unixtime"

# Create timestamp_est column
# (time was/should be set to EST on Arduino)
dat$timestamp_est <- as.POSIXct(dat$unixtime, origin = "1970-01-01")

# Remove extra columns
dat <- dat %>%
  select(-c("V28", "BoxTemp"))

# --------- REMOVE OUT-OF-WATER TIMES -------------
# Remove times when oysters were out of water (removed cages from water or something)
oow_start <- ymd_hms("2024-10-03 06:30:00") # I believe I ended the system at this time
oow_end <- ymd_hms("2024-10-03 10:38:00") # Oysters deployed after this time
oow_start2 <- ymd_hms("2024-10-06 11:30:00") # Oysters removed from water after this time
oow_end2 <- ymd_hms("2024-10-06 12:00:00")

# Set tz of timestamps
tz(dat$timestamp_est) <- "UTC"

# Filter timestamps out
dat <- dat %>%
  filter((timestamp_est < oow_start | timestamp_est > oow_end) & 
           (timestamp_est < oow_start2 | timestamp_est > oow_end2))

# --------- QAQC Num.1 ---------------------------
dat <- unique(dat)

# Create differences between times
dat$diffs <- diff(c(dat$unixtime[1]-1,dat$unixtime))

# Are any times going backwards?
which(dat$diffs < 0)

# View times where data is going backwards 
test <- dat[57354:57360,]

# Change timestamp: if diffs are less than 0, look back at the previous timestamp and add 15 seconds
dat <- dat %>%
  mutate(unixtime = ifelse(diffs < 0, lag(unixtime) + 15, unixtime))

# Recreate diffs and see if any times are going backwards
dat$diffs <- diff(c(dat$unixtime[1]-1,dat$unixtime))

# Are any times going backwards?
which(dat$diffs < 0)

# Recreate timestamp_est
dat$timestamp_est <- as.POSIXct(dat$unixtime, origin = "1970-01-01")

# Recreate diffs with timestamp_est and see if any times are going backwards
dat$diffs <- diff(c(dat$timestamp_est[1]-1,dat$timestamp_est))

# Are any times going backwards?
which(dat$diffs < 0)

# Remove diffs column
dat <- dat %>%
  select(-diffs)

# ----------- PIVOT -----------------
# Pivot longer
dat_pivot <- pivot_longer(dat, cols = names(dat)[grepl("-", names(dat))], 
                          names_to = "vars", values_to = "value")

# --------- ROUND TIMESTAMPS ----------
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
dat_combined <- rbindlist(l = list(dat_pivot, dat_pivot_1min, dat_pivot_15min), fill = T)

tz(dat_combined$timestamp_est) <- "UTC"

# ----------- QAQC ------------
# Are there any duplicated combinations that shouldn't be duplicated?
summary_table <- dat_combined %>%
  group_by(timestamp_est, vars, interval) %>%
  summarise(count = n())

# Print which rows > 1 
which(summary_table$count > 1)
  
# For each combination, if data is +/- 0.05, you can just combine via mean? 
# For this scenario, this is a fine action, but consider for future actions? idk

# ---------- SAVE ---------------
# Is there already a file named "something something 'combined' something" in the output directory? 
file_exists <- list.files(combined_dir, pattern = "combined", full.names = T)

# If there is already a file, then...
if(length(file_exists) == 1){
  # Read in old combined file
  old_dat_combined <- fread(file_exists)
  
  # Rbind new dat_combined to old_dat combined
  new_dat_combined <- rbind(old_dat_combined, dat_combined)

  # Get only unique rows 
  new_dat_combined <- unique(new_dat_combined)

  # (but I could see how with timestamp rounding, you could get new mean data
  # but have repeated timestamps? So let's see unique combinations of each time/oyster)
  summary_table <- dat_combined %>%
    group_by(timestamp_est, vars, interval) %>%
    summarise(count = n())
  
  # Print which rows > 1 
  if(sum(summary_table$count > 1) > 0){
    stop("There are unexpected duplicated times in the dataset")
    
  # If there are no duplicated rows
  } else if(sum(summary_table$count > 1) == 0){
    # Make sure timestamp is character format
    new_dat_combined$timestamp_est <- format(new_dat_combined$timestamp_est, 
                                             format = "%Y-%m-%d %H:%M:%S")
    
    # Are there any timestamps with missing times?
    sum(!grepl("....-..-.. ..:..:..", new_dat_combined$timestamp_est))
    
    # Get timestamp
    latest_time <- gsub(":", "", max(new_dat_combined$timestamp_est))
    
    # Write data
    write.csv(new_dat_combined, 
              file = file.path(combined_dir, 
                               paste0(latest_time, "_lamprey_data_combined.csv")),
              row.names = F)
    
    file.remove(file_exists)
  }
} else if(length(file_exists) > 1){
  warning("There is more than one combined file. Resolve any duplicates.")
} else {
  # If no data already exists....
  # Save, adding in any midnight timestamps
  dat_combined$timestamp_est <- format(dat_combined$timestamp_est, 
                                       format = "%Y-%m-%d %H:%M:%S")
  
  # Are there any timestamps where midnight time is missing?
  sum(!grepl("....-..-.. ..:..:..", dat_combined$timestamp_est))
  
  # Get timestamp
  latest_time <- gsub(":", "", max(dat_combined$timestamp_est))
  
  # Write
  write.csv(dat_combined, 
            file = file.path(combined_dir, 
                             paste0(latest_time, "_lamprey_data_combined.csv")),
            row.names = F)
}
