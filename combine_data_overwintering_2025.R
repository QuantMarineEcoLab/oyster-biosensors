# Read in and combine JEL data
# Selina Cheng
# Created 2025-03-19

# Add in some scenario somewhere where you save the last timestamp in the filename, strsplit the filename and check to see if the timestamp is in your time, filter it, etc. etc save
# --------- SET UP ------------
rm(list = ls())

# Load libraries
library(tidyverse)
library(data.table)
library(lubridate)

# Set source directory
raw_dir <- "~/Library/CloudStorage/OneDrive-USNH/Oyster Biosensor/Overwintering"

# Set "JEL" or "SPL" as the dataset you want to combine
# group <- "JEL"
group <- "SPL"

# ----------- SET UP DIRECTORIES ----------------
# Get base directory
dirs <- list.dirs(raw_dir, full.names = T, recursive = F)

# Source directory for raw sensor data
source_dir <- file.path(raw_dir, "data", "raw sensor data")
# Source directory for oyster schema
schema_dir <- file.path(raw_dir, "data", "schema")
# Output directory for combined data
output_dir <- file.path(raw_dir, "data")

# ----------- READ IN DATA -------------------
# First...search for combined_group file in output_dir
i <- setdiff(list.files(output_dir, full.names = T, pattern = group, recursive = F, include.dirs = F),
             list.dirs(output_dir, recursive = F, full.names = T))

# If there is no combined file....
if(length(i) == 0){
  # Read in all raw data files
  j <- list.files(source_dir, pattern = group, full.names = T, recursive = T, ignore.case = T)
  dat <- rbindlist(lapply(j, fread))
  
} else if(length(i) > 0){ # otherwise if there is a combined data file...
  # Get filename
  filename <- basename(i)
  name_elements <- unlist(strsplit(filename, "_"))
  date <- name_elements[length(name_elements)]
  date <- gsub("\\.csv", "", date)
  date <- substr(date, 1, 10)

  # Get raw file dates
  j <- list.files(source_dir, pattern = NULL, full.names = T, recursive = F)
  base_j <- basename(j)
  name_elements <- strsplit(base_j, "_")
  
  # Choose the 3rd element from the name
  dates_j <- sapply(name_elements, "[[", 3)
  
  # Which dates in raw files are > than date of combined file?
  sub_j <- j[dates_j >= date]
  
  # Read in just those raw files with dates > date of combined file
  dat <- rbindlist(lapply(sub_j, fread))
}

# ---------- MANIPULATE COLUMNS ----------
# Change first colname
names(dat)[1] <- "unixtime"

# Remove extra columns
dat <- dat %>%
  select(-any_of(c("BoxTemp", "B-Volts", contains("V2"))))

# --------- QAQC --------------
# Remove any duplicate rows
dat <- unique(dat)

# Are there any duplicated timestamps?
which(duplicated(dat$unixtime))

# Look for any backwards-moving timestamps
# Create differences between times
dat$diffs <- diff(c(dat$unixtime[1]-1,dat$unixtime))

# Average together where the unixtime is the same
setDT(dat)
avg_dat <- dat[,lapply(.SD, mean), by = unixtime]
  
# did we fix it?
which(duplicated(avg_dat$unixtime))

# Are any times going backwards?
which(avg_dat$diffs < 0)

# View times where data is going backwards 
test <- avg_dat[23650:23660,]

# Also view times if timeskip is forward by a ton?
which(avg_dat$diffs > 1000)

# Looks like for the most part, it's just a random and erroneous timeskip that just needs to be amended
# Change timestamp: if diffs less than 0, look back at the previous timestamp and add 15 seconds
avg_dat <- avg_dat %>%
  mutate(unixtime = ifelse(diffs < 0, lag(unixtime) + 15, unixtime))

# Recreate diffs and see if any times are going backwards. Did we fix it?
avg_dat$diffs <- diff(c(avg_dat$unixtime[1]-1,avg_dat$unixtime))
which(avg_dat$diffs < 0) # Yes

# Create timestamp_est
avg_dat$timestamp_est <- as.POSIXct(avg_dat$unixtime, origin = "1970-01-01", tz = "EST")

# Are there any duplicated timestamps?
which(duplicated(avg_dat$timestamp_est))

# Remove diffs column
avg_dat <- avg_dat %>%
  select(-diffs)

# ----------- PIVOT -----------------
# Pivot longer
dat_pivot <- pivot_longer(avg_dat, 
                          cols = names(avg_dat)[grepl("-", names(avg_dat))], 
                          names_to = "vars", values_to = "value")

# Remove extra column
dat_pivot <- dat_pivot %>%
  select(-V28)

# Remove "-" from variable names
dat_pivot$vars <- gsub("-", "", dat_pivot$vars)

# --------- ADD SCHEMA ---------------
# Read in schema dat
schema_dat <- fread(list.files(schema_dir, pattern = "schema", full.name = T))

schema_dat <- schema_dat %>%
  filter(overwintering == !!group, !is.na(sensor_id))

tz(schema_dat$end_time_est) <- "EST"
tz(schema_dat$start_time_est) <- "EST"

# Add in an "end date" of schema if there isn't one
schema_dat <- schema_dat %>%
  mutate(end_time_est = ifelse(is.na(end_time_est), max(avg_dat$timestamp_est)+1, 
                               end_time_est),
         end_time_est = as.POSIXct(end_time_est, origin = "1970-01-01", tz = "EST"))

# Combine schema with data
# Join based on date ranges
start <- Sys.time()
combined_dat <- left_join(dat_pivot, schema_dat, 
                          join_by(vars == sensor_id, timestamp_est >= start_time_est,
                                  timestamp_est < end_time_est))
end <- Sys.time()
end-start

# Remove where oyster_id is NA
combined_dat <- combined_dat %>%
  filter(!is.na(id))

# ---------- SAVE ---------------
# Is there already a combined data file? If there isn't...
if(length(i) == 0){
  # Just save the data, easy peasy
  # Add in any midnight timestamps (formatting issue)
  combined_dat$timestamp_est <- format(combined_dat$timestamp_est, format = "%Y-%m-%d %H:%M:%S")
  
  # Are there any timestamps where midnight time is missing? No? Good
  sum(!grepl("....-..-.. ..:..:..", combined_dat$timestamp_est))
  
  # Get latest date
  latest_time <- (max(combined_dat$timestamp_est))
  latest_time <- gsub(":", "", latest_time)
  
  # Write
  write.csv(combined_dat, 
            file = file.path(output_dir, paste0(group, "_combined_", latest_time, ".csv")),
            row.names = F)
  
} else if(length(i) > 0){ # otherwise if there IS a combined data file...
  # Read in current dat
  original_combined <- fread(i)
  tz(original_combined$timestamp_est) <- "EST"
  
  # Combine dat with original combined data
  new_combined <- rbind(original_combined,combined_dat)
  
  new_combined <- unique(new_combined)
  
  # Add in any midnight timestamps (formatting issue)
  new_combined$timestamp_est <- format(new_combined$timestamp_est, format = "%Y-%m-%d %H:%M:%S")
  
  # Are there any timestamps where midnight time is missing? No? Good
  sum(!grepl("....-..-.. ..:..:..", new_combined$timestamp_est))
  
  # Get latest date
  latest_time <- (max(new_combined$timestamp_est))
  latest_time <- gsub(":", "", latest_time)
  
  # Write
  write.csv(new_combined, 
            file = file.path(output_dir, paste0(group, "_combined_", latest_time, ".csv")),
            row.names = F)
  
  file.remove(i)
}
