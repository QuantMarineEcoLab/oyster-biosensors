# Script goals:
# 1) Read in and combine JEL data

# Notes:
# I download data weekly (sort of) so all individual files need to be read, joined, and cleaned.
# This is a semi-manual process because the Arduino time is not always accurate.
# Unixtime in successive rows of data should increase as row number increases but this is not always the case; sometimes the Arduino goes back in time (erroneously) and needs to be corrected in R.
# Usually this can just be fixed by looking back at the previous row of data and adding 15s (or whatever the set data interval was)

# WISHLIST:
# Add row number as you read in data. If you plot row number against unixtime it should be a straight line.

# old wish:
# Add in some scenario somewhere where you save the last timestamp in the filename, strsplit the filename and check to see if the timestamp is in your time, filter it, etc. etc save

# Selina Cheng
# Created 2025-01-06 
# --------- SET UP ------------
rm(list = ls())

# Load libraries
library(tidyverse)
library(data.table)
library(lubridate)

# Write function
mean_rm <- function(x){mean(x, na.rm = T)}

# sep_datetime <- function(date){
#   if(nchar(date) > 11){
#   time <- substr(date, 12, nchar(date))
#   time <- paste0(substr(time, 1, 2), ":", substr(time, 3, 4), ":", substr(time, 5, 6))
#   date <- substr(date, 1, 10)
#   date2 <- lubridate::ymd_hms(paste(date, time))
# } else{
#   date2 <- lubridate::ymd(date)
# }
#   return(date2)
# }

# Set source directory
raw_dir <- "~/Library/CloudStorage/OneDrive-USNH/Oyster Biosensor/0_InProgressExperiments"

# Set "JEL field" or "JEL lab" as the dataset you want to combine
# group <- "JEL lab"
group <- "JEL field"

# ----------- SET UP DIRECTORIES ----------------
# Get actual directory
dirs <- list.dirs(raw_dir, full.names = T, recursive = F)
base_dir <- dirs[grepl(group, list.dirs(raw_dir, full.names = T, recursive = F))]

# Source directory for raw sensor data
source_dir <- file.path(base_dir, "data", "raw sensor data")
# Source directory for oyster schema
schema_dir <- file.path(base_dir, "data", "schema")
# Output directory for combined data
output_dir <- file.path(base_dir, "data")

# ----------- READ IN DATA -------------------
# First...search for file in output_dir
i <- setdiff(list.files(output_dir, full.names = T, recursive = F, include.dirs = F),
             list.dirs(output_dir, recursive = F, full.names = T))

# If there is no combined file....
if(length(i) == 0){
  # Read in all raw data files
  j <- list.files(source_dir, pattern = NULL, full.names = T, recursive = F)
  dat <- rbindlist(lapply(j, fread))
  
} else if(length(i) > 0){ # otherwise if there is a combined data file...
  # Get filename
  filename <- basename(i)
  name_elements <- unlist(strsplit(filename, "_"))
  date <- name_elements[length(name_elements)]
  date <- gsub("\\.csv", "", date)
  date <- substr(date, 1, 10)
  # date <- sep_datetime(date)
  
  # Get raw files dates
  j <- list.files(source_dir, pattern = NULL, full.names = T, recursive = F)
  base_j <- basename(j)
  name_elements <- strsplit(base_j, "_")
  # dates_j <- sapply(name_elements, function(x){
  #   if(length(x) > 4){
  #     date <- paste(x[3], x[4])
  #   } else{
  #     date <- x[3]
  #   }
  #   return(date)
  # })
  # dates_j <- lapply(dates_j, sep_datetime)
  
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
  select(-c("BoxTemp", "B-Volts", contains("V2")))

# --------- QAQC TIMESTAMPS --------------
# Remove any duplicate rows
dat <- unique(dat)

# Are there any duplicated timestamps?
which(duplicated(dat$unixtime))

# Look for any backwards-moving timestamps
# Create differences between times
dat$diffs <- diff(c(dat$unixtime[1]-1,dat$unixtime))

# Are any times going backwards?
which(dat$diffs < 0)

# View times where data is going backwards 
test <- dat[43370:43380,]

# For JEL FIELD
# 2025-02-18 03:59:48
# TIMESKIP BACKWARDS starting at unixtime 1739865604
# # This is row 383810
# which(dat$unixtime == 1739865604)
# # TIMESKIP BACKWARDS lasts up to unixtime 1740162881
# # This is row 403582
# which(dat$unixtime == 1740162881)
# # So for the field data, for rows 383810 to 403582, add 3600s
# dat$unixtime[383810:403582] <- dat$unixtime[383810:403582] + 3600

# # For JEL LAB
# # 2025-02-20 13:59
# # TIMESKIP BACKWARDS starting at unixtime 1740074405
# # This is row 419707
# which(dat$unixtime == 1740074405)
# # TIMESKIP BACKWARDS lasts up to unixtime 1740164103
# unique(dat$diffs)
# which(dat$unixtime == 1740164103)
# # This is row 425683
# # So for the lab data, for rows 419707 to 425683, add 3600s
# dat$unixtime[419707:425683] <- dat$unixtime[419707:425683] + 3600

# Recreate diffs to check
dat$diffs <- diff(c(dat$unixtime[1]-1,dat$unixtime))
which(dat$diffs < 0)

# Also view times if timeskip is forward by a ton?
which(dat$diffs > 1000)

# View times where data timeskips
test <- dat[19650:19670,]

# # For JEL LAB data
# # Weird timeskip...delete data past timeskip forwards
# which(dat$unixtime == 1741478393)
# dat <- dat[1:512976,]

# Recreate diffs
dat$diffs <- diff(c(dat$unixtime[1]-1,dat$unixtime))
which(dat$diffs < 0)
which(dat$diffs > 1000)

# Looks like for the most part, it's just a random and erroneous timeskip that just needs to be amended
# Change timestamp: if diffs less than 0, look back at the previous timestamp and add 15 seconds
dat <- dat %>%
  mutate(unixtime = ifelse(diffs < 0, lag(unixtime) + 15, unixtime))

# Recreate diffs and see if any times are going backwards. Did we fix it?
dat$diffs <- diff(c(dat$unixtime[1]-1,dat$unixtime))
which(dat$diffs < 0) # Yes

# Create timestamp_est
dat$timestamp_est <- as.POSIXct(dat$unixtime, origin = "1970-01-01", tz = "EST")

# Are there any duplicated timestamps?
which(duplicated(dat$timestamp_est))

# Remove diffs column
dat <- dat %>%
  select(-diffs)

# ----------- PIVOT -----------------
# Pivot longer
dat_pivot <- pivot_longer(dat, cols = names(dat)[grepl("-", names(dat))], 
                          names_to = "vars", values_to = "value")

dat_pivot$vars <- gsub("-", "", dat_pivot$vars)

# --------- ADD SCHEMA ---------------
# Read in schema dat
schema_dat <- fread(list.files(schema_dir, pattern = "schema", full.name = T))
tz(schema_dat$end_date) <- "EST"
tz(schema_dat$start_date) <- "EST"

# Add in an "end date" of schema if there isn't one
schema_dat <- schema_dat %>%
  mutate(end_date = ifelse(is.na(end_date), max(dat$timestamp_est)+1, 
                           end_date),
         end_date = as.POSIXct(end_date, origin = "1970-01-01", tz = "EST"))

# Combine schema with data
# Join based on date ranges
start <- Sys.time()
combined_dat <- left_join(dat_pivot, schema_dat, 
                         join_by(vars == port, timestamp_est >= start_date,
                                 timestamp_est < end_date))
end <- Sys.time()
end-start

# Remove where oyster_id is NA
combined_dat <- combined_dat %>%
  filter(!is.na(oyster_id))

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
