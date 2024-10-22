# Created by Selina Cheng
# Last modified 22 Oct 2024
# Combine all of JEL data together

# ----------- SET UP ----------------
# Load libraries
library(tidyverse)
library(data.table)
library(lubridate)

# Create mean_rm function
mean_rm <- function(x){mean(x, na.rm = T)}

# Set source dirs
raw_dir <- "/Users/sel/Library/CloudStorage/OneDrive-USNH/Oyster Biosensor/0_InProgressExperiments/2023 - 2024 Continuous system on oysters and mussels/1_raw sensor data"

combined_dir <- "/Users/sel/Library/CloudStorage/OneDrive-USNH/Oyster Biosensor/0_InProgressExperiments/2023 - 2024 Continuous system on oysters and mussels/2_combined sensor data"

# ----------------- COMBINE DATA -------------- 
# Read in clean combined data
k <- list.files(combined_dir, pattern = "twelvesec", full.names = T)
old_dat <- fread(k)

# Get list of all new files
i <- list.files(raw_dir, pattern = NULL, all.files = T, recursive = F, full.names = T, include.dirs = F)
i <- i[-c(1,2)]

# Find file of latest date
last_file_processed <- which(grepl("2024-07-26", i))
recent_i <- i[(last_file_processed+1):length(i)]

# Read in first file
dat <- readLines(recent_i[1])

start <- Sys.time()
# Process all the rest of the data
for(n in 2:length(recent_i)){
  # Read in remaining files
  dat_plus <- readLines(recent_i[n])
  # Append dat_plus to dat
  dat <- append(dat, dat_plus)
  dat <- unique(dat)
}
end <- Sys.time()
end-start

# Broo.........why are there commas AND tab AND semicolon????
# Remove all lines with "Initializing"
dat <- dat[!grepl("Initializ", dat)]
# Remove semicolons
dat <- gsub(";", "", dat)
# Remove tab delimiter from each line and replace with comma delimiter
dat <- gsub("\\\t", ",", dat)

# Some lines have less than 17 columns, remove them if they have fewer
# Split up each element of dat into its columns
col_split <- str_split(dat, ",")
# Get number of columns
num_cols <- sapply(col_split, length)

# Remove any elements of dat that do not have ncol = 17
dat <- dat[-which(num_cols != 17)]

# Now collapse dat into a dataframe
dat_df <- fread(paste(dat, collapse = "\n"))

# Combine dat with old data
combined_dat <- rbind(old_dat, dat_df, use.names = F)
combined_dat <- unique(combined_dat)

# Change colname to timestamp_tz
names(combined_dat)[1] <- "timestamp_est"

# ---------- FIX TIMEZONE ------------------
# In 2023, switch to EST happened Nov 5, 2 AM (2 AM jumps back to 1 AM)
# THEN, we go from Nov '23 to March '24 in EST. Nice and normal.
# The switch to EDT in 2024 happens Mar 10, 2 AM (2 AM jumps forward to 3 AM)
# We are missing the 2-3 AM hour in March 2024.
# Rows 38802-end are EDT. Row 38802 is the first EDT date (2024-03-10 03:00:00)

# Get index of last timestamp in EST
edt_start <- which(date(combined_dat$timestamp_est) == "2024-03-10" & 
                     hour(combined_dat$timestamp_est) == 1)
edt_start <- edt_start[length(edt_start)]

# So now edt_start+1 is the index of the first timestamp in EDT
# View timestamps
combined_dat$timestamp_est[edt_start+1]
combined_dat$timestamp_est[nrow(combined_dat)]

# Set EDT interval backwards 1 hr
combined_dat$timestamp_est[(edt_start+1):nrow(combined_dat)] <- 
  combined_dat$timestamp_est[(edt_start+1):nrow(combined_dat)] - 3600

# View timestamps again
combined_dat$timestamp_est[edt_start+1]
combined_dat$timestamp_est[nrow(combined_dat)]

# ------------ SAVE RAW DATA ----------------
# Save data as new combined version
min_date <- min(date(combined_dat$timestamp_est))
max_date <- max(date(combined_dat$timestamp_est))

# Check timestamps (are there any midnight times that are dropped?)
which(!grepl("....-..-.. ..:..:..", combined_dat$timestamp_est))

combined_dat$timestamp_est <- format(combined_dat$timestamp_est, format = "%Y-%m-%d %H:%M:%S")

# Check timestamps again (are there any midnight times that are dropped?)
which(!grepl("....-..-.. ..:..:..", combined_dat$timestamp_est))

# Write data
write.table(combined_dat, file = file.path(combined_dir, 
                                           paste0("JEL-continuous-oyster_", min_date, "_TO_", max_date, "_twelvesec.csv")),
            append = F, sep = ",", na = "NA", dec = ".", row.names = F, col.names = T)

# ------------------- ROUND TO NEAREST 1 MIN & 15 MIN -----------------------
min_date <- "2023-11-20"
max_date <- "2024-09-22"
# Read in data
combined_dat <- fread(file.path(combined_dir, paste0("JEL-continuous-oyster_", min_date, "_TO_", max_date, "_twelvesec.csv")))

# Pivot longer
combined_dat_longer <- combined_dat %>%
  select(-seconds_since_start) %>%
  pivot_longer(cols = contains("ID"), names_to = "oyster_id", values_to = "volts")

# Round timestamps to average oyster gape data
combined_dat_longer <- combined_dat_longer %>%
  # Create new column for minute flooring
  mutate(minute_floor = floor_date(timestamp_est, "minute"),
         fifteenmin_floor = floor_date(timestamp_est, "15 minute"))

# Create a 1-min rounded dataset
start <- Sys.time()
combined_dat_1min <- combined_dat_longer %>%
  group_by(minute_floor, oyster_id) %>%
  summarise(mean_voltage = mean_rm(volts))
end <- Sys.time()
end-start

# Write file
min_date <- min(date(combined_dat_1min$minute_floor))
max_date <- max(date(combined_dat_1min$minute_floor))

# Check timestamps again (are there any midnight times that are dropped?)
which(!grepl("....-..-.. ..:..:..", combined_dat_1min$minute_floor))

combined_dat_1min$minute_floor <- format(combined_dat_1min$minute_floor, format = "%Y-%m-%d %H:%M:%S")

# Check timestamps again (are there any midnight times that are dropped?)
which(!grepl("....-..-.. ..:..:..", combined_dat_1min$minute_floor))

filename <- file.path(combined_dir, paste0("JEL-continuous-oyster_", min_date, "_TO_", max_date, "_1min.csv"))

combined_dat_1min %>%
  # pivot_wider(id_cols = minute_floor, names_from = "oyster_id", values_from = "mean_voltage") %>%
  write_csv(file = filename)

# Create a 15-min rounded dataset
start <- Sys.time()
combined_dat_15min <- combined_dat_longer %>%
  group_by(fifteenmin_floor, oyster_id) %>%
  summarise(mean_voltage = mean_rm(volts))
end <- Sys.time()
end-start

# Write file
min_date <- min(date(combined_dat_15min$fifteenmin_floor))
max_date <- max(date(combined_dat_15min$fifteenmin_floor))
filename <- file.path(combined_dir, paste0("JEL-continuous-oyster_", min_date, "_TO_", max_date, "_15min.csv"))

# Check timestamps again (are there any midnight times that are dropped?)
which(!grepl("....-..-.. ..:..:..", combined_dat_15min$fifteenmin_floor))

combined_dat_15min$fifteenmin_floor <- format(combined_dat_15min$fifteenmin_floor, format = "%Y-%m-%d %H:%M:%S")

# Check timestamps again (are there any midnight times that are dropped?)
which(!grepl("....-..-.. ..:..:..", combined_dat_15min$fifteenmin_floor))

combined_dat_15min %>%
  # pivot_wider(id_cols = fifteenmin_floor, names_from = "oyster_id", values_from = "mean_voltage") %>%
  write_csv(file = filename)




