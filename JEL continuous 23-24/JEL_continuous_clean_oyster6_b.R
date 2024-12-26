# Created by Selina Cheng
# Last modified 19 Nov 2024
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
# library(signal)
# library(plotly)
library(zoo)

# Set dirs
source_dir <- "~/Library/CloudStorage/OneDrive-USNH/Oyster Biosensor/0_InProgressExperiments/2023 - 2024 Continuous system on oysters and mussels/2a_processed sensor data"

output_dir <- "~/Library/CloudStorage/OneDrive-USNH/Oyster Biosensor/0_InProgressExperiments/2023 - 2024 Continuous system on oysters and mussels/2b_derived sensor data"

# ---------- READ DATA -----------
# Read in rough cleaned oyster data
oyster_dat <- fread(list.files(source_dir, pattern = "6", full.names=T))

# ergh..remove NAs from data
oyster_dat <- oyster_dat %>%
  dplyr::filter(!is.na(mean_value_clean))

# Get lengths and values of runs of equal values in a vector
# oyster_dat$status_numeric <- ifelse(oyster_dat$new_status == "open", 1, 0)
runs <- rle(oyster_dat$status)
runs <- data.frame(status = runs$values, length = runs$lengths)

# Create diff
oyster_dat$change <- c(diff(oyster_dat$status_numeric),0)
oyster_dat$above_midpoints <- oyster_dat$mean_value_clean > oyster_dat$midpoints

# Diffs represent status of (n+1) being different from (n)
# A diff of 1 means an oyster changed from closed to open
# A diff of -1 means an oyster changed from open to closed
# When change != 0, it coincides with the end of a status (not the beginning of a new status)

# Add where the change occurs so we know where to look
runs$change_rows <- c(which(oyster_dat$change != 0), NA)
runs$above_mid <- c(oyster_dat$above_midpoints[which(oyster_dat$change != 0)], T)

# Which rows correspond to may through september?
which(month(oyster_dat$minute_floor_est)==5)[1]

# Flag is TRUE if it needs to be changed to match previous value
runs$flag <- ifelse(runs$change_rows-runs$length+1 >= 204439 & runs$above_mid, 
                    ifelse(runs$length < 20 & runs$length > 8, T, F), ifelse(runs$length < 20, T, F))

# NOW do runs of flag = T?
runs_flag <- rle(runs$flag)
# Expand lengths
flag_lengths <- rep(runs_flag$lengths, times = runs_flag$lengths)
runs <- cbind(runs, flag_lengths)
# # When diff !=0 it is the start of a new group
runs$diff <- c(1, diff(runs$flag))
runs$diff <- ifelse(runs$diff != 0, "start", "group")
# Create ignore column
runs$ignore <- ifelse(runs$length == 1, "IGNORE", NA)

# Change oyster status...
oyster_dat$new_status <- oyster_dat$status

# Get runs of true only
runs_fix <- runs %>% dplyr::filter(flag, flag_lengths > 1)
# Find starts of runs
events <- which(runs_fix$diff == "start")

# Expand
# runs_ignore <- runs %>% dplyr::filter(flag, flag_lengths == 1)
# rows_lengths <- c()
# for(n in 1:nrow(runs_ignore)){
#   vector <- seq(from = runs_ignore$change_rows[n], length = runs_ignore$length[n])
#   rows_lengths <- c(rows_lengths, vector)
# }
# check_rows <- data.frame(index = rows_lengths, ignore = "IGNORE")
# runs_ignore <- runs %>% dplyr::filter(!is.na(runs$ignore)) %>% select(length, change_rows, ignore) 
# runs_ignore <- runs_ignore[runs_ignore$change_rows %in% runs_fix$change_rows == F,]
# oyster_dat <- left_join(oyster_dat, runs_ignore, by = c("index" = "change_rows"))

# If runs of true are greater than 3, change status
start <- Sys.time()
for(n in events){
  # Get first row of issues
    first_row <- runs_fix$change_rows[n]
  # get length of issues
    length <- runs_fix$flag_lengths[n]
  # Get last row of issues
    last_row <- runs_fix$change_rows[n+length-1]
    
    # Get length of first event
    length_dat <- runs_fix$length[n]
  # From first row to last row, change status to the one before the first row
    oyster_dat$new_status[(first_row-length_dat+1):last_row] <- oyster_dat$status[(first_row-length_dat)]
}
end <- Sys.time()
end-start

# The runs code should correct anywhere that there are a bunch of erroneous status assignments 
# Save runs data
oyster_dat %>%
  mutate(minute_floor_est = format(minute_floor_est, format = "%Y-%m-%d %H:%M:%S")) %>%
  write.table(file = file.path(output_dir, paste0("JEL-continuous-oyster6_fix.csv")),
              append = F, sep = ",", na = "NA", dec = ".", row.names = F, col.names = T)









