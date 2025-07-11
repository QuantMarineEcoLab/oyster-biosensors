# Script goals: 
# 1) Normalize data so that min is set to 0 and max is set to 1.
# 2) Assign open/closed status to oysters based on 0-1 range. (data > 10th percentile is open)

# Selina Cheng
# July 2 2025
# ------ SET UP ------
# Load libraries
library(tidyverse)
library(data.table)
library(lubridate)
library(zoo)

rm(list=ls())

# ---- FUNCTIONS  ----
max_rm <- function(x){max(x, na.rm = T)}
min_rm <- function(x){min(x, na.rm = T)}

sd_rm <- function(x){sd(x, na.rm = T)}

range_rm <- function(x){
  range <- range(x, na.rm = T)
  return(diff(range))
}

mean_trim <- function(x){mean(x, trim = 0.25, na.rm = T)}

# ------- LOAD AND FILL DATA --------
# Read in oyster dat 
oyster_dat <- fread("~/Library/CloudStorage/OneDrive-USNH/Oyster Biosensor/0_InProgressExperiments/2024 JEL field/data/rockweed_manual_clean_1min_2024-12-14_TO_2025-04-17.csv")

# Ok..let's try to normalize the data..
# Order data by time
oyster_dat <- oyster_dat %>%
  arrange(minute_floor)

# Add in any missing timestamps before using rolling window, otherwise window will roll over to other timestamps erroneously!
# Note to self instead of filtering out the rows when cleaning data I should have just <- NA 
# :/
timeladder <- seq(min(oyster_dat$minute_floor),max(oyster_dat$minute_floor),by="1 min")

# Create full time series for each oyster ID
timeladder_dt <- data.frame(timeladder_min = rep(timeladder, 
                                                 times = length(unique(oyster_dat$oyster_id))),
                            oyster_id = rep(unique(oyster_dat$oyster_id), each = length(timeladder)))

# check that timeladder_dt is right?
# They should all print true
for(n in 1:length(unique(timeladder_dt$oyster_id))){
  check <- timeladder_dt %>% filter(oyster_id == oyster_id[n])
  
  sum <- sum(check$timeladder_min == timeladder)
  
  print(sum == length(timeladder))
}

# Ok now full join timeladder_dt to oyster_dat so we have all the timestamps we need.
oyster_dat_full <- full_join(oyster_dat, timeladder_dt,
                             by = c("minute_floor" = "timeladder_min", "oyster_id"))

# Order data by time again
oyster_dat_full <- oyster_dat_full %>%
  arrange(minute_floor)

rm(oyster_dat)
rm(timeladder_dt)
rm(check)

# ------ ROLLING MAX/MIN ------
# Now...for every 48 hour window, normalize the data (Lavaud et al. 2024)
# Create max and min 
# how many minutes are in 48 hours....
60*48
key_vars <- c("oyster_id")

# Too slow to use dplyr and zoo. Use data.table.
# start_time <- Sys.time()
# oyster_dat_norm <- oyster_dat_full %>%
#   group_by(across(all_of(key_vars))) %>%
#   mutate(min = zoo::rollapply(mean_voltage, 2880, min, na.rm = T, partial = T,
#                               align = "center"),
#          max = zoo::rollapply(mean_voltage, 2880, max, na.rm = T, partial = T, 
#                               align = "center"))
# end_time <- Sys.time()
# end_time-start_time

# Ughh wait, backtrack.
# For oysters that were dead, we should only normalize data for the time they were alive.
# I identified an approximate time of death here: 
# These are rough guesses though
# Data for 14, 16, 22, 24, J23 look very noisy. The resolution of the data is pretty poor. Drop....?
# Also drop 19 and 23
# I also don't love J24
death_dat <- fread("~/Library/CloudStorage/OneDrive-USNH/Oyster Biosensor/0_InProgressExperiments/2024 JEL field/data/schema/JEL_death.csv")

oyster_dat_full <- left_join(oyster_dat_full, death_dat, join_by(oyster_id, minute_floor >= death_time))

# Remove "dead" data and save somewhere else...
oyster_dat_dead <- oyster_dat_full %>% 
  filter(!is.na(death_time))

# Ok, now this is just alive data.
oyster_dat_alive <- oyster_dat_full %>%
  filter(is.na(death_time))

# Now create rolling min and max
# Rolling window over 48 hr 
start_time <- Sys.time()
oyster_dat_alive[, min := frollapply(x = mean_voltage, n = 2880, fill = NA, align = "center", FUN = min_rm), by = oyster_id]

oyster_dat_alive[, max := frollapply(x = mean_voltage, n =  2880, FUN = max_rm,
                                    fill = NA, align = "center"), by = oyster_id]
end_time <- Sys.time()
end_time - start_time

# Copy the df
oyster_dat_norm <- oyster_dat_alive

# Create row number per group
oyster_dat_norm <- oyster_dat_norm %>%
  group_by(oyster_id) %>%
  mutate(row_id = row_number())

# Fill in min or max for leading or lagging NAs
# get list of oysters to run for loop over
oysters <- unique(oyster_dat_norm$oyster_id)

# for each oyster...
oyster_list = vector(mode = "list", length = length(oysters))
for(n in 1:length(oysters)){
  # Create temporary dataset
  temp_dat <- oyster_dat_norm %>%
    filter(oyster_id == oysters[n])
  
  # Which lines of data should we skip because they don't exist?
  skip <- which(is.na(temp_dat$mean_voltage))
  noskip <- which(!is.na(temp_dat$mean_voltage))
  
  # leading NAs: 1:1439
  # lagging NAs: nrow-1439:nrow
  
  # Doing some serious indexing to fill in the leading and lagging mins and maxes.
  # Where there are leading and lagging NAs, this section fills them in with the min/max of the leading/lagging time period.
  # This seems to work ok.
  # This is complicated by some NAs at the start and end of the data which are TRUE NAs.So the indexing does not always fit with start = 1 and end = nrow
  if(skip[1] >= 1440){
    temp_dat$min[1:1439] <- min_rm(temp_dat$mean_voltage[1:1439])
    temp_dat$max[1:1439] <- max_rm(temp_dat$mean_voltage[1:1439])
  } else{
    temp_dat$min[noskip[1]:(noskip[1]+1438)] <- min_rm(temp_dat$mean_voltage[noskip[1]:(noskip[1]+1438)])
    temp_dat$max[noskip[1]:(noskip[1]+1438)] <- max_rm(temp_dat$mean_voltage[noskip[1]:(noskip[1]+1438)])
  }
  if(noskip[length(noskip)] <= (nrow(temp_dat)-1440)){
    temp_dat$min[(nrow(temp_dat)-1439):nrow(temp_dat)] <- 
      min_rm(temp_dat$mean_voltage[(nrow(temp_dat)-1439):nrow(temp_dat)])
    temp_dat$max[(nrow(temp_dat)-1439):nrow(temp_dat)] <-
      max_rm(temp_dat$mean_voltage[(nrow(temp_dat)-1439):nrow(temp_dat)])
  } else{
    temp_dat$min[(noskip[length(noskip)]-1439):noskip[length(noskip)]] <-
      min_rm(temp_dat$mean_voltage[(noskip[length(noskip)]-1439):noskip[length(noskip)]])
    temp_dat$max[(noskip[length(noskip)]-1439):noskip[length(noskip)]] <- 
      max_rm(temp_dat$mean_voltage[(noskip[length(noskip)]-1439):noskip[length(noskip)]])
  }
  
  oyster_list[[n]] <- temp_dat
}

# create one data.table from the list
oyster_dat_list <- rbindlist(oyster_list)

# Check that the for loop did what I wanted
# oyster_dat_list$minute_floor <- format(oyster_dat_list$minute_floor, format = "%Y-%m-%d %H:%M:%S")
# 
# filename <- file.path("~/Library/CloudStorage/OneDrive-USNH/Oyster Biosensor/0_InProgressExperiments/2024 JEL field/data", paste0("test.csv"))
# 
# oyster_dat_list %>%
#   write_csv(file = filename)

# ----------- Identify events where baseline changes --------------
# Drop NAs in dataset
oyster_dat_list <- oyster_dat_list %>%
  filter(!is.na(mean_voltage))

# Look at differences among the rolling min value
# If the difference is large (greater than 10% of the range of the whole dataset), it is likely a baseline change event.
oyster_dat_list <- oyster_dat_list %>%
  group_by(oyster_id) %>%
  mutate(diffs = c(0, diff(min)),
         range = diff(range(mean_voltage, na.rm = T)),
         flag = ifelse(abs(diffs) > (0.1*range), 1, 0))

# For each flag, name it with an event
oysters <- unique(oyster_dat_list$oyster_id)

oysters_list <- vector(mode = "list", length = length(oysters))
for(n in 1:length(oysters)){
  # Get each oyster dataset
  temp_dat <- oyster_dat_list %>%
    filter(oyster_id == oysters[n])
  
  # Set first event
  temp_dat$flag[1] <- 1
  
  # Get number of event flags
  flag_sum <- sum(temp_dat$flag == 1)
  
  # Create categorical event names
  event_seq <- seq(from = 1, to = flag_sum, by = 1)
  event_names <- paste0("event", event_seq)
    
  # Where does each event start?
  rows <- which(temp_dat$flag == 1)
  
  # Create event column
  temp_dat$event <- temp_dat$flag
  temp_dat$event <- ifelse(temp_dat$event == 0, NA, temp_dat$event)
    
  # Assign event names to each flag
  temp_dat$event[rows] <- event_names
    
  # Now replace each NA with the most recent non-NA prior
  temp_dat$event <- zoo::na.locf(temp_dat$event, fromLast = F)

  oysters_list[[n]] <- temp_dat
}

oyster_dat_list <- rbindlist(oysters_list)

oyster_dat_list <- oyster_dat_list %>%
  group_by(oyster_id, event) %>%
  mutate(event_range = diff(range(mean_voltage, na.rm = T)))

# Ok...let's try to assign a status, where status = open if greater than 10th, and status = closed if less than 10% percentile (Lavaud et al. 2024)
oyster_dat_list <- oyster_dat_list %>%
  group_by(oyster_id) %>%
  mutate(voltage_norm = (mean_voltage - min)/(max - min),
         range = diff(range(mean_voltage, na.rm = T))) %>%
  
# So if the max-min is very small, that's probably because the oyster is closed.
  # So assign closed automatically if the difference between the max and min is very small compared to the range of the data.
  # Then -- if voltage is greater than 10th percentile, assign open.
  mutate(status = ifelse((max-min) < (0.1*range) & (max-min) < (0.25*event_range),
                         "closed", 
                         ifelse(voltage_norm >= 0.1*max, "open", "closed")))

# Check that the function did what I wanted
oyster_dat_list$minute_floor <- format(oyster_dat_list$minute_floor, format = "%Y-%m-%d %H:%M:%S")

filename <- file.path("~/Library/CloudStorage/OneDrive-USNH/Oyster Biosensor/0_InProgressExperiments/2024 JEL field/data", paste0("test.csv"))

oyster_dat_list %>%
  write_csv(file = filename)


# ---- NEXT STEPS: ------

# 1) use rle() to remove any erroneous status assignments
# 2) create variables like: % of time open, number of switches
# It would be nice to do mean valve opening amplitude or max valve opening amplitude but I don't think I can do that with the data I have.




















