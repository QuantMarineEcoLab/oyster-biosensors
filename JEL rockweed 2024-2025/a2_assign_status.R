# Script goals: Create derived vars (assign open/closed and join morphology data)
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

# Create time ladder for each oyster ID
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

# ------ NORMALIZE DATA ------
# Now...for every 48 hour window, normalize the data (Lavaud et al. 2024)
# Create max and min 
# how many minutes are in 48 hours....
60*48
key_vars <- c("oyster_id")

# Too slow
# start_time <- Sys.time()
# oyster_dat_norm <- oyster_dat_full %>%
#   group_by(across(all_of(key_vars))) %>%
#   mutate(min = zoo::rollapply(mean_voltage, 2880, min, na.rm = T, partial = T,
#                               align = "center"),
#          max = zoo::rollapply(mean_voltage, 2880, max, na.rm = T, partial = T, 
#                               align = "center"))
# end_time <- Sys.time()
# end_time-start_time

# Now create rolling min and max
start_time <- Sys.time()
oyster_dat_full[, min := frollapply(x = mean_voltage, n = 2880, fill = NA, align = "center", FUN = min_rm), by = oyster_id]

oyster_dat_full[, max := frollapply(x = mean_voltage, n =  2880, FUN = max_rm,
                                    fill = NA, align = "center"), by = oyster_id]
end_time <- Sys.time()
end_time - start_time

# Copy the df
oyster_dat_norm <- oyster_dat_full

# Create row number per group
oyster_dat_norm <- oyster_dat_norm %>%
  group_by(oyster_id) %>%
  mutate(row_id = row_number())

# Identify leading or lagging zeros
oysters <- unique(oyster_dat_norm$oyster_id)

oyster_list = vector(mode = "list", length = length(oysters))
for(n in 1:length(oysters)){
  temp_dat <- oyster_dat_norm %>%
    filter(oyster_id == oysters[n])
  
  skip <- which(is.na(temp_dat$mean_voltage))
  noskip <- which(!is.na(temp_dat$mean_voltage))
  
  # leading NAs: 1:1439
  # lagging NAs: nrow-1439:nrow
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

oyster_dat_list <- rbindlist(oyster_list)

oyster_dat_list$minute_floor <- format(oyster_dat_list$minute_floor, format = "%Y-%m-%d %H:%M:%S")

# Check timestamps again (are there any midnight times that are dropped?)
filename <- file.path("~/Library/CloudStorage/OneDrive-USNH/Oyster Biosensor/0_InProgressExperiments/2024 JEL field/data", paste0("test.csv"))

oyster_dat_list %>%
  write_csv(file = filename)

# ---- I STOPPED HERE --- 
oyster_dat <- oyster_dat %>%
  group_by(vars, oyster_id, ) %>%
  
  mutate(value_norm = (value - baseline)/(max(value) - baseline))

(avg_value-min)/(max-min)

# Save and view in JMP


# Ok...let's try to assign a status, where status = open if greater than 10% percentile, and status = closed if less than 10% percentile (Lavaud et al. 2024)



# ------ ADD MORPHOLOGICAL DATA -------
# Add in morphology data at the end.
morph_dat <- fread("~/Library/CloudStorage/OneDrive-USNH/Oyster Biosensor/0_InProgressExperiments/2024 JEL field/data/schema/JEL_field_data_morphology.csv")

morph_dat <- morph_dat %>%
  mutate(timepoint = ifelse(date == "2025-04-17", "final", "initial"))

# Make morph dat wide
morph_dat_wide <- pivot_wider(data = morph_dat, id_cols = c(oyster_id),
                              names_from = (timepoint), 
                              values_from = c(length_mm, width_mm, depth_mm, survival))

# Combine oyster data with morph dat
oyster_dat_full <- left_join(oyster_dat, morph_dat_wide, by = "oyster_id")






