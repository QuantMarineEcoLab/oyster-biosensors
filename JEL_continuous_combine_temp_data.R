# Code to join temperature data 
# ALSO correct EDT/EST shift ... 
# By Selina Cheng
# Last modified 20 Oct 2024

# -------------- SETUP ---------------------
# Load libraries
library(tidyverse)
library(lubridate)
library(readxl)
library(data.table)

# Set source directory
source_dir <- "~/Library/CloudStorage/OneDrive-USNH/Oyster Biosensor/0_InProgressExperiments/2023 - 2024 Continuous system on oysters and mussels/X_archive data/temp data"

# Set output directory
output_dir <- "~/Library/CloudStorage/OneDrive-USNH/Oyster Biosensor/0_InProgressExperiments/2023 - 2024 Continuous system on oysters and mussels/3_other data"

# List source files
i <- list.files(source_dir, pattern = NULL, recursive = F, full.names=T)

# ------------- READ DATA ----------------
# For all files, read in first file and rbind
data_list <- vector(mode = "list", length = length(i))

for(n in 1:length(i)){
  # Read in sheet 1
  dat <- read_excel(i[n], sheet = 1)

  # Remove first col
  dat <- dat %>%
    select(-1)
  
  # Set colnames
  names(dat) <- c("timestamp_est", "temp_c", "light_lux")
  
  # Set dat = position in data_list
  data_list[[n]] <- dat
}

# Combine all data, get only unique values
temp_dat <- rbindlist(data_list)  
temp_dat <- unique(temp_dat)
  
# ---------- REDO TIMESTAMPS -------------------
# FYI all data is 10 min interval....
# until 7/19/2024  12:52:00 PM which is when data starts being collected at 1 min intervals

# Default timestamp is UTC
# In 2023, switch to EST happened Nov 5, 2 AM (2 AM jumps back to 1 AM)
# Rows 1-20652 are EDT. Row 20652 is the last EDT date (2023-11-05 01:50:00)
# After 20652, 1 AM times are repeated. Demo:
temp_dat$timestamp_est[duplicated(temp_dat$timestamp_est)]

# Ok. Fix first EDT interval
temp_dat$timestamp_est[1]
temp_dat$timestamp_est[20652]

# Set first EDT interval backwards 1 hr
temp_dat$timestamp_est[1:20652] <- temp_dat$timestamp_est[1:20652] - 3600

# View first EDT interval
temp_dat$timestamp_est[1]
temp_dat$timestamp_est[20652]
# Still duplicated?
temp_dat$timestamp_est[duplicated(temp_dat$timestamp_est)]
# No, seems fixed.

# THEN, we go from Nov '23 to March '24 in EST. Nice and normal.

# The switch to EDT in 2024 happens Mar 10, 2 AM (2 AM jumps forward to 3 AM)
# Rows 38802-end are EDT. Row 38802 is the first EDT date (2024-03-10 03:00:00)
# We are missing the 2-3 AM hour in March 2024.

# Ok. Fix second EDT interval
temp_dat$timestamp_est[38802]
temp_dat$timestamp_est[nrow(temp_dat)]

# Set second EDT interval backwards 1 hr
temp_dat$timestamp_est[38802:nrow(temp_dat)] <- temp_dat$timestamp_est[38802:nrow(temp_dat)] - 3600

# View second EDT interval
temp_dat$timestamp_est[38802]
temp_dat$timestamp_est[nrow(temp_dat)]

# Create plot
ggplot(data = temp_dat, aes(x = timestamp_est, y = light_lux))+
  geom_point()

# ----------- SAVE DATA ----------------
# Data should be good to go..
which(!grepl("....-..-.. ..:..:..", temp_dat$timestamp))

temp_dat$timestamp_est <- format(temp_dat$timestamp_est, format = "%Y-%m-%d %H:%M:%S")

# Check timestamps again (are there any midnight times that are dropped?)
which(!grepl("....-..-.. ..:..:..", temp_dat$timestamp))

write.table(temp_dat, file = file.path(output_dir, "JEL-continuous_temperature_light_data.csv"),
            append = F, sep = ",", na = "NA", dec = ".", row.names = F, col.names = T)






  
  