# Combine and write a clean temp data stream
# Selina Cheng, 2025-06-24
# Load libraries
library(tidyverse)
library(lubridate)
library(data.table)

# Read in data
source_dir <- "~/Library/CloudStorage/OneDrive-USNH/Oyster Biosensor/0_InProgressExperiments/2024 JEL field/data/temperature"

# Create one big dataframe
logger_names <- c("JEL_bare", "JEL_rckwd", "JEL_subtid")

temp_dat <- vector(mode = "list", length = 3)

for(n in 1:length(logger_names)){
  i <- list.files(source_dir, pattern = logger_names[n], full.names = T)
  
  bleh <- vector(mode = "list", length = 2)
  
  for(m in 1:length(i)){
    bleh[[m]] <- fread(i[m], skip = 21)
    names(bleh[[m]]) <- c("timestamp_utc", "temp_c")
  }
  
  temp_dat[[n]] <- unique(rbindlist(bleh))
  temp_dat[[n]]$logger = logger_names[n]
  
}

temp_dat <- rbindlist(temp_dat)

# Add timestamp est column
temp_dat$timestamp_est <- with_tz(temp_dat$timestamp_utc, tzone = "America/Jamaica")

# Filter to these times
# Start time:
# Fri Dec 13 2024 07:38:31
# End time:
# Thu Apr 17 2025 8:30:00
temp_dat <- temp_dat %>%
  filter(timestamp_est >= ymd_hms("2024-12-13 07:30:00", tz = "EST") & 
           timestamp_est <= ymd_hms("2025-04-17 08:30:00", tz = "EST"))

# Also remove these times:
temp_dat <- temp_dat %>%
  mutate(temp_c = ifelse(logger == "JEL_rckwd" & 
                  (timestamp_est >= ymd_hms("2025-01-03 10:00:00", tz = "EST") & 
                  timestamp_est <= ymd_hms("2025-01-03 11:15:00", tz = "EST")) |
                    (timestamp_est >= ymd_hms("2025-01-08 12:30:00", tz = "EST") &
                       timestamp_est <= ymd_hms("2025-01-08 13:35:00", tz = "EST")) |
                    (timestamp_est >= ymd_hms("2025-03-10 16:00:00", tz = "EST") &
                       timestamp_est <= ymd_hms("2025-03-10 17:30:00", tz = "EST")),
                NA, temp_c),
         temp_c = ifelse(logger == "JEL_subtid" & 
                           (timestamp_est >= ymd_hms("2025-02-07 15:00:00", tz = "EST") &
                              timestamp_est <= ymd_hms("2025-02-07 16:20:00", tz = "EST")) |
                           (timestamp_est >= ymd_hms("2025-03-10 16:00:00", tz = "EST") & 
                              timestamp_est <= ymd_hms("2025-03-10 17:30:00", tz = "EST")),
                         NA, temp_c))

# Cool...
# Much more extreme highs and lows without rockweed cover
ggplot(data = temp_dat, aes(x = timestamp_utc, y = temp_c, color = logger)) +
  geom_point()







