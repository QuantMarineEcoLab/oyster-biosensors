# Created by Selina Cheng
# Last modified 6 Nov 2024
# Combine JEL schema data with raw data

# -------- SET UP ----------
# load libraries
library(data.table)
library(tidyverse)
library(lubridate)

# Set source directory
oyster_dir <- "~/Library/CloudStorage/OneDrive-USNH/Oyster Biosensor/0_InProgressExperiments/2023 - 2024 Continuous system on oysters and mussels/2_combined sensor data"

other_dir <- "~/Library/CloudStorage/OneDrive-USNH/Oyster Biosensor/0_InProgressExperiments/2023 - 2024 Continuous system on oysters and mussels/3_other data"

figs_dir <- "~/Library/CloudStorage/OneDrive-USNH/Oyster Biosensor/0_InProgressExperiments/2023 - 2024 Continuous system on oysters and mussels/4_figures"

# -------- READ DATA ----------
# Read in oyster data
oyster_dat <- fread(list.files(oyster_dir, pattern = "twelvesec", full.names=T))

# Read in oyster schema 
oyster_schema <- fread(list.files(other_dir, pattern = "schema", full.names =T))

# Read in events log
events_dat <- fread(list.files(other_dir, pattern = "event", full.names =T))

# ------- PROCESS DATA ------------
# For each sensor stream and date range from oyster schema...join with appropriate data...
# Pivot oyster_dat to long
oyster_dat <- oyster_dat %>%
  select(-seconds_since_start) %>%
  pivot_longer(cols = contains("ID"), names_to = "sensor", values_to = "value")

# Make sure end date is as timestamp
oyster_schema <- oyster_schema %>%
  mutate(end_ymdhms_est = ymd_hms(end_ymdhms_est))

# Join based on date ranges
start <- Sys.time()
oyster_full <- left_join(oyster_dat, oyster_schema, 
                         join_by(sensor, timestamp_est >= start_ymdhms_est, timestamp_est < end_ymdhms_est))
end <- Sys.time()
end-start

rm(oyster_dat)
gc()

# Timestamp, sensor, tag combinations should be unique.
test <- oyster_full %>%
  select(timestamp_est, sensor, tag)

setDT(test)
# Are there any duplicate combinations? No
anyDuplicated(test)

# Combine with events data
event_repair <- events_dat %>%
  filter(impact != "all") %>%
  rename(event_start = start_ymdhms_est, event_end = end_ymdhms_est)

# For repair events filter out data
start <- Sys.time()
oyster_full_events <- left_join(oyster_full, event_repair, 
                         join_by(sensor==impact, timestamp_est >= event_start, timestamp_est < event_end))
end <- Sys.time()
end-start

# Remove sensor repair events
oyster_full_events <- oyster_full_events %>%
  filter(is.na(event)) %>% 
  select(-c(event, event_start, event_end))

# Also remove any times where tag is NA (no organism attached to sensor)
oyster_full_events <- oyster_full_events %>%
  filter(!is.na(tag))

# Create unique id
oyster_full_events$unique_id <- paste0(oyster_full_events$sensor, "-", oyster_full_events$tag)

# Save file
oyster_full_events %>%
  mutate(timestamp_est = format(timestamp_est, format = "%Y-%m-%d %H:%M:%S")) %>%
  write.table(file = file.path(oyster_dir, paste0("JEL-continuous-oyster_full_schema.csv")),
            append = F, sep = ",", na = "NA", dec = ".", row.names = F, col.names = T)

# Create ggplot of oyster and events
events_all <- events_dat %>%
  filter(impact == "all")

# Create unique ID for all oysters
id <- unique(oyster_full_events$unique_id)

for(n in 1:length(id)){
  dat <- oyster_full_events %>%
    filter(unique_id == id[n])
  
  p <- ggplot() +
    geom_rect(data = events_all, aes(xmin = start_ymdhms_est , xmax = end_ymdhms_est, ymin = -Inf, ymax = Inf), inherit.aes=FALSE, fill = c("red"))+
    geom_line(data = dat, aes(x = timestamp_est, y = value))+
    scale_x_datetime(date_breaks = "1 month")+
    labs(title=paste0("Bivalve ID: ",id[n]),y="Voltage")
  
  ggsave(filename=paste0(id[n],"_gaping.png"), plot=p, path=figs_dir, width=10,height=6, units="in")
    
}
  


