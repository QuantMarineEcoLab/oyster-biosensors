# Created by Selina Cheng
# Last modified 6 Nov 2024
# Roughly clean JEL data (manually remove chunks of bad data)

# -------- SET UP ----------
# load libraries
library(data.table)
library(tidyverse)
library(lubridate)

# Set source directory
oyster_dir <- "~/Library/CloudStorage/OneDrive-USNH/Oyster Biosensor/0_InProgressExperiments/2023 - 2024 Continuous system on oysters and mussels/2_combined sensor data"

other_dir <- "~/Library/CloudStorage/OneDrive-USNH/Oyster Biosensor/0_InProgressExperiments/2023 - 2024 Continuous system on oysters and mussels/3_other data"

proc_dir <- "~/Library/CloudStorage/OneDrive-USNH/Oyster Biosensor/0_InProgressExperiments/2023 - 2024 Continuous system on oysters and mussels/2a_processed sensor data"

# --------- LOAD DATA ----------
# Read in data
# Read in oyster data
oyster_dat <- fread(list.files(oyster_dir, pattern = "schema", full.names=T))

# Read in oyster schema 
oyster_schema <- fread(list.files(other_dir, pattern = "schema", full.names =T))

# --------- Reduce datastream to just the bivalves we care about  -------------
keep <- oyster_schema %>% filter(keep == "y") %>%
  mutate(unique_id = paste0(sensor, "-", tag))
keep <- unique(keep$unique_id)

dat_reduced <- oyster_dat %>% 
  filter(unique_id %in% keep)

dat_reduced <- dat_reduced %>%
  select(timestamp_est, unique_id, species, died, value)

# --------- Remove chunks of bad data -------------
# Read in oyster removal doc
remove_data <- fread(list.files(other_dir, pattern = "remove", full.names =T))

# Divide into "all" applicable timestamps and specific to indiv timestamps
remove_data_all <- remove_data %>% filter(unique_id == "all") %>% select(start, end)
remove_data_indiv <- remove_data %>% filter(unique_id != "all") %>% mutate(remove = "y")

# Create cleaned dataset
dat_clean <- dat_reduced

for(n in 1:nrow(remove_data_all)){
  # Remove relevant timestamps for all rows
  dat_clean <- dat_clean %>%
    filter((timestamp_est < remove_data_all$start[n]) | (timestamp_est > remove_data_all$end[n]))
}

# Join with remove_data_indiv and then remove after?
dat_clean <- left_join(dat_clean, remove_data_indiv,
                       join_by(unique_id, timestamp_est >= start, timestamp_est <= end))

dat_clean <- dat_clean %>% 
  filter(is.na(remove)) %>%
  select(timestamp_est, unique_id, species, died, value)

rm(oyster_dat)
gc()

# Now dat_clean should be clean...try joining, saving, and viewing?
# Save file
dat_clean <- dat_clean %>% 
  mutate(status = "clean")
dat_reduced <- dat_reduced %>%
  mutate(status = "raw")

dat_save <- rbind(dat_clean, dat_reduced) %>%
  pivot_wider(id_cols = c(timestamp_est, unique_id, species, died),
              names_from = status, names_prefix = "value_", values_from = value) %>%
  mutate(timestamp_est = format(timestamp_est, format = "%Y-%m-%d %H:%M:%S")) %>%
  write.table(file = file.path(proc_dir, paste0("JEL-continuous-oyster_rough_clean.csv")),
              append = F, sep = ",", na = "NA", dec = ".", row.names = F, col.names = T)




