# Set libraries
library(tidyverse)
library(data.table)
library(lubridate)

# Set directory
dir <- "~/Library/CloudStorage/OneDrive-USNH/Oyster Biosensor/0_InProgressExperiments/2024 JEL field/data"

# Read in data
dat <- fread(list.files(path = dir, pattern = "JEL", full.names = T, recursive = F))

# Before standardizing, filter data
dat <- dat %>%
  filter(case_when(treatment != "bare" ~ value <= 2.4))
  
# Standardize data acc to baseline
dat <- dat %>%
  group_by(vars, oyster_id, baseline) %>%
  mutate(value_norm = (value - baseline)/(max(value) - baseline))

(avg_value-min)/(max-min)

# Add in any midnight timestamps (formatting issue)
dat$timestamp_est <- format(dat$timestamp_est, format = "%Y-%m-%d %H:%M:%S")

sum(!grepl("....-..-.. ..:..:..", dat$timestamp_est))

write.csv(dat, 
          file = file.path(dir, ("test_norm.csv")),
          row.names = F)


