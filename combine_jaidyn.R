# Combine Jaidyn's data
# Load libraries
library(tidyverse)
library(data.table)
library(lubridate)

output_dir <- "~/Library/CloudStorage/OneDrive-USNH/QMEL/0_People/Undergrads/JaidynThomas/data"

filepath_pre <- "~/Library/CloudStorage/OneDrive-USNH/QMEL/0_People/Undergrads/JaidynThomas/data/pre-experiment"

filepath_during <- "~/Library/CloudStorage/OneDrive-USNH/QMEL/0_People/Undergrads/JaidynThomas/data/2025-06-26_experiment"


j <- list.files(filepath_pre, pattern = NULL, full.names = T, recursive = F)
i <- list.files(filepath_during, pattern = NULL, full.names = T, recursive = F)

i <- c(j,i)
i <- i[-1]
dat <- rbindlist(lapply(i, fread))

# ---------- MANIPULATE COLUMNS ----------
# Change first colname
names(dat)[1] <- "unixtime"

# Remove extra columns
dat <- dat %>%
  select(-c("BoxTemp", "B-Volts", contains("V2")))

# Remove any duplicate rows
dat <- unique(dat)

# Are there any duplicated timestamps?
which(duplicated(dat$unixtime))

# Look for any backwards-moving timestamps
# Create differences between times
dat$diffs <- diff(c(dat$unixtime[1]-1,dat$unixtime))

# Are any times going backwards?
which(dat$diffs < 0)

dat$timestamp_est <- as.POSIXct(dat$unixtime, origin = "1970-01-01", tz = "EST")

dat$timestamp_est <- format(dat$timestamp_est, format = "%Y-%m-%d %H:%M:%S")

write.csv(dat, 
          file = file.path(output_dir, "jaidyn_data.csv"),
          row.names = F)
