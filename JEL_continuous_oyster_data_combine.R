# Created by Selina Cheng
# Last modified 06 Sept 2024
# Looking at data from continuous experimental setup at JEL

# Load libraries
library(tidyverse)
library(data.table)
library(lubridate)

# Set source dirs
raw_dir <- "/Users/sel/Library/CloudStorage/OneDrive-USNH/Oyster Biosensor/0_InProgressExperiments/2023 - 2024 Continuous system on oysters and mussels/downloaded data over duration of exp"

old_combined_dir <- "/Users/sel/Library/CloudStorage/OneDrive-USNH/Oyster Biosensor/0_InProgressExperiments/2023 - 2024 Continuous system on oysters and mussels/Data combined/Old versions"

new_combined_dir <- "/Users/sel/Library/CloudStorage/OneDrive-USNH/Oyster Biosensor/0_InProgressExperiments/2023 - 2024 Continuous system on oysters and mussels/Data combined/Current version"

# ------------------- Clean original combined data  ------------------
# Read in original combined data
combined_i <- list.files(old_combined_dir, pattern = "txt", full.names = T, include.dirs = F)

# Remove any lines with "Initializing"
messy_dat <- readLines(combined_i)
messy_dat <- messy_dat[!grepl("Initializ", messy_dat)]

# Remove semicolon from each line
messy_dat <- gsub(";", "", messy_dat)
# Remove tab delimiter from each line and replace with comma delimiter
messy_dat <- gsub("\\\t", ",", messy_dat)

# Write clean data
writeLines(messy_dat, con = paste0(new_combined_dir, "/2023-11-20_TO_2024-2-15_clean.csv"))

# ----------------- Combine original data with new raw data -------------- 
# Read in clean combined data
combined_dat_feb <- fread(paste0(new_combined_dir, "/2023-11-20_TO_2024-2-15_clean.csv"))

# Get list of all new files
i <- list.files(raw_dir, pattern = NULL, all.files = T, recursive = F, full.names = T, include.dirs = F)
i <- i[-c(1,2)]

# Find file of latest date
last_file_processed <- which(grepl("2024-02-15", i))
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
combined_dat <- rbind(combined_dat_feb, dat_df)
combined_dat <- unique(combined_dat)

# Save data as new combined version
min_date <- min(date(combined_dat$V1))
max_date <- max(date(combined_dat$V1))

# Add headers
colnames(combined_dat) <- c("timestamp", "seconds_since_start", "ID1","ID2","ID3","ID4","ID5","ID6","ID7","ID8","ID9","ID10","ID11","ID12","ID13","ID14","ID15")

# Check timestamps (are there any midnight times that are dropped?)
which(!grepl("....-..-.. ..:..:..", combined_dat$timestamp))

combined_dat$timestamp <- format(combined_dat$timestamp, format = "%Y-%m-%d %H:%M:%S")

# Check timestamps again (are there any midnight times that are dropped?)
which(!grepl("....-..-.. ..:..:..", combined_dat$timestamp))

# Write data
write.table(combined_dat, file = file.path(new_combined_dir, paste0("JEL-continuous-oyster_", min_date, "_TO_", max_date, "_twelvesec.csv")),
            append = F, sep = ",", na = "NA", dec = ".", row.names = F, col.names = T)

# --------------------- Round to minute -----------------------
# Read in data
combined_dat <- fread(file.path(new_combined_dir, paste0("JEL-continuous-oyster_", min_date, "_TO_", max_date, "_twelvesec.csv")))

# Pivot longer
combined_dat_longer <- combined_dat %>%
  select(-seconds_since_start) %>%
  pivot_longer(cols = contains("ID"), names_to = "oyster_id", values_to = "volts")

# Round timestamps to average oyster gape data
combined_dat_longer <- combined_dat_longer %>%
  # Create new column for minute flooring
  mutate(minute_floor = floor_date(timestamp, "minute"),
         fifteenmin_floor = floor_date(timestamp, "15 minute"))

mean_rm <- function(x){mean(x, na.rm = T)}

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
filename <- file.path(new_combined_dir, paste0("JEL-continuous-oyster_", min_date, "_TO_", max_date, "_1min.csv"))

combined_dat_1min %>%
  pivot_wider(id_cols = minute_floor, names_from = "oyster_id", values_from = "mean_voltage") %>%
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
filename <- file.path(new_combined_dir, paste0("JEL-continuous-oyster_", min_date, "_TO_", max_date, "_15min.csv"))

combined_dat_15min %>%
  pivot_wider(id_cols = fifteenmin_floor, names_from = "oyster_id", values_from = "mean_voltage") %>%
  write_csv(file = filename)




