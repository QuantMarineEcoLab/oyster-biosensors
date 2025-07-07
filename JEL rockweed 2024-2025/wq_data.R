# Read in WQ data and interpolate to match oyster data
# FYI WQ data ends on 2025-03-27 11:00:00

# Load libraries
library(data.table)
library(tidyverse)
library(lubridate)
library(readxl)

# Read in data
dat <- read_excel("~/Library/CloudStorage/OneDrive-USNH/Oyster Biosensor/0_InProgressExperiments/2024 JEL field/data/Great Bay Adams Pt winter sonde data 2024-2025.xlsx")

# Select columns
dat <- dat %>%
  select(-contains("Chl"), -contains("fDOM"), -contains("BGA"), -contains("Turb"))

# Filter out data
dat <- dat %>%
  filter(F_Depth != "<-3> [SCF]",
         F_pH != "<-3> (CCU)")

# Remove QAQC columns
dat <- dat %>%
  select(-contains("F_"))

# Rename columns
names(dat) <- c("site", "timestamp_est", "depth", "do_pct", "do_mgl", "sal_ppt", "spcond_ms", "ph", "temp_c")


