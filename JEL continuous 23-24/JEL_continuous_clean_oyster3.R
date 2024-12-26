# Created by Selina Cheng
# Last modified 6 Nov 2024
# Clean JEL data on more fine-scale
# Goals: apply smoothing functions to remove noise
rm(list = ls())
gc()

# ---- Functions ----
mean_rm <- function(x){mean(x, na.rm = T)}
sd_rm <- function(x){sd(x, na.rm = T)}
range_rm <- function(x){
  range <- range(x, na.rm = T)
  return(diff(range))
}

mean_trim <- function(x){mean(x, trim = 0.25, na.rm = T)}

# -------- SET UP ----------
# load libraries
library(data.table)
library(tidyverse)
library(lubridate)
library(signal)
# library(plotly)
library(zoo)

# Set source directory
oyster_dir <- "~/Library/CloudStorage/OneDrive-USNH/Oyster Biosensor/0_InProgressExperiments/2023 - 2024 Continuous system on oysters and mussels/2a_processed sensor data"

# --------- LOAD DATA ----------
# Read in rough cleaned oyster data
oyster_dat <- fread(list.files(oyster_dir, pattern = "1min", full.names=T))

# -------------- First round to nearest minute -------------------
# Round timestamps to average oyster gape data
oyster_dat <- oyster_dat %>%
  # Create new column for minute flooring
  mutate(minute_floor_est = floor_date(timestamp_est, "minute"))

# OK..... I give up on the mussel data. Filter for just oysters
# I think mussel and oyster data have to be cleaned differently
# There are only 6 oysters in the dataset.......which is FINE?
oyster_dat <- oyster_dat %>%
  dplyr::filter(species == "oyster")

# Create a 1-min rounded dataset
start <- Sys.time()
oyster_dat_1min <- oyster_dat %>%
  group_by(minute_floor_est, unique_id, species, died) %>%
  summarise(mean_value_clean = mean_rm(value_clean),
            mean_value_raw = mean_rm(value_raw))
end <- Sys.time()
end-start

# --------- Smooth data to remove extreme noise -------------
oyster6_dat <- oyster_dat_1min %>%
  dplyr::filter(unique_id == "ID6-Y6")

setDT(oyster6_dat)

# For every...day, let's say, get the 5th and 95th quantile of the data
fifth_quantile <- function(x){
  y <- quantile(x, 0.05, na.rm = T)
  return(y)
}

ninetyfifth_quantile <- function(x){
  y <- quantile(x, 0.95, na.rm = T)
  return(y)
}

middle <- function(x){
  midpt <- (diff(range(x, na.rm = T))/2)+min(x, na.rm = T)
  return(midpt)
  
  # if(length(midpt) > 1){
  #   warning("2 points returned")
  # } else{
  # }
}

# Moving window for one day, find 5th quantile
start <- Sys.time()
oyster6_dat[, fifth_quant := frollapply(mean_value_clean, 1440, fifth_quantile, fill = NA, align = c("center"))]
end <- Sys.time()
end-start

# Moving window for one day, find 95th quantile
start <- Sys.time()
oyster6_dat[, ninetyfifth_quantile := frollapply(mean_value_clean, 1440, ninetyfifth_quantile, fill = NA, align = c("center"))]
end <- Sys.time()
end-start

# Moving window for one hr, find sd
start <- Sys.time()
oyster6_dat[, sd_day := frollapply(mean_value_clean, 1440, sd_rm, fill = NA, align = c("center"))]
end <- Sys.time()
end-start

# Find the rough midpoints of each bin of data
# Create different bins of data
bins <- seq(from = 1, to = nrow(oyster6_dat), length.out = 21)
bins <- round(bins)

indices <- vector("numeric", length = length(bins)-1)
midpoints <- vector("numeric", length = length(bins)-1)

for(n in 2:length(bins)){
  midpoint <- middle(oyster6_dat$mean_value_clean[(bins[n-1]):bins[n]])
  midpoints[n-1] <- midpoint
  indices[n-1] <- round(((bins[n]-bins[n-1])/2)+bins[n-1])
}

oyster6_dat$index <- c(1:nrow(oyster6_dat))
indices <- c(1, indices)
indices <- c(indices, 393792)
midpoints <- c(530, midpoints)
midpoints <- c(midpoints, 525)

midpoint_dat <- data.frame(index= indices, midpoints=midpoints)
midpoint_dat$minute_floor_est <- oyster6_dat$minute_floor_est[midpoint_dat$index]
midpoint_dat <- midpoint_dat %>%
  dplyr::filter(month(minute_floor_est) != 1)

oyster6_dat <- left_join(oyster6_dat, midpoint_dat[,-3], by = "index")

# Now cubic interpolation between each one
oyster6_dat$midpoints <- na.spline(oyster6_dat$midpoints, na.rm = F)

# For oyster 6, down is open and up is closed.
# Fill in leading and trailing NAs
oyster6_dat$fifth_quant[1:719] <- quantile(oyster6_dat$mean_value_clean[1:719], 0.05, na.rm = T)
oyster6_dat$fifth_quant[393073:393792] <- quantile(oyster6_dat$mean_value_clean[393073:393792], 0.05, na.rm = T)

oyster6_dat$ninetyfifth_quantile[1:719] <- quantile(oyster6_dat$mean_value_clean[1:719], 0.95, na.rm = T)
oyster6_dat$ninetyfifth_quantile[393073:393792] <- quantile(oyster6_dat$mean_value_clean[393073:393792], 0.95, na.rm = T)

oyster6_dat$sd_day[1:719] <- sd_rm(oyster6_dat$mean_value_clean[1:719])
oyster6_dat$sd_day[393073:393792] <- sd_rm(oyster6_dat$mean_value_clean[393073:393792])

# Create "20th percentile" between 95th and 5th quantile
oyster6_dat[, ten_pct := ((ninetyfifth_quantile-fifth_quant)*0.9)+fifth_quant]

# Assess variability over the day
oyster6_dat[, low_var := ifelse(sd_day < quantile(sd_day, 0.4, na.rm = T) | (ninetyfifth_quantile - fifth_quant) < 3, "low", "high")]

# First, by default if mean value is below the twenty pct line, assign open. otherwise, closed
oyster6_dat[, status := ifelse(mean_value_clean < ten_pct, "open", "closed")]

# Then, if variability is low, look at if value is above or below mean trim.
oyster6_dat[, status := ifelse(low_var == "low", ifelse(mean_value_clean > midpoints, "closed", "open"), status)]
oyster6_dat[, status_numeric := ifelse(status == "open", 1, 0)]

# Save data
oyster6_dat %>%
  mutate(minute_floor_est = format(minute_floor_est, format = "%Y-%m-%d %H:%M:%S")) %>%
  write.table(file = file.path(oyster_dir, paste0("JEL-continuous-oyster6.csv")),
              append = F, sep = ",", na = "NA", dec = ".", row.names = F, col.names = T)

# Midpoints plot
# ggplot()+
#   geom_point(data = oyster6_dat, aes(x = minute_floor_est, y = mean_value_clean), 
#              color= "blue")+
#   geom_point(data = oyster6_dat, aes(x = minute_floor_est, y= midpoints),
#              color = "red")+
#   geom_point(data = midpoint_dat, aes(x = minute_floor_est, y = midpoints), size = 3)

# -------- Create figures? ------------------
ggplot(data = oyster6_dat, aes(x = minute_floor_est, y=mean_value_clean, color = status))+
  geom_point()+
  labs(x="Time", y = "Voltage (mV)", color = "Status")+
  scale_x_datetime(date_breaks = "1 month", 
                   limits = as.POSIXct(c('11/15/2023', '05/01/2024'), format="%m/%d/%Y"),
                   date_labels="%b-%Y") +
  scale_y_continuous(limits = c(505, 535))+
  selina_theme(12)

ggplot(data = oyster6_dat, aes(x = minute_floor_est, y=status_numeric))+
  geom_point()+
  labs(x="Time", y = "Open (1)/Closed (0)", color = "Status")+
  scale_x_datetime(date_breaks = "1 month", 
                   limits = as.POSIXct(c('11/15/2023', '05/01/2024'), format="%m/%d/%Y"),
                   date_labels="%b-%Y") +
  # scale_y_continuous(limits = c(520, 535))+
  selina_theme(12)

ggsave("oyster6_winter2.png", width = 18, height = 6, dpi = 300)

