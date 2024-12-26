# Created by Selina Cheng
# Last modified 19 Nov 2024
# Create some figures for JEL data
rm(list = ls())
gc()

# ---------- SET UP ---------------
library(tidyverse)
library(lubridate)
library(patchwork)

# Set source directory
source_dir <- "~/Library/CloudStorage/OneDrive-USNH/Oyster Biosensor/0_InProgressExperiments/2023 - 2024 Continuous system on oysters and mussels/3_other data"

# -------- THEME -----------------
# Create theme
selina_theme <- function(text_size){
  theme_classic()+
    theme(panel.grid.major.y = element_line(colour = "#ececec"),
          panel.grid.major.x = element_line(color = "#ececec"),
          text = element_text(size = (text_size + 1)),
          axis.text.x = element_text(margin=margin(0.25,0.25,0.25,0.25, "cm"), color = "black", size = text_size, angle = 45, hjust = 1, vjust = 1),
          axis.text.y = element_text(margin=margin(0.25,0.25,0.25,0.25, "cm"), color = "black", size = text_size),
          axis.line.x = element_line(color="black", size = 0.5),
          axis.line.y = element_line(color="black", size = 0.5),
          axis.ticks = element_line(size = 0.5, color = "black"),
          axis.ticks.length = unit(0.25, "cm"),
          axis.title.x = element_text(vjust= -1),
          legend.title = element_text(size = (text_size +1)),
          legend.text = element_text(size = text_size))
}

# -------------- Temperature --------------
# Read in JEL sonde data (outside temperature)
temp_dat2 <- fread(list.files(source_dir, pattern = "sonde data.csv", full.names = T))
temp_dat2$DateTimeStamp <- mdy_hm(temp_dat2$DateTimeStamp)

# Read in JEL interior tank data (inside temp)
dat <- fread(list.files(source_dir, pattern = "temp", full.names=T))

# Make figure
ggplot()+
  geom_point(data = dat, aes(x = timestamp_est, y = temp_c), color = "blue")+
  geom_point(data=temp_dat2, aes(x = DateTimeStamp, y = Temp), color = "red")+
  labs(y = "Temperature (°C)", x = "Timestamp")+
  selina_theme(12)+
  scale_x_datetime(date_breaks = "1 month", date_labels = "%b-%Y")

# Save
ggsave("temp_JEL_inside_outside.png", width = 12, height = 6, dpi = 300)

# ----------- Chlorophyll -----------------
# Read in chlorophyll data
chl_dat <- fread(list.files(source_dir, pattern = "chl", full.names=T))

# Reformat data
chl_dat <- chl_dat %>%
  select(-contains("V"))
names(chl_dat)[5:7] <- c("chl_rep1", "chl_rep2", "chl_rep3")
chl_dat$chl_rep1 <- as.numeric(chl_dat$chl_rep1)

# Create mean chl for each date
chl_dat <- chl_dat %>%
  mutate(mean_chl_a = (chl_rep1+chl_rep2+chl_rep3)/3,
         date = as.POSIXct(paste0(Year, "-", Month_Pull, "-", Day_Pull), format = "%Y-%m-%d"))

# plot
chl_plt <- ggplot(data = chl_dat, aes(x = date, y = mean_chl_a))+
  geom_line()+
  labs(y = expression(paste("Chlorophyll ( ", mu, "g/L)")),
       x = "Time")+
  scale_x_datetime(date_breaks = "1 month", 
                   limits = as.POSIXct(c('11/15/2023', '05/01/2024'), format="%m/%d/%Y"),
                   date_labels = "%b-%Y")+
  selina_theme(12)

# Save
ggsave("chl_JEL.png", width = 10, height = 6, dpi = 300)

# ----------- OYSTER x ENVIRONMENT -------------
# Oyster plot
oyster_plt <- ggplot(data = oyster6_dat, aes(x = minute_floor_est, y=mean_value_clean, color = status))+
  geom_point()+
  labs(x="Time", y = "Voltage (mV)", color = "Status")+
  scale_x_datetime(date_breaks = "1 month", 
                   limits = as.POSIXct(c('11/15/2023', '05/01/2024'), format="%m/%d/%Y"),
                   date_labels="%b-%Y") +
  scale_y_continuous(limits = c(505, 535))+
  selina_theme(12)

ggsave("oyster6_winter2.png", plot=oyster_plt, width = 8, height = 6, dpi = 300)

# Light plot
light_plt <- ggplot(data = dat, aes(x = timestamp_est, y = light_lux))+
  geom_point()+
  labs(y = "Light level (lux)", x = "Timestamp")+
  selina_theme(12)+
  scale_x_datetime(date_breaks = "1 week", 
                   limits = as.POSIXct(c('04/01/2024', '04/15/2024'), format="%m/%d/%Y"),
                   date_labels="%d-%b-%Y")+
  scale_y_continuous(limits = c(0, 10100))

# Temperature plot
temp_plt <- ggplot(data = dat, aes(x = timestamp_est, y = temp_c))+
  geom_point()+
  labs(y = "Temperature (°C)", x = "Timestamp")+
  selina_theme(12)+
  scale_x_datetime(date_breaks = "1 month", 
                   limits = as.POSIXct(c('02/01/2024', '03/01/2024'), format="%m/%d/%Y"),
                   date_labels="%b-%Y")+
  scale_y_continuous(limits = c(0, 15))

oyster_plt/light_plt

ggsave("oyster_light.png", plot=(oyster_plt/light_plt), width = 14, height = 10, dpi = 300)

ggsave("oyster_chl.png", plot=(oyster_plt/chl_plt), width = 14, height = 10, dpi = 300)
