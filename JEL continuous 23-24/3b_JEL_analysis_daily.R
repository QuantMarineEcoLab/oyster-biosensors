# Created by Selina Cheng
# Modeling daily timescale -- number of times opened or closed

# Need to subset out data where there are NAs for predictors
# Stepwise backwards model selection for different models
# Compare models using AIC
# Report pseudo R squared value

# ----- Set up ------
rm(list = ls())
gc()

library(tidyverse)
library(lubridate)
library(GGally)
library(performance)
library(DHARMa)
library(MASS)

source_dir <- "~/Library/CloudStorage/OneDrive-USNH/Oyster Biosensor/0_InProgressExperiments/2023 - 2024 Continuous system on oysters and mussels/2c_data for analysis"

oyster_dat_daily <- fread(list.files(source_dir, pattern = "daily", full.names=T))

# ---- # of times open per day (or hour?) is count data, so use negative binomial glm ---
# Let's model # of times open per day because the per hour numbers are really low

# Check for NAs in predictors
which(is.na(oyster_dat_daily$mean_temp_c))
which(is.na(oyster_dat_daily$mean_do))
which(is.na(oyster_dat_daily$mean_sal))
which(is.na(oyster_dat_daily$mean_ph))

# # Are predictors correlated with one another?
corrplot <- oyster_dat_daily %>%
  dplyr::select(mean_temp_c, time_above_threshold:mean_ph)

# Create correlation plot
ggcorr(corrplot, palette = "RdBu", label = TRUE)
ggpairs(corrplot, lower=list(continuous='smooth'),  axisLabels = 'show')

# Again, DO is perfectly negatively correlated with temp
# DO highly negatively correlated with salinity
# Salinity highly positively correlated with temperature
# Time above threshold somewhat correlated with temperature

# Check VIF of pairs
r2 <- round(summary(lm(time_above_threshold ~ mean_temp_c, data = oyster_dat_daily))$r.squared, 1)
vif <- round(1 / (1 - r2), 1)
ggplot(oyster_dat_daily, aes(x = mean_temp_c, y = time_above_threshold)) + 
  geom_point() + 
  theme_classic() +
  ggtitle(paste("VIF = ", vif, ". R2 = ", 100*r2, "%", sep = ""))

# VIF for model of salinity x temperature is 3.3
# DO is collinear with salinity and temp
# time above threshold is not collinear with temp

# -------- Compare different temp variables to original temp data ------
# Lag 1 day
# Subset data for no NAs in predictors
lag1_dat <- oyster_dat_daily[!is.na(oyster_dat_daily$mean_lag1),]

mod1 <- glm.nb(formula = num_switch_event ~ mean_temp_c, data = lag1_dat)

mod2 <- glm.nb(formula = num_switch_event ~ mean_lag1, data = lag1_dat)

AIC(mod1)
AIC(mod2)
# mean_temp_c still performs better (AIC lower)

# Lag 2 days
lag2_dat <- oyster_dat_daily[!is.na(oyster_dat_daily$mean_lag2),]

mod1 <- glm.nb(formula = num_switch_event ~ mean_temp_c, data = lag2_dat)

mod2 <- glm.nb(formula = num_switch_event ~ mean_lag2, data = lag2_dat)

AIC(mod1)
AIC(mod2)
# mean_temp_c still performs better (AIC lower)

# Lag 3 days
lag3_dat <- oyster_dat_daily[!is.na(oyster_dat_daily$mean_lag3),]

mod1 <- glm.nb(formula = num_switch_event ~ mean_temp_c, data = lag3_dat)

mod2 <- glm.nb(formula = num_switch_event ~ mean_lag3, data = lag3_dat)

AIC(mod1)
AIC(mod2)
# mean_temp_c still performs better (AIC lower)

# Lag 4 days
lag4_dat <- oyster_dat_daily[!is.na(oyster_dat_daily$mean_lag4),]

mod1 <- glm.nb(formula = num_switch_event ~ mean_temp_c, data = lag4_dat)

mod2 <- glm.nb(formula = num_switch_event ~ mean_lag4, data = lag4_dat)

AIC(mod1)
AIC(mod2)
# mean_temp_c still performs better (AIC lower)

# Lag 5 days
lag5_dat <- oyster_dat_daily[!is.na(oyster_dat_daily$mean_lag5),]

mod1 <- glm.nb(formula = num_switch_event ~ mean_temp_c, data = lag5_dat)

mod2 <- glm.nb(formula = num_switch_event ~ mean_lag5, data = lag5_dat)

AIC(mod1)
AIC(mod2)
# mean_temp_c still performs better (AIC lower)

# Lag 6 days
lag6_dat <- oyster_dat_daily[!is.na(oyster_dat_daily$mean_lag6),]

mod1 <- glm.nb(formula = num_switch_event ~ mean_temp_c, data = lag6_dat)

mod2 <- glm.nb(formula = num_switch_event ~ mean_lag6, data = lag6_dat)

AIC(mod1)
AIC(mod2)
# mean_temp_c still performs better (AIC lower)

# Lag 7 days
lag7_dat <- oyster_dat_daily[!is.na(oyster_dat_daily$mean_lag7),]

mod1 <- glm.nb(formula = num_switch_event ~ mean_temp_c, data = lag7_dat)

mod2 <- glm.nb(formula = num_switch_event ~ mean_lag7, data = lag7_dat)

AIC(mod1)
AIC(mod2)
# mean_temp_c still performs better (AIC lower)

# number of continuous minutes above threshold?
mod1 <- glm.nb(formula = num_switch_event ~ mean_temp_c, data = oyster_dat_daily)

mod2 <- glm.nb(formula = num_switch_event ~ time_above_threshold, data = oyster_dat_daily)

AIC(mod1)
AIC(mod2)
# mean_temp_c still performs better (AIC lower)

# What if we compare temp vs date?
mod1 <- glm.nb(formula = num_switch_event ~ mean_temp_c, data = oyster_dat_daily)

mod2 <- glm.nb(formula = num_switch_event ~ date, data = oyster_dat_daily)

AIC(mod1)
AIC(mod2)
# mean_temp_c still performs better (AIC lower)

# What if we compare temp vs do?
mod1 <- glm.nb(formula = num_switch_event ~ mean_temp_c, data = oyster_dat_daily)

mod2 <- glm.nb(formula = num_switch_event ~ mean_do, data = oyster_dat_daily)

AIC(mod1)
AIC(mod2)
# mean_temp_c still performs better (AIC lower)

# --------- Model selection ------ 
# Including all predictors 
mod1 <- glm.nb(formula = num_switch_event ~ mean_temp_c + mean_sal + mean_ph + time_above_threshold,
               data = oyster_dat_daily)

summary(mod1)
AIC(mod1)
# Calculate McFadden's pseudo R^2
1-(mod1$deviance/mod1$null.deviance)

# time above threshold is not significant, drop
mod2 <- glm.nb(formula = num_switch_event ~ mean_temp_c + mean_sal + mean_ph,
               data = oyster_dat_daily)

summary(mod2)
AIC(mod2)
# Calculate McFadden's pseudo R^2
1-(mod2$deviance/mod2$null.deviance)
# AIC of mod2 is lower than mod1

# What happens if we drop salinity?
mod3 <- glm.nb(formula = num_switch_event ~ mean_temp_c + mean_ph,
               data = oyster_dat_daily)

summary(mod3)
AIC(mod3)
# Calculate McFadden's pseudo R^2
1-(mod3$deviance/mod3$null.deviance)
# AIC of mod2 is still lowest

# What happens if we drop ph?
mod4 <- glm.nb(formula = num_switch_event ~ mean_temp_c + mean_sal,
               data = oyster_dat_daily)

summary(mod4)
AIC(mod4)
# Calculate McFadden's pseudo R^2
1-(mod4$deviance/mod4$null.deviance)
# AIC of mod2 is still lowest

# If we model with just temperature?
mod5 <- glm.nb(formula = num_switch_event ~ mean_temp_c,
               data = oyster_dat_daily)

summary(mod5)
AIC(mod5)
# Calculate McFadden's pseudo R^2
1-(mod5$deviance/mod5$null.deviance)
# AIC of mod2 is still lowest

# ----- AIC of mod2 is the lowest -----
summary(mod2)
check_collinearity(mod2)
1-(mod2$deviance/mod2$null.deviance)

# Run some DHARMa diagnostic plots?
simulationOutput <- simulateResiduals(mod2, n=1000)
plot(simulationOutput)  
testDispersion(simulationOutput)
testZeroInflation(simulationOutput)

# Create plots
ggplot(data = oyster_dat_daily, aes(x = mean_temp_c, y = num_switch_event))+
  geom_point()+
  stat_smooth(method = "glm.nb")

