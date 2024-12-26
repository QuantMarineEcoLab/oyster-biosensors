# Created by Selina Cheng
# Modeling 1 minute timescale "open" or "closed" binary data

# Steps:
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

source_dir <- "~/Library/CloudStorage/OneDrive-USNH/Oyster Biosensor/0_InProgressExperiments/2023 - 2024 Continuous system on oysters and mussels/2c_data for analysis"

oyster_dat_1min <- fread(list.files(source_dir, pattern = "1min", full.names=T))

# ----- Binomial GLM for 1 minute data to model probability of open or closed ----
# oyster_dat_1min
oyster_dat_1min$status_numeric <- ifelse(oyster_dat_1min$new_status == "open", 1, 0)

# Check for NAs in predictors
which(is.na(oyster_dat_1min$do_mgl))
which(is.na(oyster_dat_1min$sal_ppt))
which(is.na(oyster_dat_1min$ph))
which(is.na(oyster_dat_1min$temp_c))

# Are predictors correlated with one another?
corrplot <- oyster_dat_1min %>%
  dplyr::select(do_mgl:temp_c, rows_above)

# Create correlation plot
ggcorr(corrplot, palette = "RdBu", label = TRUE)
ggpairs(corrplot, lower=list(continuous='smooth'),  axisLabels = 'show')

# DO and salinity are highly negatively correlated
# Salinity is correlated with temperature
# DO is perfectly negatively correlated with temperature
# Rows above is highly correlated with temp_c
# Check VIF of pairs

r2 <- round(summary(lm(temp_c ~ sal_ppt, data = oyster_dat_1min))$r.squared, 1)
vif <- round(1 / (1 - r2), 1)
ggplot(oyster_dat_1min, aes(x = sal_ppt, y = temp_c)) + 
  geom_point() + 
  theme_classic() +
  ggtitle(paste("VIF = ", vif, ". R2 = ", 100*r2, "%", sep = ""))

# VIF of salinity and temperature = 2.5, may be ok to leave in model. Exclude DO

# -------- Compare different temp variables to original temp data ------
# Lag 1 day
# Subset data for no NAs in predictors
lag1_dat <- oyster_dat_1min[!is.na(oyster_dat_1min$lag_1day),]

mod1 <- glm(formula = status_numeric ~ temp_c, data = lag1_dat, 
                family = "binomial")

mod2 <- glm(formula = status_numeric ~ lag_1day, data = lag1_dat, 
            family = "binomial")
AIC(mod1)
AIC(mod2)
# temp_c still performs better (AIC lower)


# Lag 2 days
lag2_dat <- oyster_dat_1min[!is.na(oyster_dat_1min$lag_2day),]

mod1 <- glm(formula = status_numeric ~ temp_c, data = lag2_dat, 
            family = "binomial")

mod2 <- glm(formula = status_numeric ~ lag_2day, data = lag2_dat, 
            family = "binomial")
AIC(mod1)
AIC(mod2)
# temp_c still performs better (AIC lower)

# Lag 3 days
lag3_dat <- oyster_dat_1min[!is.na(oyster_dat_1min$lag_3day),]

mod1 <- glm(formula = status_numeric ~ temp_c, data = lag3_dat, 
            family = "binomial")

mod2 <- glm(formula = status_numeric ~ lag_3day, data = lag3_dat, 
            family = "binomial")
AIC(mod1)
AIC(mod2)
# temp_c still performs better (AIC lower)

# Lag 4 days
lag4_dat <- oyster_dat_1min[!is.na(oyster_dat_1min$lag_4day),]

mod1 <- glm(formula = status_numeric ~ temp_c, data = lag4_dat, 
            family = "binomial")

mod2 <- glm(formula = status_numeric ~ lag_4day, data = lag4_dat, 
            family = "binomial")
AIC(mod1)
AIC(mod2)
# temp_c still performs better (AIC lower)

# Lag 5 days
lag5_dat <- oyster_dat_1min[!is.na(oyster_dat_1min$lag_5day),]

mod1 <- glm(formula = status_numeric ~ temp_c, data = lag5_dat, 
            family = "binomial")

mod2 <- glm(formula = status_numeric ~ lag_5day, data = lag5_dat, 
            family = "binomial")
AIC(mod1)
AIC(mod2)
# temp_c still performs better (AIC lower)

# Lag 6 days
lag6_dat <- oyster_dat_1min[!is.na(oyster_dat_1min$lag_6day),]

mod1 <- glm(formula = status_numeric ~ temp_c, data = lag6_dat, 
            family = "binomial")

mod2 <- glm(formula = status_numeric ~ lag_6day, data = lag6_dat, 
            family = "binomial")
AIC(mod1)
AIC(mod2)
# temp_c still performs better (AIC lower)

# Lag 7 days
lag7_dat <- oyster_dat_1min[!is.na(oyster_dat_1min$lag_7day),]

mod1 <- glm(formula = status_numeric ~ temp_c, data = lag7_dat, 
            family = "binomial")

mod2 <- glm(formula = status_numeric ~ lag_7day, data = lag7_dat, 
            family = "binomial")
AIC(mod1)
AIC(mod2)
# temp_c still performs better (AIC lower)

# number of continuous minutes above threshold?
oyster_dat_1min <- oyster_dat_1min %>%
  mutate(rows_above = ifelse(is.na(rows_above), 0, rows_above))

mod1 <- glm(formula = status_numeric ~ temp_c, data = oyster_dat_1min, 
            family = "binomial")

mod2 <- glm(formula = status_numeric ~ rows_above, data = oyster_dat_1min, 
            family = "binomial")
AIC(mod1)
AIC(mod2)
# temp_c still performs better (AIC lower)

# What if we compare temp vs DO?
mod1 <- glm(formula = status_numeric ~ temp_c, data = oyster_dat_1min, 
            family = "binomial")

mod2 <- glm(formula = status_numeric ~ do_mgl, data = oyster_dat_1min, 
            family = "binomial")
AIC(mod1)
AIC(mod2)
# temp_c still performs better (AIC lower)

# What if we compare temp vs minute?
mod1 <- glm(formula = status_numeric ~ temp_c, data = oyster_dat_1min, 
            family = "binomial")

mod2 <- glm(formula = status_numeric ~ minute_floor_est, data = oyster_dat_1min, 
            family = "binomial")
AIC(mod1)
AIC(mod2)
# temp_c still performs better (AIC lower)

# ---------- Model selection -----------------
# All variables are significant..
mod1 <- glm(formula = status_numeric ~ temp_c + sal_ppt + ph, data = oyster_dat_1min, 
            family = "binomial")

summary(mod1)
AIC(mod1)
# Calculate McFadden's pseudo R^2
1-(mod1$deviance/mod1$null.deviance)

# What happens if you drop ph?
mod2 <- glm(formula = status_numeric ~ temp_c + sal_ppt, data = oyster_dat_1min, 
            family = "binomial")

summary(mod2)
AIC(mod2)
1-(mod2$deviance/mod2$null.deviance)

# What happens if you drop sal_ppt?
mod3 <- glm(formula = status_numeric ~ temp_c + ph, data = oyster_dat_1min, 
            family = "binomial")

summary(mod3)
AIC(mod3)
1-(mod3$deviance/mod3$null.deviance)

# What happens if you model with temperature only?
mod4 <- glm(formula = status_numeric ~ temp_c, data = oyster_dat_1min, 
            family = "binomial")

summary(mod4)
AIC(mod4)
1-(mod4$deviance/mod4$null.deviance)

# ----- AIC of full model is the lowest, pseudo R^2 is also the highest -----
summary(mod1)
check_collinearity(mod1)

# Run some DHARMa diagnostic plots?
simulationOutput <- simulateResiduals(mod1, n=1000)
plot(simulationOutput)  
# testOutliers(simulationOutput, type = c("bootstrap"), nBoot = 100, plot = TRUE,
#              plotBoostrap = FALSE)
testDispersion(simulationOutput)
testZeroInflation(simulationOutput)

# Create figures
ggplot(data = oyster_dat_1min, aes(x = mean_temp_c, y = status_numeric))+
  geom_point()+
  stat_smooth(method = "glm", method.args = list(family=binomial), se = TRUE)





