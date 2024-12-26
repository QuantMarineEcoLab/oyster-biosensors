# Created by Selina Cheng
# Scrap code from me trying to create smoothing functions for JEL data

# Actually, let's start with a rolling mean.........

# mean_trim <- function(x){ mean(x, na.rm = TRUE, trim = 0.35) } ## Create mean, trimmed to the middle 30% of values
# dt[,m_lag_filled:= mean_trim(lag_filled), by = key2] # #mean of zone by hour for each day
# dt[,m_lag_filled_upper:= m_lag_filled + 3 ,] # median boundary
# dt[,m_lag_filled_lower:= m_lag_filled - 3 ,]
# dt$lag_filled2 <- ifelse(dt$lag_filled2 > dt$m_lag_filled_upper, NA, dt$lag_filled2)#remove more than 5 C away from mean
# dt$lag_filled2 <- ifelse(dt$lag_filled2 < dt$m_lag_filled_lower, NA, dt$lag_filled2)

start <- Sys.time()
# Moving window sd for 6 hrs
oyster_dat_1min[, value_sd := frollapply(mean_value_clean, 360, sd_rm, fill = NA, align = c("center")), 
                by = "unique_id"] 
end <- Sys.time()
end-start

# start <- Sys.time()
# # Moving window sd for 6 hrs
# oyster_dat_1min[, value_sd := frollapply(mean_value_clean, 360, sd_rm, fill = NA, align = c("center")), by = "unique_id"] 
# end <- Sys.time()
# end-start

# Find 95% quantile of SDs over the 12 hrs
# Remove data if SD for that row is greater than 95% quantile of the SD over the given window
# start <- Sys.time()
# oyster_dat_1min[, quant_sd := frollapply(x=value_sd, n=720, FUN=quantile95, fill = NA, align = c("center")), 
#                 by = "unique_id"]
# end <- Sys.time()
# end-start

# Remove data if SD for that row is greater than 95% quantile of the SD over entire dataset for that oyster
oyster_dat_1min[,mean_value_clean := ifelse(value_sd > quantile(value_sd, 0.975, na.rm = T), NA, 
                                            mean_value_clean), by = "unique_id"]

test_signal <- drop_na(test_signal)

setDT(test_signal)
start <- Sys.time()
# Moving window sd for 6 hrs
test_signal[, value_sd := frollapply(mean_value_clean, 60, sd_rm, fill = NA, align = c("center"))]
end <- Sys.time()
end-start

test_signal$value_sd <- ifelse(is.na(test_signal$value_sd), 1, test_signal$value_sd)

signal <- test_signal$mean_value_clean
noise <- test_signal$value_sd[1:1000]
noise_level<- sd(noise)

erfc <- function(x) 2 * pnorm(x * sqrt(2), lower = FALSE)

# Calculate approximate error
approx_error <- function(signal){
  # signal <- signal[!is.na(signal)]
  x_i <- 1:length(signal)
  p <- lm(signal~stats::poly(x_i, 2))
  pred <- predict(p)
  if(length(pred) != length(signal)){
    print(signal)
    print(pred)
  }
  output_df <- data.frame(predictions = pred, 
                          errors = (signal-pred))
  return(output_df)
}

is_noise_possible_ext <- function(error, sigma, confidence){
  ind <- (sigma>0)
  error <- error[ind]
  
  if(length(error) == 0){
    if(any(error > 1e-6)){
      return(F)
    } else{
      return(T)
    }
  }
  if(all(error == 0)){
    return(T)
  } 
  
  sigma <- sigma[ind]
  noise_prob <- erfc(abs(error)/sigma/sqrt(2))
  
  # This one is problematic?
  # print(noise_prob)
  if(any(noise_prob < (1-confidence))){
    return(F)
  }
  
  for(j in 2:(length(error)/2)){
    error1 <- convolve(x=error, y=(rep(1, times = j)/j), type="filter", conj = F)
    sigma1 <- sigma[(j/2):(j/2+length(error1)-1)]/sqrt(j)
    noise_prob <- erfc(abs(error1)/sigma1/sqrt(2))
    
    if(any(noise_prob < (1-confidence))){
      return(F)
    }
  }
  return(T)
}

is_noise_possible <- function(i0, size, noise_level, confidence){
  if(length(signal[i0:(i0+size-1)]) == 154){
    # browser()
  }
  error <- approx_error(signal[i0:(i0+size-1)])$errors
  # browser()
  output <- is_noise_possible_ext(error, noise_level[i0:(i0+size-1)], confidence)
  return(output)
}

detect_segment_size <- function(i0, old_size, noise_level, confidence){
  sz <- old_size
  is_ns_possible <- is_noise_possible(i0, sz, noise_level, confidence)
  if(i0 == 867){
    browser()
  }
  if(is_ns_possible){
    while(is_ns_possible){
      sz <- sz+1
      if((i0+sz-1) > length(signal)){
        # browser()
        return(sz-1)
      }
      is_ns_possible <- is_noise_possible(i0, sz, noise_level, confidence)
    }
    return(sz-1)
  } else{
    while(!is_ns_possible){
      if(sz >= 3){
        sz <- sz-1
      }
      is_ns_possible <- is_noise_possible(i0, sz, noise_level, confidence)
    }
    return(sz)
  }
  if(i0 == 867){
    browser()
  }
}

confsmooth <- function(signal, noise_level, confidence = 0.99, overlap_fraction = 0.5, deg = 2){
  # y = function values
  # noise_level = vector of same size as y, containing SD of noise
  # confidence: errors with probability < 1-confidence are treated as signal and are not smoothed
  # overlap_fraction: how many percent of points of interval to use for overlap
  # deg = degree of polynomial
  # returns smoothed y
  
  n <- length(signal)
  i0 <- 1
  old_size <- deg+1
  result <- vector(mode = "numeric", length = n)
  osz <- 0
  
  while(T){
    new_size <- detect_segment_size(i0, old_size, noise_level, confidence)
    
    # Get error
    # browser()
    pred_df <- approx_error(signal[i0:(i0+new_size-1)])
    pred <- pred_df$predictions
    error <- pred_df$errors
    
    if(i0 == 1){
      # browser()
      result[1:new_size] <- pred
      osz <- as.integer(round(new_size*overlap_fraction))
      if(osz == 0){
        osz <- 1 
      }
    } else{
      corrected_osz <- min(osz, new_size)
      if(corrected_osz >= 3){
        alpha <- seq(from = 0, to = 1, length.out = corrected_osz)
      } else{
        alpha <- rep(1, corrected_osz)/2
      }
      result[i0:(i0+corrected_osz-1)] <- (1-alpha)*result[i0:(i0+corrected_osz-1)] 
      + alpha*pred[1:corrected_osz]
      
      # middle
      result[(i0+corrected_osz-1):(i0+new_size-1)] <- pred[corrected_osz:length(pred)]
      # browser()
      # Overlap with new
      osz <- as.integer(round(new_size*overlap_fraction))
      if(osz == 0){
        osz <- 1
      }
    }
    i0 <- i0 + new_size - osz -1 
    old_size <- new_size
    if((i0+(new_size)-1) >= length(signal)){
      break
    }
  }
  return(result)
}

test_confsmooth <- confsmooth(signal, rep(0.03, length(signal)))

ugh <- data.frame(og_signal = signal, test_signal = test_confsmooth, index = c(1:length(signal)))
ggplot(data =ugh, aes(x = index))+
  geom_point(aes(y = og_signal), color = "blue")+
  geom_point(aes(y = test_signal), color = "red")

sg <- sgolay(p=2.5, n=1111, m=0)
dim(sg)
test_sgolay <- signal::filter(sg, test_signal$mean_value_clean)

start <- Sys.time()
smth.gaussian(x = test_signal$mean_value_clean, window = 60,
              alpha = 4)
end <- Sys.time()
end-start

test_kernelsmooth = ksmooth(test_signal$minute_floor_est, test_signal$mean_value_clean, 
                            bandwidth = 20)$y


output <- cbind(test_signal, test_kernelsmooth)
names(output)[length(output)] <- "test_kernelsmooth"


# plot_ly(test_df, x = ~index, y = ~signal, type = "scatter")
# Trying one type of butterworth filter
# bf <- butter(n = 1, W = 0.4, type = "pass", plane = "z")
# test <- signal::filtfilt(bf$b, bf$a, test_signal$mean_value_clean)
# output <- cbind(output, test)
# names(output)[7] <- "filter0.4"
# butterworth filter is too rigorous?

# Create full time series
# Interpolate missing values
# time_ladder <- seq(from = min(test_signal$minute_floor_est), to = max(test_signal$minute_floor_est),
#                    by = "1 min")
# time_ladder <- as.data.frame(time_ladder)
# full_df <- full_join(test_signal, time_ladder, by = c("minute_floor_est" = "time_ladder"))
# full_df <- full_df %>%
#   arrange(minute_floor_est)
# full_df$mean_value_clean <- na.approx(full_df$mean_value_clean, na.rm = F)

# Trying to play with loess settings....nah
# setDT(test_signal)
# start <- Sys.time()
# # Moving window sd for 6 hrs
# test_signal[, value_sd := frollapply(mean_value_clean, 60, sd_rm, fill = NA, align = c("center"))] 
# end <- Sys.time()
# end-start
# test_signal <- test_signal %>%
#   mutate(value_sd = ifelse(is.na(value_sd) | value_sd == 0, 0.1, value_sd))
# loessMod10 <- loess(mean_value_clean ~ index, weights = (1/(value_sd)), data=test_signal, span=0.001) # 10% smoothing span
# smoothed10 <- predict(loessMod10)

oyster6_dat$index <- c(1:nrow(oyster6_dat))
# Get a loess curve?
start <- Sys.time()
test <- loess(mean_value_clean ~ index, weights = sd_day, data = oyster6_dat, span = 0.1)
end <- Sys.time()
end-start
smoothed10 <- predict(test)
loess <- data.frame(loess = smoothed10, index = c(1:length(smoothed10)))

old_oyster6_dat <- oyster6_dat
oyster6_dat <- left_join(old_oyster6_dat, loess, by = "index")

start <- Sys.time()
oyster6_dat[, middle := frollapply(mean_value_clean, 28800, middle, fill = NA, align = c("center"))]
end <- Sys.time()
end-start


