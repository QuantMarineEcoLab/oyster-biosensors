# Write 1 Euro filter (preserve spikes, reduce noise)
# fc = cutoff
# te = sampling period = 1 min
get_smoothing_factor <- function(te, cutoff){
  r <- 2*pi*cutoff*te
  return(r/(r+1))
}

get_exponential_smoothing <- function(alpha, x, x_prev){
  output <- alpha*x + (1-alpha)*x_prev
  return(output)
}

one_euro_filter <- function(te, x, min_cutoff = 1, beta = 0, d_cutoff = 1){
  # Create empty vector
  x_hat_list <- vector(mode = "numeric", length = length(x))
  dx_prev <- 0
  x_prev <- x[1]
  
  for(i in 2:length(x)){
    # Filtered derivative of the signal
    a_d <- get_smoothing_factor(te, d_cutoff)
    dx <- (x[i] - x_prev)/te
    dx_hat <- get_exponential_smoothing(a_d, dx, dx_prev)
    
    # The filtered signal
    cutoff <- min_cutoff + beta * abs(dx_hat)
    a <- get_smoothing_factor(te, cutoff)
    x_hat <- get_exponential_smoothing(a, x[i], x_prev)
    
    # Set x_prev = x_hat
    x_prev <- x_hat
    dx_prev <- dx_hat
    
    x_hat_list[i] <- x_hat
  }
  return(x_hat_list)
}

min_cutoff <- 0.00001
beta <- 0.1
te <- 60

test_filter <- one_euro_filter(te, test_signal$mean_value_clean, min_cutoff, beta)