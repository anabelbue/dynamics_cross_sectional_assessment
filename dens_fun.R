dens_fun <- function(frequencies, adjust = 3, n = 100, repetitions = 50, plot = FALSE) {
  # Convert the frequency vector to a data vector
  frequencies[is.na(frequencies)] <- 0  # exchange NAs with 0s
  data_i_j <- rep(1:10, frequencies) # 'recreate' the dataset
  
  if (sd(data_i_j) < 0.01 || (max(data_i_j) - min(data_i_j)) == 1) {
    # If sd is too small or range is 1, return the single value as the mode location
    return(list(modality = 1, mode_locations = unique(data_i_j)))
  }
  
  # Calculate the proportion of the mode (Q)
  prop_modecat <- max(table(data_i_j)) / length(data_i_j)
  
  # Set noise level to the proportion of the mode for Likert scale data
  noise <- prop_modecat
  
  # Initialize vectors to store the modalities and mode locations from each repetition
  v_M <- numeric(repetitions)
  mode_locations_all <- list()
  
  for (r in 1:repetitions) {
    # Add noise
    Xn <- data_i_j + rnorm(length(data_i_j), 0, noise)
    
    # Density Estimation
    den <- density(Xn, bw = "SJ", adjust = adjust, n = n)
    
    # Compute number of reversals to determine modality
    ni <- length(den$y)
    diff <- den$y[-ni] - den$y[-1]
    sign_reversals <- sign(diff[-(ni-1)]) != sign(diff[-1])
    Nrev <- sum(sign_reversals)
    
    # Store the estimated modality for this repetition
    v_M[r] <- ceiling(Nrev / 2)
    
    # Identify mode locations (peaks in the density) and round them
    mode_indices <- which(diff(sign(diff(den$y))) == -2) + 1
    mode_locations_all[[r]] <- round(den$x[mode_indices])
  }
  
  # Determine the most common modality across all repetitions
  tb <- table(v_M)
  m_BC <- as.numeric(names(tb)[which.max(tb)])  # if equal, take smaller one
  
  # Aggregate mode locations for the most common modality
  final_mode_locations <- numeric(m_BC)
  for (i in 1:m_BC) {
    # Collect all locations for this mode number across selected repetitions
    locations <- unlist(lapply(mode_locations_all[v_M == m_BC], `[[`, i))
    
    # Determine the most frequent location for this mode
    if (length(locations) > 0) {
      final_mode_locations[i] <- as.numeric(names(which.max(table(locations))))
    } else {
      final_mode_locations[i] <- NA
    }
  }
  
  # Optionally plot the density and histogram
  if (plot) {
    hist(data_i_j, breaks = seq(0.5, 10.5, 1), freq = FALSE, main = paste("Modality:", m_BC, "Mode Locations:", paste(final_mode_locations, collapse = ", ")), xlab = "Likert Scale", ylab = "Density", col = "lightgray", xlim = c(1, 10), xaxt = 'n')
    axis(1, at = 1:10, labels = TRUE)
    lines(den, col = "blue", lwd = 2)
  }
  
  return(list(modality = m_BC, mode_locations = paste(final_mode_locations, collapse = ", ")))
}




