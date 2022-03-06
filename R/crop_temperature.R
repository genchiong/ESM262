#' crop_temperature
#' 
#' Compute level of risk associated with extreme temperature events for wheat crop 
#' @param daily_temp (C) 
#' @param age (months) 
#' @param temp_threshold_med (C) (default = 26)
#' @param temp_threshold_high (C) (default = 28)
#' @param age_threshold (years) (default = 8)
#' @return risk (high, med, low), n_extremes

crop_temperature = function(daily_temp, age, temp_threshold_med = 26, 
                            temp_threshold_high = 28, age_threshold = 8) {
  
  daily_temp = as.data.frame(daily_temp)
  
  # error checking 
  
  
  # only high or med if wheat age is greater than 8 months 
  if (any(age >= 8)) {
    risk = case_when(daily_temp < temp_threshold_med ~ "low",
                     daily_temp >= temp_threshold_med &
                       daily_temp < temp_threshold_high ~ "medium", 
                     daily_temp >= temp_threshold_high ~ "high")
  } else
    risk = "low" 
  
  # compute the number of extreme events 
  #n_extremes = count(daily_temp > 28) 
  n_extremes <- daily_temp %>% 
    filter(tmax >= 28) %>% 
    nrow()
  
  return(list(risk = risk, n_extremes = n_extremes))
}


