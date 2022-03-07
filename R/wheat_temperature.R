#' wheat_temperature
#' 
#' Compute level of risk associated with extreme temperature events for wheat crop 
#' @param daily_temp (C) 
#' @param age (months) 
#' @param risk_threshold_med (default = 75)
#' @param risk_threshold_high (default = 82)
#' @param age_threshold (years) (default = 8)
#' @return risk (high, med, low), n_extremes

wheat_temperature = function(daily_temp, age, risk_threshold_med = 75, 
                            risk_threshold_high = 82, age_threshold = 8) {
  
  daily_temp = as.data.frame(daily_temp)
  
  risk = 1.8 * daily_temp + 0.5 * (age^2)  
  
  # only high or med if wheat age is greater than 8 months 
  if (any(age >= age_threshold)) {
    risk = case_when(risk < risk_threshold_med ~ "low",
                     risk >= risk_threshold_med &
                       risk < risk_threshold_high ~ "medium", 
                     risk >= risk_threshold_high ~ "high")  
  } else
    risk = "low" 
  
  # compute the number of extreme events 
  #n_extremes = count(daily_temp > 28) 
  n_extremes <- daily_temp %>% 
    filter(tmax >= 28) %>% 
    nrow()
  
  return(list(risk = risk, n_extremes = n_extremes))
}
  

