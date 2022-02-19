
#' Crop Yield 
#' 
#' This function determines the annual yield of a crop given fertilizer application
#' @param fertilizer annual fertilizer
#' @param TP mean precipitation (cm), default = 20 
#' @return crop yield
#' 
# function definition 
crop_yield = function(fertilizer, TP=20) {
  result = 1.8 * (fertilizer^2) - 0.5 * fertilizer + 0.3 * TP
  return(result)
}


