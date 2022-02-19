
#' Automobile Fuel Efficiency 
#' 
#' This function determines the power required to keep a car moving at a given speed
#' @param crolling rolling resistive coefficients, default = 0.015
#' @param m vehicle mass (kg)
#' @param g acceleration due to gravity (m/s) default = 9.8 
#' @param V vehicle speed assuming no headwind (m/s or mps)
#' @param A surface area of car (m2) 
#' @param p_air density of air (kg/m3) default = 1.2 
#' @param c_drag aerodynamic resistive coefficients, default = 0.3 
#' @return power (W)
#' 
# function definition 
auto_power_gen = function(crolling=0.015, m, g=9.8, V, A, p_air=1.2, c_drag=0.3) {
  result = crolling * m * g * V + (0.5 * A * p_air * c_drag * (V^3))
  return(result)
}









