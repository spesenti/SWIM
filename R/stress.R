#'  this function combines the subfunctions:
#' stress_VaR, stress_VaR_ES, stress_prob

#' @param type The type of stress. A choice of "VaR", "VaR ES", "prob", "user", "moments"
#' @param ... Arguments to be passed to subsequent funcitons
#' @return A SWIM object
stress <- function(type = c("VaR", "VaR ES", "prob", "user"), ...){

  if(type == "VaR") SWIM <- stress_VaR(...)
  if(type == "VaR ES") SWIM <- stress_VaR_ES(...)
  if(type == "prob") SWIM <- stress_prob(...)
  if(type == "user") SWIM <- stress_user(...)
  if(type == "moments") SWIM <- stress_moments(...)
  return(SWIM)
}

