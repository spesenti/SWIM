#' Stressing random variables 
#' 
#' Provides the scenario weights such that a random variable
#'    under the new scenraio weights fulfils probabilistic constraints and
#'    has minimal Kullback-Leibler divergence to the baseline random
#'    variable.
#'    
#' stress_VaR, stress_VaR_ES, stress_prob

#' @param type    The type of stress. A choice of "VaR", "VaR ES", "prob", "user", "moments"
#' @param ...     Arguments to be passed depending on \code{type}.
#' @return        A SWIM object.
#' 
#' @export
#' 
#' 
#' @importFrom    Rdpack reprompt
#' 
#' 
#' 
stress <- function(type = c("VaR", "VaR ES", "prob", "user"), ...){

  if(type == "VaR") SWIM <- stress_VaR(...)
  if(type == "VaR ES") SWIM <- stress_VaR_ES(...)
  if(type == "prob") SWIM <- stress_prob(...)
  if(type == "user") SWIM <- stress_user(...)
  if(type == "moments") SWIM <- stress_moments(...)
  return(SWIM)
}

