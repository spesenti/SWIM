#' Stressing random variables 
#' 
#' Provides the scenario weights such that a random variable
#'    under the new scenraio weights fulfils probabilistic constraints and
#'    has minimal Kullback-Leibler divergence to the baseline random
#'    variable.
#'    

#' @param type    The type of stress. A choice of "VaR", "VaR ES", "prob", "user", "moments"
#' @param ...     Arguments to be passed depending on \code{type}.
#' @return        A SWIM object.
#' 
#' @note \code{\link{stress}}, \code{\link{stress_VaR}} for stressing 
#'     the VaR, \code{\link{stress_VaR_ES}} for stressing the VaR and ES
#'     jointly, \code{\link{stress_mean}} for stressing means, 
#'     \code{\link{stress_moment}} for stressing means and standard
#'     deviations, \code{\link{stress_moment}} for stressing moments, 
#'     \code{\link{stress_prob}} for stressing intervals and 
#'     \code{\link{stress_user}} for user defined scenario weights.
#' 
#' @author Silvana M. Pesenti 
#' @family stress functions
#' 
#' @export
#' 
#' @importFrom    Rdpack reprompt
#' 
#' 
#' 
  stress <- function(type = c("VaR", "VaR ES", "prob", "user"), ...){
   if (type == "VaR") SWIM <- stress_VaR(...)
   if (type == "VaR ES") SWIM <- stress_VaR_ES(...)
   if (type == "prob") SWIM <- stress_prob(...)
   if (type == "user") SWIM <- stress_user(...)
   if (type == "moments") SWIM <- stress_moments(...)
   return(SWIM)
  }

