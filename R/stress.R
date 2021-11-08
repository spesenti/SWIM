 #' Stressing Random Variables 
 #' 
 #' Provides weights on simulated scenarios from a baseline stochastic
 #'     model, such that stressed random variables fulfil given 
 #'     probabilistic constraints (e.g. specified values for risk 
 #'     measures), under the new scenario weights. Scenario weights are 
 #'     selected by constrained minimisation of the relative entropy to the
 #'     baseline model. 
 #'    
 #' @param type    Type of stress, one of \code{"VaR", 
 #'     "VaR ES", "mean", "mean sd", "moment", "prob", "user"}.
 #' @param ...     Arguments to be passed on, depending on \code{type}.
 #'                
 #' @return An object of class \code{SWIM}, see \code{\link{SWIM}} 
 #'     for details.
 #' 
 #' @examples 
 #' set.seed(0)
 #' x <- as.data.frame(cbind(
 #'   "normal" = rnorm(1000), 
 #'   "gamma" = rgamma(1000, shape = 2)))
 #' res <- stress(type = "VaR", x = x, 
 #'   alpha = 0.9, q_ratio = 1.05)
 #' summary(res)   
 #' 
 #' @author Silvana M. Pesenti 
 #' @family stress functions
 #' @inherit SWIM references 
 #' 
 #' @export
 #' 
  stress <- function(type = c("VaR", "VaR ES", "mean", 
                     "mean sd", "moment", "prob", "user"), ...){
   if (type == "VaR") SWIM <- stress_VaR(...)
   if (type == "VaR ES") SWIM <- stress_VaR_ES(...)
   if (type == "mean") SWIM <- stress_mean(...)
   if (type == "mean sd") SWIM <- stress_mean_sd(...)
   if (type == "moment") SWIM <- stress_moment(...)
   if (type == "prob") SWIM <- stress_prob(...)
   if (type == "user") SWIM <- stress_user(...)
   return(SWIM)
  }
  
#' Stressing Random Variables Using Wasserstein Distance
#' 
#' Provides weights on simulated scenarios from a baseline stochastic
#'     model, such that stressed random variables fulfill given 
#'     probabilistic constraints (e.g. specified values for risk 
#'     measures), under the new scenario weights. Scenario weights are 
#'     selected by constrained minimisation of the Wasserstein Distance to the
#'     baseline model. 
#'    
#' @param type    Type of stress, one of \code{"RM", 
#'     "mean sd", "RM mean sd", "HARA RM"}.
#' @param ...     Arguments to be passed on, depending on \code{type}.
#'                
#' @return An object of class \code{SWIMw}, see \code{\link{SWIM}} 
#'     for details.
#' 
#' @examples 
#' set.seed(0)
#' x <- as.data.frame(cbind(
#'   "normal" = rnorm(1000), 
#'   "gamma" = rgamma(1000, shape = 2)))
#' res <- stress_wass(type = "RM", x = x, 
#'   alpha = 0.9, q_ratio = 1.05)
#' summary(res)   
#' 
#' @author Zhuomin Mao 
#' @family stress functions
#' @inherit SWIM references 
#' 
#' @export
#' 
  stress_wass <- function(type = c("RM", "mean sd", "RM mean sd", "HARA RM"), ...){
     if (type == "RM") SWIMw <- stress_RM_w(...)
     if (type == "mean sd") SWIMw <- stress_mean_std_w(...)
     if (type == "RM mean sd") SWIMw <- stress_RM_mean_std_w(...)
     if (type == "HARA RM") SWIMw <- stress_HARA_RM_w(...)
     if (type == "mean") SWIMw <- stress_mean_w(...)
     return(SWIMw)
  }
