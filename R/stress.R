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
 #' @param x       A vector, matrix or data frame
 #'     containing realisations of random variables. Columns of \code{x}
 #'     correspond to random variables; OR\cr
 #'     A \code{SWIM} object, where \code{x} corresponds to the
 #'     underlying data of the \code{SWIM} object.
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
                     "mean sd", "moment", "prob", "user"), x, ...){
   if (is.SWIMw(x)) stop("Function cannot be a SWIMw object")
   if (type == "VaR") SWIM <- stress_VaR(x, ...)
   if (type == "VaR ES") SWIM <- stress_VaR_ES(x, ...)
   if (type == "mean") SWIM <- stress_mean(x, ...)
   if (type == "mean sd") SWIM <- stress_mean_sd(x, ...)
   if (type == "moment") SWIM <- stress_moment(x, ...)
   if (type == "prob") SWIM <- stress_prob(x, ...)
   if (type == "user") SWIM <- stress_user(x, ...)
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
#' @param x       A vector, matrix or data frame
#'     containing realisations of random variables. Columns of \code{x}
#'     correspond to random variables; OR\cr
#'     A \code{SWIMw} object, where \code{x} corresponds to the
#'     underlying data of the \code{SWIMw} object.
#' @param ...     Arguments to be passed on, depending on \code{type}.
#'                
#' @return An object of class \code{SWIMw}, see \code{\link{SWIM}} 
#'     for details.
#' 
#' @examples 
#' \dontrun{
#' set.seed(0)
#' x <- as.data.frame(cbind(
#'   "normal" = rnorm(1000), 
#'   "gamma" = rgamma(1000, shape = 2)))
#' res <- stress_wass(type = "RM", x = x, 
#'   alpha = 0.9, q_ratio = 1.05)
#' summary(res)   
#' }
#' 
#' @author Zhuomin Mao 
#' @family stress functions
#' @inherit SWIM references 
#' 
#' @export
#' 
  stress_wass <- function(type = c("RM", "mean sd", "RM mean sd", "HARA RM"), x, ...){
     if (is.SWIM(x)) stop("Function cannot be a SWIM object")
     if (type == "RM") SWIMw <- stress_RM_w(x, ...)
     if (type == "mean sd") SWIMw <- stress_mean_sd_w(x, ...)
     if (type == "RM mean sd") SWIMw <- stress_RM_mean_sd_w(x, ...)
     if (type == "HARA RM") SWIMw <- stress_HARA_RM_w(x, ...)
     if (type == "mean") SWIMw <- stress_mean_w(x, ...)
     return(SWIMw)
  }
