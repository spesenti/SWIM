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
  
# K-L
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
  
# Wasserstein
  stress.wass <- function(type = c("ES"), ...){
     if (type == "ES") SWIMw <- stress_ES_w(...)
     return(SWIMw)
  }