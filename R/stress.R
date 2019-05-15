 #' Stressing Random Variables 
 #' 
 #' Provides the scenario weights such that a random variable
 #'    under the scenraio weights fulfils probabilistic constraints and
 #'    has minimal Kullback-Leibler divergence to the baseline random
 #'    variable.
 #'    
 #' @param type    Charactor, the type of stress, one of \code{"VaR", 
 #'                "VaR ES", "prob", "user", "moment", "mean", "mean sd"}.
 #' @param ...     Arguments to be passed depending on \code{type}.
 #'                
 #' @return An object of class \code{SWIM}, see \code{\link{SWIM}} 
 #'     for more details.
 #' 
 #' @author Silvana M. Pesenti 
 #' @family stress functions
 #' 
 #' @inherit SWIM references 
 #' 
 #' @export
 #' 
  
  stress <- function(type = c("VaR", "VaR ES", "prob", "user"), ...){
   if (type == "VaR") SWIM <- stress_VaR(...)
   if (type == "VaR ES") SWIM <- stress_VaR_ES(...)
   if (type == "prob") SWIM <- stress_prob(...)
   if (type == "user") SWIM <- stress_user(...)
   if (type == "moment") SWIM <- stress_moment(...)
   return(SWIM)
  }