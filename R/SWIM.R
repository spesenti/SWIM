 #' SWIM: A package for Sensitivity Analysis  
 #'
 #' The \code{SWIM} (Scenario Weights for Importance Measurement) package
 #'    provides scenario weights such that the random variable
 #'    under the new scenraio weights fulfils the constraint on the VaR and
 #'    has minimal Kullback-Leibler divergence to the baseline random
 #'    variable.
 #' 
 #' @details a;sldkfnds
 #' 
 #' @section A \code{SWIM} object:
 #'     A \code{SWIM} object contains:
 #'   \itemize{
 #'     \item \code{x}, the data;
 #'     \item \code{new_weights}, a list of functions, that applied
 #'   to the \code{k}th colum of \code{x} generate the vectors of 
 #'   scenario weights;
 #'     \item \code{specs}, the specification of what has been
 #'   stressed.
 #'   The \code{specs} is a data.frame consisting of \code{type},
 #'   \code{k}, and constraints depending on the \code{type} of stress.
 #'     \itemize{
 #'       \item \code{type = "VaR"}: \code{alpha}, the level of the 
 #'     stressed VaR; \code{q} the stressed VaR at level \code{alpha}.
 #'       \item \code{type = "VaR ES"}: \code{alpha}, the level of the 
 #'     stressed VaR; \code{q} the stressed VaR at level \code{alpha};
 #'     \code{s} the stressed ES at level \code{alpha}.
 #'       \item \code{type = "prob"}: \code{lower}, the left endpoints of
 #'     the intervals; \code{upper}, the right endpoints of the intervals;
 #'     \code{prob}, stressed probabilties corresponding to the intervals
 #'     defined through \code{lower} and \code{upper}.
 #'       \item \code{type = "user"}: \code{constr = user}.  
 #'       \item \code{type = "moment"}:
 #'       \item \code{type = "mean"}:
 #'       \item \code{type = "mean sd"}:
 #'     }
 #'     } 
 #'     
 #' @seealso \code{\link{stress}}, \code{\link{stress_VaR}} for stressing 
 #'     the VaR, \code{\link{stress_VaR_ES}} for stressing the VaR and ES
 #'     jointly, \code{\link{stress_mean}} for stressing means, 
 #'     \code{\link{stress_moment}} for stressing means and standard
 #'     deviations, \code{\link{stress_moment}} for stressing moments, 
 #'     \code{\link{stress_prob}} for stressing intervals and 
 #'     \code{\link{stress_user}} for user defined scenario weights.
 #' 
 #' @docType package
 #' @name SWIM
 NULL
 