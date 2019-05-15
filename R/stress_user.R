#' User Defined Stress
#'
#' Returns a \code{SWIM} object with scenario weights defined by the user.
#'    
#' @inheritParams stress_VaR
#' @param new_weights     A vector, matrix or data frame containing weights.
#'     Colums of \code{new_weights} are interpreted to correspond to random
#'     variables. OR\cr 
#'     A list of function, that applied to the \code{k}th colum of \code{x} 
#'     generate the vecotrs of the new weights. \cr
#'     \code{new_weights} are normalised to 1.
#'  
#' @return A \code{SWIM} object containing:
#'     \itemize{
#'       \item \code{x}, the data;
#'       \item \code{new_weights}, a list of functions, that applied to
#'       the \code{k}th component of \code{x} generate the vectors of
#'       scenario weights;
#'       \item \code{specs}, the specification of what has been
#'       stressed.
#'       The \code{specs} is a data.frame consisting of \code{type},
#'       \code{k} and \code{constr = user}. Each row correponds to a 
#'       differentstress, see \code{\link{SWIM}} for details.
#'     }
#'     
#' @family stress functions 
#' @inherit SWIM references 
#' @export 

stress_user <- function(x, new_weights, k = 1){
  if (is.SWIM(x)) x_data <- get.data(x) else x_data <- as.matrix(x)
  if (anyNA(x_data)) warning("x contains NA")
  if (is.null(colnames(x_data))) colnames(x_data) <- paste("X", 1:ncol(x_data), sep = "")
  
  if (is.data.frame(new_weights) | is.vector(new_weights)) new_weights <- as.matrix(new_weights)
  if (is.function(new_weights)) new_weights <- as.list(new_weights)
  if (is.list(new_weights)) new_weights <- sapply(new_weights, function(f)f(x[, k]))

  if (any(new_weights < 0)) stop("Invalid new_weights argument")
  new_weights <- t(t(new_weights) / colMeans(new_weights))
  max_length <- ncol(new_weights)
  colnames(new_weights) <- paste("stress", 1:max_length)
  
  specs <- data.frame("type" = rep("user", length.out = max_length), "k" = rep(k, length.out = max_length), constr = "user", stringsAsFactors = FALSE)
  rownames(specs) <- paste("stress", 1:max_length)
  my_list <- SWIM("x" = x_data, "new_weights" = new_weights, "specs" = specs)
  if (is.SWIM(x)) my_list <- merge(x, my_list)
  return(my_list)
  }