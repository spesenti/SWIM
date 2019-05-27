#' User Defined Stress
#'
#' Returns a \code{SWIM} object with scenario weights defined by the user.
#'    
#' @inheritParams stress_VaR
#' @param new_weights     A vector, matrix or data frame containing scenario
#'     weights. Columns of \code{new_weights} correspond to different
#'     stresses. \cr
#'     \code{new_weights} will be normalised to 1.
#' @param new_weightsfun  A list of functions, that applied to the \code{k}th column
#'     of \code{x} generate the vectors of the scenario weights. Each function 
#'     correspond to a stress. \cr
#'     \code{new_weights} will be normalised to 1.
#'  
#' @return A \code{SWIM} object containing:
#'     \itemize{
#'       \item \code{x}, the data;
#'       \item \code{new_weights}, a data frame containing scenario
#'       weights; OR \cr
#'       a list of functions, that applied to the \code{k}th component 
#'       of \code{x} generate the vectors of scenario weights;
#'       \item \code{specs}, the specification of what has been
#'       stressed.
#'       \code{specs} is a data.frame consisting of \code{type},
#'       and \code{k}. Each row corresponds to a different stress.
#'     }
#'     See \code{\link{SWIM}} for details.
#'     
#' @examples      
#' set.seed(0)
#' x <- as.data.frame(cbind(
#'   "normal" = rnorm(1000), 
#'   "gamma" = rgamma(1000, shape = 2)))
#' res1 <- stress(type = "user", x = x, new_weightsfun = function(x)x^2)
#'     
#' @family stress functions 
#' @inherit SWIM references 
#' @export 

stress_user <- function(x, new_weights = NULL, new_weightsfun = NULL, k = 1){
  if (is.SWIM(x)) x_data <- get.data(x) else x_data <- as.matrix(x)
  if (anyNA(x_data)) warning("x contains NA")
  if (is.null(colnames(x_data))) colnames(x_data) <- paste("X", 1:ncol(x_data), sep = "")
  
  if (!is.null(new_weights)) {
    nweights <- as.matrix(new_weights)
    nweights <- t(t(nweights) / colMeans(nweights))
    max_length <- ncol(nweights)
    colnames(nweights) <- paste("stress", 1:max_length)
    if (any(nweights < 0)) stop("Invalid new_weights argument")
  } else if (!is.null(new_weightsfun)) {
   if (!is.list(new_weightsfun)) new_weightsfun <- list(new_weightsfun) 
   nweights_values <- sapply(new_weightsfun, function(s) s(x_data[, k]))
   max_length <- length(new_weightsfun)
   if (any(nweights_values < 0)) stop("Invalid new_weights argument")
   nweights <- list()
   for(i in 1:length(new_weightsfun)){
       nweights[[i]] <- function(x) new_weightsfun[[i]](x) / mean(new_weightsfun[[i]](x))
       names(nweights)[i] <- paste("stress", i)
     }
  }

  specs <- data.frame("type" = rep("user", length.out = max_length), "k" = rep(k, length.out = max_length), stringsAsFactors = FALSE)
  rownames(specs) <- paste("stress", 1:max_length)
  my_list <- SWIM("x" = x_data, "new_weights" = nweights, "specs" = specs)
  if (is.SWIM(x)) my_list <- merge(x, my_list)
  return(my_list)
  }