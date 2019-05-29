 # Defines the class "SWIM"
  SWIM <- function(x = "x", new_weights = "new_weights", 
                       specs = c(type = "type", k = "k", constr = "constr"     )){
   mymodel <- list(
   x = x, # vector, matrix or dataframe
   new_weights = new_weights, # list of functions providing the new_weights
   specs = specs
      # data.frame with the following structure (here for type = "VaR")
      #           type    k   alpha        q
      # stress 1  "VaR"   1   0.9       1.804807
    
      # type = c("VaR", "VaR ES", "prob", "moment", "user"),
      # k = k,
      # further columns according to the constraints of the optimisation
   )   
   ## Name of the class
   attr(mymodel, "class") <- "SWIM"
   return(mymodel)
  }

  
  is.SWIM <- function(object) inherits(object, "SWIM")

 #' Extracting from a Stressed Model
 #'
 #' Extracting the data (realisations of the stochastic model), the 
 #'     scenario weights, the functions generating the scenario weights, 
 #'     or the specifications of the stress from an object of class 
 #'     \code{SWIM}. 
 #' 
 #' @param object    A \code{SWIM} object.
 #' @inheritParams   summary.SWIM
 #'  
 #' @return \code{get.data}: A data.frame containing the realisations of 
 #'         the stochastic model on which the \code{object} is based.    
 #'
 #' @seealso \code{\link{SWIM}}
 #'                  
 #' @author Silvana M. Pesenti 
 #' @describeIn get.data extracting data.
 #' @export

  get.data <- function(object, xCol = "all"){
   if (!is.SWIM(object)) stop("Object not of class SWIM")
   if (xCol == "all") xCol = 1:ncol(object$x) else if (!(xCol %in% 1:ncol(object$x))) stop("invalid 'xCol' argument")
   return(as.matrix(object$x[, xCol]))
  }

 #' @describeIn get.data extracting scenario weights. 
 #'
 #' @return \code{get.weights:} A data.frame containing the scenario 
 #'     weights of the \code{object}. Columns corresponds 
 #'     to different stresses.
 #'         
 #' @examples 
 #' ## continuing example in stress_VaR
 #' set.seed(0)
 #' x <- as.data.frame(cbind(
 #'   "normal" = rnorm(1000), 
 #'   "gamma" = rgamma(1000, shape = 2)))
 #' res1 <- stress(type = "VaR", x = x, 
 #'   alpha = 0.9, q_ratio = 1.05)
 #'   
 #' ## returning the underlying data
 #' all(get.data(res1) == x)
 #' ## the scenario weights
 #' get.weights(res1)
 #' get.weightsfun(res1)
 #' get.specs(res1)
 #'                             
 #' @export

  get.weights <- function(object){
   if (!is.SWIM(object)) stop("Object not of class SWIM")
   specs <- get.specs(object)
   x_data <- get.data(object)
   m <- length(specs$type)
   new_weights <- matrix(0, nrow = nrow(x_data), ncol = m)
   for(i in 1:m){
    if (!is.function(object$new_weights[[i]])) {
     new_weights[, i] <- object$new_weights[, i]
    } else {
     k <- specs$k[i]
     new_weights[, i] <- object$new_weights[[i]](x_data[, k])
    }
   }
   colnames(new_weights) <- paste("stress", 1:m)
   return(new_weights)
  }

 #' @describeIn get.data extracting weight functions.
 #'
 #' @return \code{get.weightsfun}: A list containing functions, which, 
 #'     when applied to a column of the data, generate the 
 #'     scenario weights of the \code{object}. The corresponding stressed 
 #'     columns can be obtained via \code{get.specs}.\cr
 #'     Use \code{\link{get.weights}} if the \code{SWIM} object only contains 
 #'     scenario weights and not a list of functions.
 #'         
 #' @export

  get.weightsfun <- function(object){
   if (!is.SWIM(object)) stop("Object not of class SWIM")
   specs <- get.specs(object)
   if (!is.function(object$new_weights[[1]]))
      stop("New_weights is not a function, use get.weights() instead.") 
   return(object$new_weights)
  }

 #' @describeIn get.data extracting information of the stress.
 #'
 #' @return \code{get.specs}: A data.frame containing the specifications 
 #'         of the \code{object}. Rows corresponds to different stresses.
 #'         See \code{\link{SWIM}} for details.
 #' @export

  get.specs <- function(object){
   if (is.SWIM(object)) return(object$specs) else stop("Object not of class SWIM")
  }

 #' Merging Two Stressed Models
 #'
 #' This function is a \code{method} for an object of class 
 #'     \code{SWIM}.
 #'     
 #' @details Merges two objects of class \code{SWIM}, that 
 #'     are based on the same data. \cr 
 #'     Stresses with \code{type = c("user", "moment")} are ignored. 
 #' 
 #' @param x,y    Objects of class \code{SWIM}.
 #' @param ...    Additional arguments will be ignored.
 #'  
 #' @return An object of class \code{SWIM} containing:
 #'   \itemize{
 #'     \item \code{x}, the data;
 #'     \item \code{new_weights}, a list of functions, each applied
 #'    to a column of \code{x}, thus generating the vectors of 
 #'    scenario weights;
 #'     \item \code{specs}, the specification of what has been
 #'     stressed.
 #'     \code{specs} is a data.frame consisting of \code{type},
 #'     \code{k}, and constraints depending on the \code{type} of stress,
 #'     see \code{\link{SWIM}} for details.
 #'   }
 #' 
 #' @author Silvana M. Pesenti 
 #'
 #' @export

  merge.SWIM <- function(x, y, ...){
  if (!is.SWIM(x) | !is.SWIM(y)) stop("x and y are not of class SWIM.")
  if (!identical(get.data(x), get.data(y))) stop("x and y are not based on the same data")
  new_weights <- c(get.weightsfun(x), get.weightsfun(y))
  specs <- plyr::rbind.fill(get.specs(x), get.specs(y))
  m <- length(specs$type)
  rownames(specs) <- names(new_weights) <- paste("stress", 1:m)
  xy <- SWIM("x" = get.data(x), "new_weights" = new_weights, "specs" = specs)
  return(xy)
  }