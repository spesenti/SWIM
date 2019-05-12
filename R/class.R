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

  
  is.SWIM <- function(x) inherits(x, "SWIM")

 #' Extracting data from an object of class \code{SWIM}. 
 #'
 #' Extracting data, \code{x}, from an object of class \code{SWIM}. 
 #' 
 #' @param x         A \code{SWIM} object.
 #' @inheritParams   summary.SWIM
 #'  
 #' @return A data.frame containing the realisations of the
 #'         random variables on which the object of class \code{SWIM}    
 #'         is based on.
 #' @author Silvana M. Pesenti 
 #'
 #' @export

  get.data <- function(x, xCol = "all"){
   if (!is.SWIM(x)) stop("Object not of class SWIM")
   if (xCol == "all") xCol = 1:ncol(x$x) else if (!(xCol %in% 1:ncol(x$x))) stop("invalid 'xCol' argument")
   return(as.matrix(x$x[, xCol]))
  }

 #' Extracting weights from an object of class \code{SWIM}. 
 #'
 #' Extracting weights, \code{new_weights}, from an object of 
 #'     class \code{SWIM}. 
 #' 
 #' @inheritParams get.data
 #'  
 #' @return A data.frame containing the scenario weights of the object
 #'         of class \code{SWIM}. Colums corresponds to different stresses.
 #'         
 #' @author Silvana M. Pesenti 
 #'
 #' @export
  
  get.weights <- function(x){
   if (!is.SWIM(x)) stop("Object not of class SWIM")
   specs <- get.specs(x)
   x_data <- get.data(x)
   m <- nrow(specs)
   new_weights <- matrix(0, nrow = nrow(x_data), ncol = m)
   for(i in 1:m){
    if (specs$type[i] %in% c("user", "moment")) {
     new_weights[, i] <- x$new_weights[, i]
    } else {
     k <- specs$k[i]
     new_weights[, i] <- x$new_weights[[i]](x_data[, k])
    }
   }
   colnames(new_weights) <- paste("stress", 1:m)
   return(new_weights)
  }

 #' Extracting list of weights functions
 #'
 #' Extracting the list of functions from an object of class \code{SWIM},
 #'     that, applied to the \code{k}th colum of \code{x}, gernerates the
 #'     scenario weights. 
 #' 
 #' @note A stress with type = c("user", "moment") will be ignored. 
 #' 
 #' @inheritParams get.data
 #'  
 #' @return A data.frame containing the scenario weights of an object
 #'         of class \code{SWIM}. Colums corresponds to different stresses.
 #'         
 #' @author Silvana M. Pesenti 
 #'
 #' @export
 
  get.weightsfun <- function(x){
   if (!is.SWIM(x)) stop("Object not of class SWIM")
   specs <- get.specs(x)
   typeCol <- which((specs$type != "user") & (specs$type != "moment"))
   if (length(typeCol) != length(specs$type)) warning("type user and moment are ignored. Use get.weights() instead.")    
   return(x$new_weights[as.vector(typeCol)])
  }

 #' Extracting specification of a stress
 #'
 #' Extracting the specifications of an object of class \code{SWIM},
 #'     on which the stresses are based.
 #' 
 #' @inheritParams get.data
 #'  
 #' @return A data.frame containing the specifications of an object
 #'         of class \code{SWIM}. Rows corresponds to different stresses.
 #'         
 #' @author Silvana M. Pesenti 
 #'
 #' @export
 
  get.specs <- function(x){
   if (is.SWIM(x)) return(x$specs) else stop("Object not of class SWIM")
  }

 #' Merge two \code{SWIM} objects
 #'
 #' Merge two objects of class \code{SWIM}, that are based on the same
 #'     data \code{x}. 
 #' 
 #' @note A stress with type = c("user", "moment") will be ignored. 
 #' 
 #' @param x,y       Objects of class \code{SWIM}.
 #'  
 #' @return An object of class \code{SWIM} containing:
 #'   \itemize{
 #'     \item \code{x}, the data;
 #'     \item \code{new_weights}, a list of functions, that applied
 #'    to the \code{k}th colum of \code{x} generate the vectors of 
 #'    scenario weights;
 #'     \item \code{specs}, the specification of what has been
 #'     stressed.
 #'     The \code{specs} is a data.frame consisting of \code{type},
 #'     \code{k}, and constraints depending on the \code{type} of stress,
 #'     see \code{\link{SWIM}} object for details.
 #'     }
 #' 
 #' @author Silvana M. Pesenti 
 #'
 #' @export

  merge.SWIM <- function(x, y){
  if (!is.SWIM(x) | !is.SWIM(y)) stop("x and y are not of class SWIM.")
  if (!identical(get.data(x), get.data(y))) stop("x and y are not based on the same data")
  require(plyr, quietly = TRUE)
  new_weights <- c(get.weightsfun(x), get.weightsfun(y))
  specs <- rbind.fill(get.specs(x), get.specs(y))
  m <- length(specs$type)
  rownames(specs) <- names(new_weights) <- paste("stress", 1:m)
  xy <- SWIM("x" = get.data(x), "new_weights" = new_weights, "specs" = specs)
  return(xy)
  }