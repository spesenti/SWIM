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

 #' Extracting Data 
 #'
 #' Extracting data, \code{x}, from an \code{object} of class \code{SWIM}. 
 #' 
 #' @param object    A \code{SWIM} object.
 #' @inheritParams   summary.SWIM
 #'  
 #' @return A data.frame containing the realisations of the
 #'         random variables on which the object of class \code{SWIM}    
 #'         is based on.
 #' @author Silvana M. Pesenti 
 #'
 #' @export

  get.data <- function(object, xCol = "all"){
   if (!is.SWIM(object)) stop("Object not of class SWIM")
   if (xCol == "all") xCol = 1:ncol(object$x) else if (!(xCol %in% 1:ncol(object$x))) stop("invalid 'xCol' argument")
   return(as.matrix(object$x[, xCol]))
  }

 #' Extracting Weights 
 #'
 #' Extracting weights, \code{new_weights}, from an \code{object} of 
 #'     class \code{SWIM}. 
 #' 
 #' @inheritParams get.data
 #'  
 #' @return A data.frame containing the scenario weights, \code{new_weights} 
 #'         of the object of class \code{SWIM}. Colums corresponds to 
 #'         different stresses.
 #'         
 #' @author Silvana M. Pesenti 
 #'
 #' @export
  
  get.weights <- function(object){
   if (!is.SWIM(object)) stop("Object not of class SWIM")
   specs <- get.specs(object)
   x_data <- get.data(object)
   m <- length(specs$type)
   new_weights <- matrix(0, nrow = nrow(x_data), ncol = m)
   for(i in 1:m){
    if (specs$type[i] %in% c("user", "moment")) {
     new_weights[, i] <- object$new_weights[, i]
    } else {
     k <- specs$k[i]
     new_weights[, i] <- object$new_weights[[i]](x_data[, k])
    }
   }
   colnames(new_weights) <- paste("stress", 1:m)
   return(new_weights)
  }

 #' Extracting List of Weights Functions
 #'
 #' Extracting the list of functions from an \code{object} of class 
 #'     \code{SWIM}, that, applied to the \code{k}th colum of \code{x},
 #'     gernerates the scenario weights. 
 #'      
 #' @note A stress with type = c("user", "moment") will be ignored. 
 #' 
 #' @inheritParams get.data
 #'  
 #' @return A list containing the functions, that, applied to the 
 #'     \code{k}the colum of \code{X}, generate the scenario weights 
 #'     of the \code{object} of class \code{SWIM}.
 #'         
 #' @author Silvana M. Pesenti 
 #'
 #' @export
 
  get.weightsfun <- function(object){
   if (!is.SWIM(object)) stop("Object not of class SWIM")
   specs <- get.specs(object)
   typeCol <- which((specs$type != "user") & (specs$type != "moment"))
   if (length(typeCol) != length(specs$type)) warning("type user and moment are ignored. Use get.weights() instead.")    
   return(object$new_weights[as.vector(typeCol)])
  }

 #' Extracting Specification of a Stress
 #'
 #' Extracting the specifications of an \code{object} of class \code{SWIM},
 #'     on which the stresses are based.
 #' 
 #' @inheritParams get.data
 #'  
 #' @return A data.frame containing the specifications of an \code{object}
 #'         of class \code{SWIM}. Rows corresponds to different stresses.
 #'         See \code{\link{SWIM}} object for details.
 #'         
 #' @author Silvana M. Pesenti 
 #'
 #' @export
 
  get.specs <- function(object){
   if (is.SWIM(object)) return(object$specs) else stop("Object not of class SWIM")
  }

 #' Merge two \code{SWIM} Objects
 #'
 #' Merge two objects of class \code{SWIM}, that are based on the same
 #'     data. 
 #' 
 #' @note A stress with type = c("user", "moment") will be ignored. 
 #' 
 #' @param object1,object2       Objects of class \code{SWIM}.
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
 #'     see \code{\link{SWIM}} for details.
 #'     }
 #' 
 #' @author Silvana M. Pesenti 
 #'
 #' @export

  merge.SWIM <- function(object1, object2){
  if (!is.SWIM(object1) | !is.SWIM(object2)) stop("object1 and object2 are not of class SWIM.")
  if (!identical(get.data(object1), get.data(object2))) stop("object1 and object2 are not based on the same data")
  new_weights <- c(get.weightsfun(object1), get.weightsfun(object2))
  specs <- plyr::rbind.fill(get.specs(object1), get.specs(object2))
  m <- length(specs$type)
  rownames(specs) <- names(new_weights) <- paste("stress", 1:m)
  xy <- SWIM("x" = get.data(object1), "new_weights" = new_weights, "specs" = specs)
  return(xy)
  }