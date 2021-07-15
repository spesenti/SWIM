 # Defines the class "SWIM"
  SWIM <- function(x = "x", new_weights = "new_weights", 
                       type = "type", specs = "specs"){
   mymodel <- list(
   x = x, # vector, matrix or dataframe containing the underlying data
   new_weights = new_weights, # list of either functions, that applied 
      # to the kth column of x providing the scenario weights; OR a 
      # vector containing the new_weights
   type  = type, # a list of characters each corresponding to a stress
      # one of ("VaR", "VaR ES", "prob", "moment", "mean", "mean sd", "user")
   specs = specs # a list with elements called "stress i".
      #
      # all input varaibles of the stress and constraints according
      # to the stress. For example a stress on 
      # the VaR contains: k, alpha, q
   )   
   ## Name of the class
   attr(mymodel, "class") <- "SWIM"
   return(mymodel)
  }

  
  is.SWIM <- function(object) inherits(object, "SWIM")

 # Defines the class "SWIMw"
  SWIMw <- function(x = "x", h = "h", u="u", lam = "lam", new_weights = "new_weights", 
                    str.fY = 'str.fY', str.FY = 'str.FY', str.FY.inv = 'str.FY.inv',
                    gamma = 'gamma', type = "type", specs = "specs"){
     mymodel <- list(
        x = x, # vector, matrix or dataframe containing the underlying data
        h = h, # function, that applied to x, provide the bandwidths
        u = u, # vector containing the gridspace on [0,1]
        lam = lam, # optimized lambda value(s)
        str.fY = str.fY, # function, that applied to the stressed column, provide the density
        str.FY = str.FY, # function, that applied to the stressed column, provide the cdf
        str.FY.inv = str.FY.inv, # function, that applied to the stressed column, provide the quantile
        gamma = gamma, # function that provides gamma used to calculate the risk measure;
        new_weights = new_weights, # list of either functions, that applied 
        # to the k-th column of x providing the scenario weights; OR a 
        # vector containing the new_weights
        type  = type, # a list of characters each corresponding to a stress
        # one of ("VaR", "VaR ES", "prob", "moment", "mean", "mean sd", "user")
        specs = specs # a list with elements called "stress i".
        #
        # all input variables of the stress and constraints according
        # to the stress. For example a stress on 
        # the VaR contains: k, alpha, q
     )   
     ## Name of the class
     attr(mymodel, "class") <- "SWIMw"
     return(mymodel)
  }
  
  is.SWIMw <- function(object) inherits(object, "SWIMw")
  
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
 #' @return \code{get_data}: A data.frame containing the realisations of 
 #'         the stochastic model on which the \code{object} is based.    
 #'
 #' @seealso \code{\link{SWIM}}
 #'                  
 #' @author Silvana M. Pesenti 
 #' @describeIn get_data extracting data.
 #' @export

  get_data <- function(object, xCol = "all"){
   if (!is.SWIM(object) && !is.SWIMw(object)) stop("Object not of class SWIM or SWIMw")
   if (length(xCol) ==1 && xCol == "all") {
      xdata <- as.matrix(object$x[, 1:ncol(object$x)])
      if(is.character(colnames(object$x)) | is.null(colnames(object$x))) colnames(xdata) <- colnames(object$x)
   } else{ 
#   if (length(xCol) ==1 && xCol == "all" && is.null(colnames(object$x))) {xCol <- 1:ncol(object$x) 
#      } else if (length(xCol) ==1 && xCol == "all") {xCol <- colnames(object$x)} 
   if (is.null(xCol)) stop("invalid 'xCol' argument")
   if (is.numeric(xCol) && !(any(xCol %in% 1:ncol(object$x)))) stop("invalid 'xCol' argument")
   if (is.character(xCol) && !(any(xCol %in% colnames(object$x)))) stop("invalid 'xCol' argument")
   xdata <- as.matrix(object$x[, xCol])
   if (is.character(xCol[xCol])) colnames(xdata) <- xCol
   if (is.numeric(xCol) && is.null(colnames(object$x)[xCol])) {
      colnames(xdata) <- paste("X", as.character(xCol), sep = "")
   } else if (is.numeric(xCol) && is.character(colnames(object$x)[xCol])) {
      colnames(xdata) <- colnames(object$x)[xCol]
   }}
   return(xdata)
  }

 #' @describeIn get_data extracting scenario weights. 
 #'
 #' @return \code{get_weights:} A data.frame containing the scenario 
 #'     weights of the \code{object}. Columns corresponds 
 #'     to different stresses.
 #'         
 #' @examples 
 #' ## continuing example in stress_VaR
 #' set.seed(0)
 #' x <- as.data.frame(cbind(
 #'      "normal" = rnorm(1000), 
 #'      "gamma" = rgamma(1000, shape = 2)))
 #'   res1 <- stress(type = "VaR", x = x, 
 #'                  alpha = 0.9, q_ratio = 1.05, k = 1)
 #' 
 #' ## returning the underlying data
 #' all(get_data(res1) == x)
 #'  ## the scenario weights
 #' w <- get_weights(res1) 
 #' get_weightsfun(res1)
 #' get_specs(res1)
 #'   
 #' ## now add a stress on the means of both variables
 #' res1 <- stress(type = "mean", x = res1, k = 1:2, new_means = c(0.5,1.5))
 #' get_specs(res1)
 #' ## the required moments for a stress of type "mean" are not displayed 
 #' ## the type of stress and the specs for the second stress can be 
 #' ## extracted directly from the SWIM object.
 #' res1$type[[2]]
 #' res1$specs[[2]]
 #'                                              
 #'                                                               
 #'                                                                                                 
 #' @export

  get_weights <- function(object){
   if (!is.SWIM(object) && !is.SWIMw(object)) stop("Object not of class SWIM or SWIMw")
   x_data <- get_data(object)
   m <- length(object$type)
   new_weights <- matrix(0, nrow = nrow(x_data), ncol = m)
   for(i in 1:m){
    if (!is.function(object$new_weights[[i]])) {
     new_weights[, i] <- object$new_weights[[i]]
    } else {
     k <- object$specs[[i]]$k
     new_weights[, i] <- object$new_weights[[i]](x_data[, k])
    }
   }
   colnames(new_weights) <- paste("stress", 1:m)
   return(new_weights)
  }

 #' @describeIn get_data extracting weight functions.
 #'
 #' @return \code{get_weightsfun}: A list containing functions, which, 
 #'     when applied to a column of the data, generate the 
 #'     scenario weights of the \code{object}. The corresponding stressed 
 #'     columns can be obtained via \code{get_specs}.\cr
 #'     Use \code{\link{get_weights}} if the \code{SWIM} object only contains 
 #'     scenario weights and not a list of functions.
 #'         
 #' @export

  get_weightsfun <- function(object){
   if (!is.SWIM(object)) stop("Object not of class SWIM")
   if (!is.function(object$new_weights[[1]]))
      stop("New_weights is not a function, use get_weights() instead.") 
   return(object$new_weights)
  }

 #' @describeIn get_data extracting information of the stress.
 #'
 #' @return \code{get_specs}: A data.frame containing specifications 
 #'         of the stresses with each row corresponding to a different 
 #'         stress. Only a selection of the specifications is returned; 
 #'         however, all input variables are stored in the \code{object}.
 #'         See also \code{\link{SWIM}}.
 #' @export

  get_specs <- function(object){
   if (!is.SWIM(object)) stop("Object not of class SWIM")
   .type <- object$type 
   .specs <- data.frame()
   for (i in 1:length(.type)){  
   if (.type[i] %in% c("VaR", "VaR ES", "user", "prob")){
      if (.type[[i]] == "prob" && length(object$specs[[i]]$prob) > 1){
      .specs <- plyr::rbind.fill(.specs, as.data.frame(t(unlist(object$specs[[i]]))))       
      } else {
      .specs <- plyr::rbind.fill(.specs, as.data.frame(object$specs[[i]], stringsAsFactors = FALSE)) 
      }} else if (.type[i] %in% c("moment", "mean", "mean sd")){
      k <- paste(object$specs[[i]]$k, collapse = " ")
      .specs <- plyr::rbind.fill(.specs, as.data.frame(k))
   } else stop("Object contains wrong type.")}
   .type <- t(as.data.frame(.type))
   .specs <- cbind(.type, .specs)
   colnames(.specs)[1] <- "type"
   rownames(.specs) <- paste("stress", 1:length(.type), sep = " ")      
   return(.specs)
  }

  
  
  
 #' Merging Two Stressed Models
 #'
 #' This function is a \code{method} for an object of class 
 #'     \code{SWIM}.
 #'     
 #' @details Merges two objects of class \code{SWIM}, that 
 #'     are based on the same data. \cr 
 #' 
 #' @param x,y    Objects of class \code{SWIM}.
 #' @param ...    Additional arguments will be ignored.
 #'  
 #' @return An object of class \code{SWIM} containing:
 #'   \itemize{
 #'     \item \code{x}, a data.frame containing the data;
 #'     \item \code{new_weights}, a list, each component corresponds to 
 #'    a different stress and is either a vector of scenario weights or a
 #'    function, that applied to a column of \code{x}, generates the 
 #'    vectors of scenario weights; 
 #'     \item \code{type}, a list, each component corresponds to a 
 #'    different stress and specifies the type of the stress;
 #'     \item \code{specs}, a list, each component corresponds to 
 #'    a different stress and contains a list with the specifications 
 #'    of what has been stressed.
 #'   }
 #' See \code{\link{SWIM}} for details.
 #' 
 #' @author Silvana M. Pesenti 
 #'
 #' @export

  merge.SWIM <- function(x, y, ...){
  if (!is.SWIM(x) | !is.SWIM(y)) stop("x and y are not of class SWIM.")
  if (!identical(get_data(x), get_data(y))) stop("x and y are not based on the same data")
  type <- c(x$type, y$type)
  m <- length(type)
  new_weights <- c(x$new_weights, y$new_weights)
  names(new_weights) <- paste("stress", 1:m)
  specs <- c(x$specs, y$specs)
  names(specs) <- paste("stress", 1:m)
  xy <- SWIM("x" = get_data(x), "new_weights" = new_weights, "type" = type, "specs" = specs)
  return(xy)
  }