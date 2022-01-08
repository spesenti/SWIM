 # Defines the class "SWIM"
  SWIM <- function(x = "x", new_weights = "new_weights", 
                       type = "type", specs = "specs"){
   mymodel <- list(
      x = x, 
      # vector, matrix or dataframe containing the underlying data
      new_weights = new_weights, 
      # list of eithter functions, that applied 
      # to the kth column of x providing the scenario weights; OR a 
      # vector containting the new_weights
      type  = type,
      # a list of characters each corresponding to a stress
      # one of ("VaR", "VaR ES", "prob", "moment", "mean", "mean sd", "user")
      specs = specs
      # a list with elements called "stress i".
      # all input varaibles of the stress and constraints according
      # to the stress. For example a stress on 
      # the VaR contains: k, alpha, q
      # names = names
   )   
   
   ## Name of the class
   attr(mymodel, "class") <- "SWIM"
   return(mymodel)
  }

  
  is.SWIM <- function(object) inherits(object, "SWIM")

 # Defines the class "SWIMw"
  SWIMw <- function(x = "x", h = "h", u="u", lam = "lam", new_weights = "new_weights", 
                    str_fY = 'str_fY', str_FY = 'str_FY', str_FY_inv = 'str_FY_inv',
                    gamma = 'gamma', type = "type", specs = "specs"){
     mymodel <- list(
        x = x, # vector, matrix or dataframe containing the underlying data
        h = h, # function, that applied to x, provide the bandwidths for KDE estimation
        u = u, # vector containing the gridspace on [0,1]
        lam = lam, # optimized lambda value(s)
        str_fY = str_fY, # function, that applied to the stressed column, provides the density
        str_FY = str_FY, # function, when applied to the stressed column, provides the cdf
        str_FY_inv = str_FY_inv, # function, when applied to the stressed column, provides the quantile
        gamma = gamma, # function that provides gamma used to calculate the risk measure (if applicable);
        new_weights = new_weights, # list of either functions, that applied 
        # to the k-th column of x providing the scenario weights; OR a 
        # vector containing the new_weights
        type  = type, # a list of characters each corresponding to a stress
        # one of ("RM", "mean sd", "RM mean sd", "HARA RM")
        specs = specs # a list with elements called "stress i".
        #
        # all input variables of the stress and constraints according
        # to the stress. For example a stress on 
        # the RM contains: k, alpha, q
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
 #'     \code{SWIM} or \code{SWIMw}. 
 #' 
 #' @param object    A \code{SWIM} or \code{SWIMw} object.
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

  get_weights <- function(object, wCol = "all"){
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
   colnames(new_weights) <- names(object$specs)
   if (is.character(wCol) && wCol == "all") return (new_weights)
   return(new_weights[, wCol])
  }

 #' @describeIn get_data extracting weight functions.
 #'
 #' @return \code{get_weightsfun}: A list containing functions, which, 
 #'     when applied to a column of the data, generate the 
 #'     scenario weights of the \code{object}. The corresponding stressed 
 #'     columns can be obtained via \code{get_specs}.\cr
 #'     
 #'     Use \code{\link{get_weights}} if the \code{SWIM} object only contains 
 #'     scenario weights and not a list of functions.
 #'         
 #' @export

  get_weightsfun <- function(object, wCol = "all"){
   if (!is.SWIM(object) && !is.SWIMw(object)) stop("Object not of class SWIM or SWIMw")
   if (!is.function(object$new_weights[[1]]))
      stop("New_weights is not a function, use get_weights() instead.") 
   if (is.character(wCol) && wCol == "all") return (object$new_weights)
   return(object$new_weights[wCol])
  }

 #' @describeIn get_data extracting information of the stress.
 #'
 #' @return \code{get_specs}: A data.frame containing specifications 
 #'         of the stresses with each row corresponding to a different 
 #'         stress. Only a selection of the specifications is returned; 
 #'         however, all input variables are stored in the \code{object}.
 #'         See also \code{\link{SWIM}}.
 #' @export

  get_specs <- function(object, wCol = "all"){
   if (!is.SWIM(object) && !is.SWIMw(object)) stop("Object not of class SWIM or SWIMw.")
   .type <- object$type 
   .specs <- data.frame()
   if (is.SWIM(object)){
     for (i in 1:length(.type)){  
       if (.type[i] %in% c("VaR", "VaR ES", "user", "prob")){
         if (.type[[i]] == "prob" && length(object$specs[[i]]$prob) > 1){
           .specs <- plyr::rbind.fill(.specs, as.data.frame(t(unlist(object$specs[[i]]))))       
         } else {
           .specs <- plyr::rbind.fill(.specs, as.data.frame(object$specs[[i]], stringsAsFactors = FALSE)) 
         }
       } else if (.type[i] %in% c("moment", "mean", "mean sd")){
         k <- paste(object$specs[[i]]$k, collapse = " ")
         .specs <- plyr::rbind.fill(.specs, as.data.frame(k))
       } else stop("Object contains wrong type.")
    }
  } else {
    # Wasserstein
    for (i in 1:length(.type)){
     if (.type[i] %in% c("ES", "ES mean sd", "HARA ES")) {
       .specs <- plyr::rbind.fill(.specs, as.data.frame(object$specs[[i]], stringsAsFactors = FALSE)[1:3]) # Only include k, alpha, and q
     } else if (.type[i] %in% c("RM", "RM mean sd", "HARA RM")) {
       .specs <- plyr::rbind.fill(.specs, as.data.frame(object$specs[[i]], stringsAsFactors = FALSE)[c(1,3)]) # Only include k and q
     } else if (.type[i] %in% c("mean sd", "mean")){
       k <- paste(object$specs[[i]]$k, collapse = " ")
       .specs <- plyr::rbind.fill(.specs, as.data.frame(k))
    } else stop("Object contains wrong type.")
    }
  }
   .type <- t(as.data.frame(.type))
   .specs <- cbind(.type, .specs) 
   colnames(.specs)[1] <- "type"
   rownames(.specs) <- names(object$specs)
   return(.specs)
  }

  #' Rename Stressed Models
  #'     
  #' @details Get a new \code{SWIM} object with desired \code{name}
  #' 
  #' @param object A \code{SWIM} or \code{SWIMw} object
  #' @param names   Character vector, the new names of k-th stressed model.
  #' @param k   Numeric vector, the k-th stressed model of object to rename. (\code{default = 1}).
  #'  
  #' @return An renamed object of class \code{SWIM} containing:
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
  #' @examples
  #' set.seed(0)
  #' x <- as.data.frame(cbind(
  #'   "normal" = rnorm(1000),
  #'   "gamma" = rgamma(1000, shape = 2)))
  #' res1 <- stress(type = "VaR", x = x,
  #'   alpha = 0.9, q_ratio = 1.05)
  #' res1 <- rename(res1, "VaR_09", 1)
  #' 
  #' @author Kent Wu 
  #'
  #' @export
  
  rename <- function(object, `names`, k=1){
    if (!is.SWIM(object) & !is.SWIMw(object)) stop("Object not of class SWIM or SWIMw")
    if (length(names) != length(k)) stop("The number of names should be equal to k")
    temp <- names(object$new_weights)
    if (length(temp) > length(k) | length(temp) > length(names)) stop("The number of names or k exceeds the number of stressed models")
    temp[k] <- names
    names(object$new_weights) <- temp
    names(object$specs) <- temp
    object <- object
    return(object)
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
  
  specs <- c(x$specs, y$specs)
  
  # Check if there are duplicate names for stresses
  x_name <- names(x$specs)
  y_name <- names(y$specs)
  
  if (length(intersect(x_name, y_name)) >= 1) {
     names(new_weights) <- paste("stress", 1:m)
     names(specs) <- paste("stress", 1:m)
  } else {
    original <- c(x_name, y_name)
    for (i in 1:length(original)) {
      if (startsWith(original[i], "stress ")) original[i] <- paste("stress", i, collapse = " ")
    }
    names(new_weights) <- original
    names(specs) <- original
  }

  xy <- SWIM("x" = get_data(x), "new_weights" = new_weights, "type" = type, "specs" = specs)
  return(xy)
  }

  #' Merging Two Stressed Models
  #'
  #' This function is a \code{method} for an object of class 
  #'     \code{SWIMw}.
  #'     
  #' @details Merges two objects of class \code{SWIMw}, that 
  #'     are based on the same data. \cr 
  #' 
  #' @param x,y    Objects of class \code{SWIMw}.
  #' @param ...    Additional arguments will be ignored.
  #'  
  #' @return A \code{SWIMw} object containing:
  #'     \itemize{
  #'       \item \code{x}, a data.frame containing the data;
  #'       \item \code{h}, bandwidths;
  #'       \item \code{u}, vector containing the gridspace on [0, 1]
  #'       \item \code{lam}, vector containing the lambda's of the optimized model
  #'       \item \code{str_fY}, function defining the densities of the stressed component;
  #'       \item \code{str_FY}, function defining the distribution of the stressed component;
  #'       \item \code{str_FY_inv}, function defining the quantiles of the stressed component;
  #'       \item \code{gamma}, function defining the risk measure;
  #'       \item \code{new_weights}, a list of functions, that applied to
  #'   the \code{k}th column of \code{x}, generates the vectors of scenario
  #'   weights. Each component corresponds to a different stress;
  #'      \item \code{type}, specifies the stress type
  #'      \item \code{specs}, a list, each component corresponds to
  #'    a different stress and contains a list with the specifications
  #'   of what has been stressed.
  #'     }
  #'     See \code{\link{SWIM}} for details.
  #' @author Zhuomin Mao
  #'
  #' @export  
  merge.SWIMw <- function(x, y, ...){
    if (!is.SWIMw(x) | !is.SWIMw(y)) stop("x and y are not of class SWIMw.")
    if (!identical(get_data(x), get_data(y))) stop("x and y are not based on the same data")
    if (!identical(get_data(x), get_data(y))) stop("x and y are not based on the same gridspace")
    
    type <- c(x$type, y$type)
    h <- c(x$h, y$h)
    lam <- c(x$lam, y$lam)
    str_fY <- c(x$str_fY, y$str_fY)
    str_FY <- c(x$str_FY, y$str_FY)
    str_FY_inv <- c(x$str_FY_inv, y$str_FY_inv)
    gamma <- c(x$gamma, y$gamma)
    m <- length(type)
    new_weights <- c(x$new_weights, y$new_weights)
    specs <- c(x$specs, y$specs)
    # names(new_weights) <- paste("stress", 1:m)
    # names(specs) <- paste("stress", 1:m)
    
    # Check if there are duplicate names for stresses
    x_name <- names(x$specs)
    y_name <- names(y$specs)
    
    if (length(intersect(x_name, y_name)) >= 1) {
      names(new_weights) <- paste("stress", 1:m)
      names(specs) <- paste("stress", 1:m)
      }
     
    xy <- SWIMw("x" = get_data(x), "new_weights" = new_weights, "type" = type, "specs" = specs,
                "str_fY" = str_fY, "str_FY" = str_FY, "str_FY_inv" = str_FY_inv,
                "u" = x$u, "h" = h, "lam"=lam, "gamma"=gamma)
    return(xy)
  }
  
  #' @describeIn get_data extracting summary statistics of scenario weights.
  #'
  #' @return \code{summary_weights}: print a list containing summary statistics 
  #'         of the stresses with each element being a table for a different stress. 
  #'         The summary statistics include minimum, maximum, standard deviation, 
  #'         Gini coefficient, entropy and effective sample size. 
  #'         
  #'         Gini coefficient uses the formula \eqn{\frac{\sum_{i=1}^{n} \sum_{j=1}^{n}\left|x_{i}-x_{j}\right|}{2 n^{2} \bar{x}}}.
  #'         
  #'         Effective Sample Size is equal to n / (1+Var(W)), see 
  #'         Equation (9.13) in Owen, Art B. "Monte Carlo theory, methods and examples." (2013).
  #' @export

  summary_weights <- function(object, wCol = "all"){
    table <- list()
    if (is.character(wCol) && wCol == "all") wCol <- 1:ncol(get_weights(object))
    new_weights <- get_weights(object)[ ,wCol]
    
    for (i in wCol){
      w <- get_weights(object)[, i]
      freqs <- w / length(w)
      sub_table <- t(matrix(c(min(w), max(w), stats::sd(w), .gini(w), .entropy(freqs), length(w)/(1 + stats::var(w)))))
      sub_table <- round(sub_table, 4)
      colnames(sub_table) <- c("min", "max", "sd", "Gini coef", "Entropy", "effective sample size")
      name <- names(object$specs)[i]
      table[[name]] <- sub_table
    }
    print(table)
  }
  
  .gini <- function(w) mean(outer(X = w, Y = w, FUN = function(x, y)abs(x - y))) / 2
  .entropy <- function(freqs) -sum(freqs * log2(freqs))

  
  #' Convert SWIMw to SWIM 
  #'     
  #' @details Convert a SWIMw object into a SWIM object
  #' 
  #' @param object A \code{SWIMw} object
  #'  
  #' @return A \code{SWIM} object containing:
  #'     \itemize{
  #'       \item \code{x}, a data.frame containing the data;
  #'       \item \code{new_weights}, a list of functions, that applied to
  #'   the \code{k}th column of \code{x}, generates the vectors of scenario
  #'   weights. Each component corresponds to a different stress;
  #'      \item \code{type = "VaR"};
  #'      \item \code{specs}, a list, each component corresponds to
  #'    a different stress and contains \code{k},
  #'     \code{alpha} and \code{q}.
  #'     }
  #'     See \code{\link{SWIM}} for details.
  #' 
  #' @examples
  #' \dontrun{
  #' set.seed(0)
  #' x <- as.data.frame(cbind(
  #'   "normal" = rnorm(1000),
  #'   "gamma" = rgamma(1000, shape = 2)))
  #' res1 <- stress_wass(type = "RM", x = x,
  #'   alpha = 0.9, q_ratio = 1.05)
  #' convert_SWIMw_to_SWIM(res1)
  #' }
  #' 
  #' @author Kent Wu 
  #'
  #' @export

  convert_SWIMw_to_SWIM <- function(object) {
    if (!is.SWIMw(object)) stop("Object not of class SWIMw")
    my_list <- SWIM("x" = object$x, "new_weights" = object$x, "type" = object$type, "specs" = object$specs)
    return (my_list)
  }