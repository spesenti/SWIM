 ## DESCRIPTION: class SWIM with some methods

 ## This generates a object of class "SWIM"
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

 # get.data provides data of a SWIM object  
  get.data <- function(x, xCol = "all"){
   if (!is.SWIM(x)) stop("Object not of class SWIM")
   if (xCol == "all") xCol = 1:ncol(x$x) else if (!(xCol %in% 1:ncol(x$x))) stop("invalid 'xCol' argument")
   return(as.matrix(x$x[, xCol]))
  }

# get.weights returns a matrix of weights of a SWIM object
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

 # get.weightsfun returns a list of function of weights of a SWIM object 
 # SWIM object
 # a stress with type = c("user", "moment") will be ignored 
  get.weightsfun <- function(x){
   if (!is.SWIM(x)) stop("Object not of class SWIM")
   specs <- get.specs(x)
   typeCol <- which((specs$type != "user") & (specs$type != "moment"))
   if (length(typeCol) != length(specs$type)) warning("type user and moment are ignored. Use get.weights() instead.")    
   return(x$new_weights[as.vector(typeCol)])
  }

 # get.specs provides the information about the stresses 
  get.specs <- function(x){
   if (is.SWIM(x)) return(x$specs) else stop("Object not of class SWIM")
  }
  
 # method of merge 
 # merges two SWIM objects if they are based on the same data.
 # x, y     SWIM object

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