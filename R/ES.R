#' @describeIn VaR_stressed Expected Shortfall of a stressed model
#' 
#' @return \code{ES_stressed}: Returns a matrix with the empirical ES's at level \code{alpha} of 
#'     model components specified in \code{xCol}, under the scenario weights 
#'     \code{wCol}. 
#'     
#' @details \code{ES_stressed}: The ES of a stressed model is the
#'      ES of a chosen stressed model component, subject to the calculated scenario 
#'      weights. The ES at level \code{alpha} of a stressed model 
#'      component is given by:
#'      \deqn{ES_alpha = 1 / (1 - alpha) * int_alpha^1 VaR_u^W d u,}
#'      where \code{VaR_u^W} is the VaR of the stressed model component, defined below.
#'      
#' @export
#'     

ES_stressed <- function(object, alpha = 0.95, xCol = "all", wCol = 1, base = FALSE) {
  if (!is.SWIM(object)) stop("Wrong object")
  if (any(alpha <= 0) || any(alpha >= 1)) stop("Invalid alpha argument")
  if (is.character(xCol) && xCol == "all") xCol <- 1:ncol(get_data(object))
  x_data <- as.matrix(get_data(object, xCol = xCol))
  weights <- matrix(rep(get_weights(object)[, wCol], dim(x_data)[2] ), ncol = dim(x_data)[2])
  if (base == TRUE) weights <- cbind(weights, matrix(1, nrow = dim(x_data)[1], ncol = dim(x_data)[2]))
  if (base == TRUE) x_data <- cbind(x_data, x_data)
    
  VaR <- VaR_stressed(object, alpha = alpha, xCol = xCol, wCol = wCol, base = base)
  ES <- matrix(nrow = dim(VaR)[1], ncol = dim(VaR)[2])
  for (i in 1:dim(VaR)[2]){
    ES[, i] <- .ES_stressed(x_data[, i], w = weights[, i], alpha = alpha, VaR = VaR[, i])
  }

  rownames(ES) <- rownames(VaR)
  colnames(ES) <- colnames(VaR)
  return(ES)
} 

 # help function that calculates the ES for x and alpha a vector.
.ES_stressed<- function(x, w, alpha, VaR){
  # x: vector of data
  # w: vector of weights
  # alpha: vector of ES levels
  # VaR: vector, same length as alpha, with VaR at level alpha of x
  
  VaR_matrix <- matrix(rep(VaR, each = length(x)), ncol = length(VaR))
  w_matrix <- matrix(rep(w, length(alpha)), ncol = length(alpha))
  ES <- colMeans(w_matrix * (x - VaR_matrix) * (x > VaR_matrix)) / (1 - alpha) + VaR
  return(matrix(ES, ncol = 1))
}



