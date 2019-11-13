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
  cname <- colnames(x_data)
  
  VaR <- VaR_stressed(object, alpha = alpha, xCol = xCol, wCol = wCol, base = base)
  
  n <- length(x_data)
  if (!is.null(dim(VaR))){
  for (i in 1:dim(VaR)[2]){
  .VaR <- VaR[,i]
  VaR_matrix <- matrix(rep(.VaR, each = n), ncol = length(.VaR))
  ES <- colMeans((x_data[,i] - VaR_matrix) * (x_data[,i] > VaR_matrix)) / (1 - alpha) + VaR
  }}
  
  return(ES)
} 


