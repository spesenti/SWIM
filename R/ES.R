#' @describeIn VaR_stressed Expected Shortfall of a stressed model
#'
#' @return \code{ES_stressed}: Returns a matrix with the empirical or KDE 
#'     ES's at level \code{alpha} of
#'     model components specified in \code{xCol}, under the scenario weights
#'     \code{wCol}.
#'
#' @details \code{ES_stressed}: The ES of a stressed model is the
#'      ES of a chosen stressed model component, subject to the calculated scenario
#'      weights. The ES at level \code{alpha} of a stressed model
#'      component is given by:
#'      \deqn{ES_{alpha} = 1 / (1 - alpha) * \int_{alpha}^1 VaR_u^W d u,}
#'      where \code{VaR_u^W} is the VaR of the stressed model component, defined below.
#'
#' @export

ES_stressed <- function(object, alpha = 0.95, xCol = "all", wCol = 1, base = FALSE, gamma = NULL) {
  if (!is.SWIM(object) && !is.SWIMw(object)) stop("Wrong object")
  if (any(alpha <= 0) || any(alpha >= 1)) stop("Invalid alpha argument")
  if (is.character(xCol) && xCol == "all") xCol <- 1:ncol(get_data(object))
  x_data <- as.matrix(get_data(object, xCol = xCol))
  weights <- matrix(rep(get_weights(object)[, wCol], dim(x_data)[2] ), ncol = dim(x_data)[2])
  if (base == TRUE) weights <- cbind(weights, matrix(1, nrow = dim(x_data)[1], ncol = dim(x_data)[2]))
  if (base == TRUE) x_data <- cbind(x_data, x_data)
  if (!is.null(gamma)){
    if (!all(sapply(gamma, is.function))) stop("gamma must be a function")
  } else{
    gamma <- function(x, alpha_i){as.numeric((x >= alpha_i) / (1 - alpha_i))}
  }

  if (is.SWIM(object)){
    # K-L Divergence
    VaR <- VaR_stressed(object, alpha = alpha, xCol = xCol, wCol = wCol, base = base)
    ES <- matrix(nrow = dim(VaR)[1], ncol = dim(VaR)[2])
    for (i in 1:dim(VaR)[2]){
      ES[, i] <- .ES_stressed(x_data[, i], w = weights[, i], alpha = alpha, VaR = VaR[, i])
    }
    
  } else {
    # Wasserstein Distance
    index <- names(object$specs)[wCol]
    k <- object$specs[[index]]$k
    if(is.character(k)) k_name <- k
    if(is.null(colnames(get_data(object)))) k_name <- paste("X", k, sep = "") 
    else if(!is.character(k)) k_name <- colnames(get_data(object))[k]
    
    u <- object$u
    w <- get_weights(object)[ , wCol]
    
    VaR <- VaR_stressed(object, alpha = alpha, xCol = xCol, wCol = wCol, base = base)
    ES <- c()
    for (c in 1:length(xCol)){
      h <- object$h[[wCol]](x_data[, c])
      lower_bracket = min(x_data[, c])#-(max(x_data[, c])-min(x_data[, c]))*0.1
      upper_bracket = max(x_data[, c])#+(max(x_data[, c])-min(x_data[, c]))*0.1
      
      if(k_name == colnames(get_data(object))[c]){
        # Get stressed quantile
        G.inv.fn <- Vectorize(object$str_FY_inv[[wCol]])
      } else{
        # Get KDE
        G.fn <- function(x){
          return(sum(w * stats::pnorm((x - x_data[,c])/h)/length(x_data[,c])))
        }
        G.fn <- Vectorize(G.fn)
        G.inv.fn <- Vectorize(.inverse(G.fn, lower_bracket, upper_bracket))
      }
      temp <- c()
      for (alpha_i in alpha) {temp <- c(temp, .rm(G.inv.fn(u), gamma(u, alpha_i), u))}
      ES <- cbind(ES, temp)
      
      if (base == TRUE){
        # Get KDE
        F.fn <- function(x){
          return(sum(stats::pnorm((x - x_data[, c])/h)/length(x_data[, c])))
        }
        F.fn <- Vectorize(F.fn)
        F.inv.fn <- Vectorize(.inverse(F.fn, lower_bracket, upper_bracket))
        
        temp <- c()
        for (alpha_i in alpha) {temp <- c(temp, .rm(G.inv.fn(u), gamma(u, alpha_i), u))}
        ES <- cbind(ES, temp)
      }
    }
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

# help function to get risk measure for gamma function
.rm <- function(F_inv, gamma, u){
  return(.integrate(F_inv*gamma, u))
}

.integrate <- function(f, x){
  return(sum(0.5*(f[1:length(f) - 1] + f[2:length(f)])*diff(x)))
}


