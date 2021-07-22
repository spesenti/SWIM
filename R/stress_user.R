#' User Defined Stress
#'
#' Returns a \code{SWIM} object with scenario weights defined by the user.
#'
#' @inheritParams stress_VaR
#' @param new_weights     A vector, matrix or data frame containing scenario
#'     weights. Columns of \code{new_weights} correspond to different
#'     stresses. \cr
#'     \code{new_weights} are normalised to have a mean of 1.
#' @param new_weightsfun  A list of functions, that applied to
#'     the \code{k}th column of \code{x} generate the vectors of
#'     the scenario weights. Each function corresponds to a stress. \cr
#'     The weights generated for each stress are normalised to
#'     have a mean of 1.
#'
#' @return A \code{SWIM} object containing:
#'     \itemize{
#'       \item \code{x}, a data.frame containing the data;
#'       \item \code{new_weights}, a list, each component corresponds to
#'    a different stress and is either a vector of scenario weights (if \code{new_weights} is provided) or (if \code{new_weightsfun} is provided) a
#'    function, that applied to the \code{k}th column of \code{x}, generates the
#'    vectors of scenario weights;
#'      \item \code{type = "user"};
#'       \item \code{specs}, a list, each component corresponds to
#'    a different stress and contains \code{k}.
#'     }
#'     See \code{\link{SWIM}} for details.
#'
#' @examples
#' set.seed(0)
#' x <- as.data.frame(cbind(
#'   "normal" = rnorm(1000),
#'   "gamma" = rgamma(1000, shape = 2)))
#' res1 <- stress(type = "user", x = x, new_weightsfun = function(x)x ^ 2, k = 1)
#' ## plot user defined weights against the first column of x.
#' plot(x$normal, get_weights(res1), pch = ".")
#'
#' @family stress functions
#' @inherit SWIM references
#' @export

stress_user <- function(x, new_weights = NULL, new_weightsfun = NULL, k = 1, names = NULL){
  if (is.SWIM(x)) x_data <- get_data(x) else x_data <- as.matrix(x)
  if (anyNA(x_data)) warning("x contains NA")
  if (is.null(colnames(x_data))) colnames(x_data) <- paste("X", 1:ncol(x_data), sep = "")
  if (is.function(new_weightsfun)) new_weightsfun <- list(new_weightsfun)
  if (!is.numeric(k) && length(k) != k) stop("k needs to be numeric.")
  if (!is.null(new_weights) && !is.null(new_weightsfun)) stop("only provide new_weightsfun or new_weights.")
  if (!is.null(new_weights)) {
    nweights <- as.matrix(new_weights)
    nweights <- t(t(nweights) / colMeans(nweights))
    if (any(nweights < 0)) stop("Invalid new_weights argument")
    nweights <- lapply(seq_len(ncol(nweights)), function(i) nweights[,i])
    max_length <- length(nweights)
  } else if (!is.null(new_weightsfun)) {
   if (!is.list(new_weightsfun)) new_weightsfun <- list(new_weightsfun)
   nweights_values <- sapply(new_weightsfun, function(s) s(x_data[, k]))
   max_length <- length(new_weightsfun)
   if (any(nweights_values < 0)) stop("Invalid new_weights argument")
   nweights <- list()
   for(i in 1:max_length){
       nweights[[i]] <- function(x) new_weightsfun[[i]](x) / mean(new_weightsfun[[i]](x))
     }
  }
  # names(nweights) <- paste("stress", 1: length(nweights), sep = " ")
  
  # Name stresses
  if (is.null(names)) {
    names <- paste("stress", 1:max_length)
  }
  
  names(nweights) <- names
  
  type <- rep(list("user"), length.out = max_length)
  constr_user <- list("k" = k)
  constr <- rep(list(constr_user), length.out = max_length)
  names(constr) <- names
  my_list <- SWIM("x" = x_data, "new_weights" = nweights, "type" = type, "specs" = constr)
  if (is.SWIM(x)) my_list <- merge(x, my_list)
  return(my_list)
  }
