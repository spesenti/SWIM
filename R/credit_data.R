#' Credit data set
#'
#' A dataset containing total aggregate losses from three sub-
#'    portfolios, generated through a binomial credit model.
#'
#' @format A data frame with 100,000 rows and 7 variables:
#' \describe{
#'   \item{L}{total aggregate loss of a portfolio consisting
#'            of three homogeneous sub-portfolios L1, L2 and L3}
#'   \item{L1}{aggregate loss of sub-portfolio 1}
#'   \item{L2}{aggregate loss of sub-portfolio 2}
#'   \item{L3}{aggregate loss of sub-portfolio 3}
#'   \item{H1}{(conditional) default probability of sub-portfolio 1}
#'   \item{H2}{(conditional) default probability of sub-portfolio 2}
#'   \item{H3}{(conditional) default probability of sub-portfolio 3}
#' }
#' @source see cite the SWIM Vignette
"credit_data"