% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ES.R, R/VaR.R
\name{ES_stressed}
\alias{ES_stressed}
\alias{VaR_stressed}
\title{Value-at-Risk and Expected Shortfall of a Stressed Model}
\usage{
ES_stressed(
  object,
  alpha = 0.95,
  xCol = "all",
  wCol = 1,
  base = FALSE,
  gamma = NULL
)

VaR_stressed(object, alpha = 0.95, xCol = "all", wCol = 1, base = FALSE)
}
\arguments{
\item{object}{A \code{SWIM} or \code{SWIMw} object.}

\item{alpha}{Numeric vector, the levels of the stressed VaR and ES
(\code{default = 0.95}).}

\item{xCol}{Numeric or character vector, (names of) the columns of 
the underlying data 
of the \code{object} (\code{default = "all"}).}

\item{wCol}{Numeric, the column of the scenario weights
of the \code{object} (\code{default = 1}).}

\item{base}{Logical, if \code{TRUE}, statistics under the baseline 
are also returned (\code{default = "FALSE"}).}

\item{gamma}{Function that defines the gamma of the risk measure. If null,
the Expected Shortfall (ES) will be used.\cr}
}
\value{
\code{ES_stressed}: Returns a matrix with the empirical or KDE 
    ES's at level \code{alpha} of
    model components specified in \code{xCol}, under the scenario weights
    \code{wCol}.

\code{VaR_stressed}: Returns a matrix with the empirical or KDE VaR's
    at level \code{alpha} of
    model components specified in \code{xCol}, under the scenario weights
    \code{wCol}.
}
\description{
Provides the Value-at-Risk (VaR) and the Expected Shortfall (ES)
    for components (random variables) of a stochastic model.
}
\details{
\code{ES_stressed}: The ES of a stressed model is the
     ES of a chosen stressed model component, subject to the calculated scenario
     weights. The ES at level \code{alpha} of a stressed model
     component is given by:
     \deqn{ES_{alpha} = 1 / (1 - alpha) * \int_{alpha}^1 VaR_u^W d u,}
     where \code{VaR_u^W} is the VaR of the stressed model component, defined below.

\code{VaR_stressed}: The VaR of a model is the VaR (quantile) of
     a chosen stressed model component, subject to the calculated scenario weights.
     The VaR at level \code{alpha} of a stressed model component with
     stressed distribution function F^W is defined as its
     left-quantile at alpha:
     \deqn{VaR_{alpha}^W = F^{W,-1}(alpha).}

     The function \code{VaR_stressed} provides the empirical quantile, whereas
     the function \code{quantile_stressed} calculates quantiles of model
     components with different interpolations.
}
\section{Functions}{
\itemize{
\item \code{ES_stressed}: Expected Shortfall of a stressed model

\item \code{VaR_stressed}: Value-at-Risk of a stressed model.
}}

\examples{
## example with a stress on VaR
set.seed(0)
x <- as.data.frame(cbind(
  "normal" = rnorm(1000),
  "gamma" = rgamma(1000, shape = 2)))
res1 <- stress(type = "VaR", x = x,
  alpha = c(0.9, 0.95), q_ratio = 1.05)
## stressed ES
quantile_stressed(res1, probs = seq(0.9, 0.99, 0.01),
                    xCol = 1, wCol = 2, type = "i/n")
quantile(x[, 1],  probs = seq(0.9, 0.99, 0.01), type = 1)
VaR_stressed(res1, alpha = seq(0.9, 0.99, 0.01), xCol = 1,
                    wCol = 2, base = TRUE)

## the ES of both model components under stress one
ES_stressed(res1, alpha = seq(0.9, 0.99, 0.01), xCol = "all",
                    wCol = 1)
## the ES of both model components under stress two
ES_stressed(res1, alpha = seq(0.9, 0.99, 0.01), xCol = "all",
                    wCol = 2)

}
\seealso{
See \code{quantile_stressed} for quantiles other than the
    empirical quantiles and \code{cdf} for the empirical or KDE distribution
    function of a stressed model.
}
\author{
Silvana M. Pesenti, Zhuomin Mao
}
