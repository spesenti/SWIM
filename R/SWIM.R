 #' SWIM: A Package for Sensitivity Analysis
 #'
 #' The \code{SWIM} package provides weights on simulated scenarios
 #'     from a stochastic model, such that a stressed model component
 #'     (random variable) fulfil given probabilistic constraints (e.g.
 #'     specified values for risk measures), under the new scenario weights.
 #'     Scenario weights are selected by constrained minimisation of the
 #'     relative entropy or Wasserstein distance to the baseline model.
 #'
 #' @details The \code{SWIM} (Scenario Weights for Importance Measurement)
 #'     package provides weights on simulated scenarios from a stochastic
 #'     model, such that stressed random variables fulfil given
 #'     probabilistic constraints (e.g. specified values for risk
 #'     measures), under the new scenario weights. Scenario weights are
 #'     selected by constrained minimisation of the relative entropy or 
 #'     Wasserstein distance to the
 #'     baseline model.
 #'
 #'     The \code{SWIM} package is based on the \emph{reverse sensitivity
 #'     framework} developed by (Pesenti et al. 2019) and \insertCite{Pesenti2021SSRN}{SWIM}.
 #'     
 #'     Consider the random vector \code{X = (X1,...,Xn)}. Let P
 #'     represent the probability measure under which all simulated
 #'     scenarios have the same probability. First, take the 
 #'     approach of minimizing the relative entropy. Then, for a random variable
 #'     \code{Xi}, the package solves:
 #'     \deqn{min D(P | Q)}
 #'     subject to constraints on the distribution
 #'     of \code{Xi} under \code{Q},
 #'     where \code{D(P | Q)} is the Kullback-Leibler divergence
 #'     (relative entropy) between \code{P} and \code{Q}.
 #'
 #'     The approach of minimizing the Wasserstein distance of order 2 proceeds 
 #'     as follows: Let F be the distribution function of the random variable \code{Xi} 
 #'     under \code{P}, then the package solves
 #'     \deqn{argmin_{G} W_{2}(G, F)}
 #'     subject to constraints on \code{G}, \code{W_{2}(G, F)} is the 2-Wasserstein distance 
 #'     between \code{G} and \code{F}. The solution to the above minimisation problem is the 
 #'     distribution of \code{Xi} under \code{Q}. The current implementation of the Wasserstein 
 #'     approach is based on Kernel density estimation with Gaussian kernels.
 #'
 #'    For both approaches, the scenario weights are then formed via the Radon-Nikodym 
 #'    derivative \code{dQ / dP}. The weighting generates a model for which the joint distribution 
 #'    of \code{(X1,...,Xn)} is stressed.
 #'
 #'     Different elements of \code{X} can be understood as
 #'     inputs or outputs of a model. For example, consider a model
 #'     \code{Y = g(Z)} with input vector \code{Z = (Z1,...,Z(n-1))}.
 #'     One can then identify \code{X1 = Y} and \code{X2 = Z1,...,Xn
 #'     = Z(n-1)}. Subsequently, the user of the \code{SWIM} package can
 #'     stress the model output or any of the inputs, measuring the
 #'     resulting impact on the distributions of other variables.
 #'
 #' @section Stresses for Relative Entropy Minimization:
 #'     Scenario weights for the following stresses are provided:
 #'     \tabular{ll}{
 #'        \code{\link{stress}}\tab calls one of the functions below by
 #'     using \code{type}\cr
 #'       \code{\link{stress_VaR}} \tab for stressing the VaR
 #'     (\code{type = "VaR"})\cr
 #'       \code{\link{stress_VaR_ES}} \tab for stressing the VaR and
 #'     ES jointly (\code{type = "VaR ES"})\cr
 #'       \code{\link{stress_mean}}\tab for stressing means
 #'     (\code{type = "mean"}) \cr
 #'       \code{\link{stress_mean_sd}} \tab for stressing means and
 #'     standard deviations (\code{type = "mean std"})\cr
 #'       \code{\link{stress_moment}} \tab for stressing moments
 #'     (\code{type = "moment"})\cr
 #'       \code{\link{stress_prob}} \tab for stressing the probabilities
 #'       of intervals
 #'     (\code{type = "prob"}) \cr
 #'       \code{\link{stress_user}} \tab for user defined scenario weights
 #'     (\code{type = "user"}) \cr
 #'     }
 #'
 #' @section Stresses for Wasserstein Distance Minimization:
#'     Scenario weights for the following stresses are provided:
#'     \tabular{ll}{
#'       \code{\link{stress_wass}}\tab calls one of the functions below by
#'     using \code{type}\cr
#'       \code{\link{stress_RM_w}} \tab for stressing the distortion risk measure (RM)
#'     (\code{type = "RM"})\cr
#'       \code{\link{stress_mean_sd_w}} \tab for stressing mean and
#'     standard deviation (\code{type = "mean sd"})\cr
#'       \code{\link{stress_RM_mean_sd_w}} \tab for stressing the RM, mean and 
#'       standard deviation (\code{type = "RM mean sd"})\cr
#'       \code{\link{stress_HARA_RM_w}} \tab for stressing the HARA utility and RM
#'     (\code{type = "HARA RM"})\cr
#'       \code{\link{stress_mean_w}} \tab for stressing mean (\code{type = "mean"})
#'     }
 #'
 #' @section A \code{SWIM} object:
 #'     A SWIM object is generated by applying a stress function subject to a relative entropy minimisation. 
 #'     An object of class \code{SWIM} contains a list of:
 #'   \itemize{
 #'     \item \code{x}, a data.frame containing realisations of a random
 #'   vector;
 #'     \item \code{new_weights}, a list, each component corresponds to
 #'    a different stress and is either a vector of scenario weights or a
 #'    function, that applied to the \code{k}th column of \code{x},
 #'    generates the vectors of scenario weights;
 #'     \item \code{type}: a list, each component corresponds to a
 #'    different stress and specifies the type of the stress;
 #'     \item \code{specs}, a list, each component corresponds to
 #'   a different stress and contains a list with the specifications
 #'   of what has been stressed.
 #'   Specifications depend on the \code{type} of stress:
 #'     \itemize{
 #'       \item \code{type = "VaR"}: \code{k}, the column of \code{x}
 #'     on which the stress is applied to; \code{alpha}, the level of
 #'     the stressed VaR; \code{q}, the stressed VaR at level
 #'       \code{alpha}.
 #'       \item \code{type = "VaR ES"}: \code{k}, the column of \code{x}
 #'     on which the stress is applied to; \code{alpha}, the level of the
 #'     stressed VaR and ES; \code{q}, the stressed VaR at level
 #'     \code{alpha}.
 #'       \item \code{type = "mean"}: \code{k}, the columns of \code{x}
 #'     on which the stress is applied to; \code{new_means}, the
 #'     stressed means.
 #'       \item \code{type = "mean sd"}: \code{k}, the columns of \code{x}
 #'     on which the stress is applied to; \code{new_means}, the
 #'     stressed means; \code{new_sd}, the stressed standard deviations.
 #'     \code{s}, the stressed ES at level \code{alpha}.
 #'       \item \code{type = "moment"}: \code{f}, the list of functions,
 #'     that, applied to \code{x}, constitute the moment constraints;
 #'     \code{k}, the columns of \code{x} on which each function in
 #'     \code{f} operates on; \code{m}, the stressed moments of
 #'     \code{f(x)}.
 #'       \item \code{type = "prob"}: \code{k}, the column of \code{x}
 #'     on which the stress is applied to; \code{lower}, the left
 #'     endpoints of the intervals; \code{upper}, the right endpoints
 #'     of the intervals; \code{prob}, stressed probabilities
 #'     corresponding to the intervals defined through \code{lower}
 #'     and \code{upper}.
 #'       \item \code{type = "user"}: \code{k}, the column of \code{x}
 #'     on which the stress is applied to.
 #'     }
 #'   }
 #'
#' @section A \code{SWIMw} object:
#'     A SWIMw object is generated by applying a stress function 
#'     subject to a Wasserstein minimisation. The Wasserstein 
#'     minimisation approach assumes that all model components, 
#'     (random variables) are continuously distributed. If only 
#'     the stressed model component is continuously distributed, 
#'     the SWIMw stress should be converted to a SWIM object, see 
#'     \code{convert_SWIMw_to_SWIM}. 
#'     An object of class \code{SWIMw} contains a list of:
#'   \itemize{
#'     \item \code{x}, a data.frame containing realisations of a random
#'   vector;
#'     \item \code{new_weights}: a list, each component corresponds to
#'    a different stress and is either a vector of scenario weights or a
#'    function, that applied to the \code{k}th column of \code{x},
#'    generates the vectors of scenario weights;
#'     \item \code{type}: a list, each component corresponds to a
#'    different stress and specifies the type of the stress;
#'     \item \code{h}: a list, each component corresponds to a different stress
#'     and specifies the bandwidth;
#'     \item \code{u}: a list, each component corresponds to a different stress
#'     and is a vector containing the gridspace on [0, 1];
#'     \item \code{lam}: a list, each component corresponds to a different stress
#'     and is vector containing the lambda's of the optimized model;
#'     \item \code{str_fY}: a list, each component corresponds to a different 
#'     stress and is a function defining the densities of the stressed component;
#'     \item \code{str_FY}: a list, each component corresponds to a different 
#'     stress and is a function defining the distribution of the stressed component;
#'     \item \code{str_FY_inv}: a list, each component corresponds to a different 
#'     stress and is a function defining the quantiles of the stressed component;
#'     \item \code{gamma}: a list, each component corresponds to a different 
#'     stress and is a function defining the risk measure;
#'     \item \code{specs}: a list, each component corresponds to
#'   a different stress and contains a list with the specifications
#'   of what has been stressed.
#'   Specifications depend on the \code{type} of stress:
#'     \itemize{
#'       \item \code{type = "RM"}: \code{k}, the column of \code{x}
#'     on which the stress is applied to; \code{alpha}, the level of
#'     the RM; \code{q}, the stressed RM at level
#'       \code{alpha}.
#'       \item \code{type = "mean sd"}: \code{k}, the columns of \code{x}
#'     on which the stress is applied to; \code{new_mean}, the
#'     stressed mean; \code{new_sd}, the stressed standard deviation.
#'       \item \code{type = "RM mean sd"}: \code{k}, the column of \code{x}
#'     on which the stress is applied to; \code{alpha}, the level of the
#'     stressed RM; \code{q}, the stressed RM at level
#'     \code{alpha}; \code{new_mean}, the
#'     stressed mean; \code{new_sd}, the stressed standard deviation.
#'       \item \code{type = "HARA RM"}: \code{k}, the column of \code{x}
#'     on which the stress is applied to; \code{alpha}, the level of
#'     the stressed RM; \code{q}, the stressed RM at level
#'       \code{alpha}; \code{a} a parameter of the HARA utility function;
#'       \code{b}, a parameter of the HARA utility function;
#'       \code{eta} a parameter of the HARA utility function;
#'       \code{hu}, the stressed HARA utility with parameters 
#'       \code{a}, \code{b}, and \code{eta}.
#'     }
#'   }
 #'
 #' @references \insertRef{Pesenti2019reverse}{SWIM}\cr
 #'
 #'     \insertRef{Pesenti2020SSRN}{SWIM}\cr
 #'
 #'     \insertRef{Csiszar1975}{SWIM}
 #'
 #' @seealso See \code{\link{get_data}} for extracting the data,
 #'     \code{x}; \code{\link{get_weights}} for extracting the scenario
 #'     weights, \code{new_weights}; \code{\link{get_weightsfun}} for
 #'     extracting the functions generating the scenario weights; and
 #'     \code{\link{get_specs}} for extracting the specifications of
 #'     the stress on an object of class \code{SWIM}.
 #' @importFrom  Rdpack reprompt
 #'
 #' @docType package
 #' @name SWIM
 NULL
