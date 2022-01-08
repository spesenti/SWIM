<table>
<tbody>
<tr class="odd">
<td># author: Silvana M. Pesenti</td>
</tr>
<tr class="even">
<td># date: June 01, 2019</td>
</tr>
<tr class="odd">
<td>output:</td>
</tr>
<tr class="even">
<td>rmarkdown::html_vignette:</td>
</tr>
<tr class="odd">
<td>md_extensions: [</td>
</tr>
<tr class="even">
<td>“-autolink_bare_uris”</td>
</tr>
<tr class="odd">
<td>]</td>
</tr>
</tbody>
</table>

<!-- README.md is generated from README.Rmd. Please edit that file -->

# SWIM - A Package for Sensitivity Analysis

[![Travis-CI Build
Status](https://travis-ci.org/spesenti/SWIM.svg?branch=master)](https://travis-ci.org/spesenti/SWIM)
[![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version/SWIM)](https://cran.r-project.org/package=SWIM)
[![downloads](https://cranlogs.r-pkg.org/badges/grand-total/SWIM)](https://cran.r-project.org/package=SWIM)

The SWIM package provides weights on simulated scenarios from a
stochastic model, such that stressed model components (random variables)
fulfil given probabilistic constraints (e.g. specified values for risk
measures), under the new scenario weights. Scenario weights are selected
by constrained minimisation of the relative entropy to the baseline
model. The SWIM package is based on the papers Pesenti S.M, Millossovich
P., Tsanakas A. (2019) [“Reverse Sensitivity Testing: What does it take
to break the model”](https://openaccess.city.ac.uk/id/eprint/18896/) and
and Pesenti S.M. (2021) [“Reverse Sensitivity Analysis for Risk
Modelling”](https://www.ssrn.com/abstract=3878879).

## Vignette

The Vignette of the SWIM package is available in html format
utstat.toronto.edu/pesenti/SWIMVignette/ and as pdf
<https://openaccess.city.ac.uk/id/eprint/25845/>.

## Installation

The SWIM package can be installed from
[CRAN](https://CRAN.R-project.org/package=SWIM) :

> <https://CRAN.R-project.org/package=SWIM>;

alternatively from [GitHub](https://github.com/spesenti/SWIM):

> <https://github.com/spesenti/SWIM>

## Scope of the SWIM package

The SWIM package provides sensitivity analysis tools for stressing model
components (random variables). Implemented stresses using the relative
Entropy (Kullback-Leibler divergence) are:

<table>
<thead>
<tr class="header">
<th>R functions</th>
<th>Stress</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td><code>stress()</code></td>
<td>A wrapper for the <code>stress_</code> functions</td>
</tr>
<tr class="even">
<td><code>stress_VaR()</code></td>
<td>VaR risk measure, a quantile</td>
</tr>
<tr class="odd">
<td><code>stress_VaR()</code></td>
<td>VaR risk measure, a quantile</td>
</tr>
<tr class="even">
<td><code>stress_VaR_ES()</code></td>
<td>VaR and ES risk measures</td>
</tr>
<tr class="odd">
<td><code>stress_mean()</code></td>
<td>means</td>
</tr>
<tr class="even">
<td><code>stress_mean_sd()</code></td>
<td>means and standard deviations</td>
</tr>
<tr class="odd">
<td><code>stress_moment()</code></td>
<td>moments, functions of moments</td>
</tr>
<tr class="even">
<td><code>stress_prob()</code></td>
<td>probabilities of intervals</td>
</tr>
<tr class="odd">
<td><code>stress_user()</code></td>
<td>user defined scenario weights</td>
</tr>
</tbody>
</table>

Implemented stresses using the 2-Wasserstein distance are:

<table>
<thead>
<tr class="header">
<th>R functions</th>
<th>Stress</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td><code>stress_wass()</code></td>
<td>A wrapper for the <code>stress_w</code> functions</td>
</tr>
<tr class="even">
<td><code>stress_RM_w()</code></td>
<td>Risk measure</td>
</tr>
<tr class="odd">
<td><code>stress_RM_mean_sd_w()</code></td>
<td>Risk measure, means and standard deviations</td>
</tr>
<tr class="even">
<td><code>stress_HARA_RM_w()</code></td>
<td>Risk measure and HARA utility</td>
</tr>
<tr class="odd">
<td><code>stress_mean_sd_w()</code></td>
<td>means and standard deviations</td>
</tr>
<tr class="even">
<td><code>stress_mean_w()</code></td>
<td>means</td>
</tr>
</tbody>
</table>

Implemented functions allow to graphically display the change in the
probability distributions under different stresses and the baseline
model as well as calculating sensitivity measures.

## Example - Stressing the VaR of a portfolio

Consider a portfolio Y = X1 + X2 + X3 + X4 + X5, where (X1, X2, X3, X4,
X5) are correlated normally distributed with equal mean and different
standard deviations. We stress the VaR (quantile) of the portfolio loss
Y at levels 0.75 and 0.9 with an increase of 10%.

     # simulating the portfolio 
    library(SWIM)
    set.seed(0)
    SD <- c(70, 45, 50, 60, 75)
    Corr <- matrix(rep(0.5, 5^2), nrow = 5) + diag(rep(1 - 0.5, 5))
    x <- mvtnorm::rmvnorm(10^5, 
       mean =  rep(100, 5), 
       sigma = (SD %*% t(SD)) * Corr)
    data <- data.frame(rowSums(x), x)
    names(data) <- c("Y", "X1", "X2", "X3", "X4", "X5")
     # stressing the portfolio 
    rev.stress <- stress(type = "VaR", x = data, 
       alpha = c(0.75, 0.9), q_ratio = 1.1, k = 1)
    #> Stressed VaR specified was 722.9387 , stressed VaR achieved is 722.9378
    #> Stressed VaR specified was 878.859 , stressed VaR achieved is 878.8296

Summary statistics of the baseline and the stressed model can be
obtained via the `summary()` method.

    #> $base
    #> 
    #> 
    #> |            |      Y|     X1|     X2|     X3|     X4|     X5|
    #> |:-----------|------:|------:|------:|------:|------:|------:|
    #> |mean        | 500.18| 100.14| 100.01|  99.93|  99.98| 100.13|
    #> |sd          | 232.13|  69.79|  44.93|  49.83|  59.85|  74.76|
    #> |skewness    |   0.00|  -0.01|   0.00|   0.01|  -0.01|   0.01|
    #> |ex kurtosis |  -0.04|  -0.03|  -0.02|  -0.01|  -0.02|   0.00|
    #> |1st Qu.     | 342.56|  53.24|  69.70|  66.23|  59.45|  49.72|
    #> |Median      | 500.45| 100.16| 100.06| 100.11| 100.23|  99.94|
    #> |3rd Qu.     | 657.22| 147.33| 130.31| 133.51| 140.33| 150.80|
    #> 
    #> $`stress 1`
    #> 
    #> 
    #> |            |      Y|     X1|     X2|     X3|     X4|     X5|
    #> |:-----------|------:|------:|------:|------:|------:|------:|
    #> |mean        | 534.03| 108.20| 104.85| 105.38| 106.70| 108.89|
    #> |sd          | 245.47|  72.32|  46.35|  51.48|  61.90|  77.60|
    #> |skewness    |  -0.06|  -0.04|  -0.03|  -0.02|  -0.04|  -0.02|
    #> |ex kurtosis |  -0.29|  -0.13|  -0.10|  -0.10|  -0.12|  -0.11|
    #> |1st Qu.     | 361.73|  58.81|  73.38|  70.21|  64.41|  55.97|
    #> |Median      | 532.15| 108.59| 105.08| 105.65| 107.33| 109.02|
    #> |3rd Qu.     | 722.94| 158.24| 136.71| 140.66| 149.34| 162.58|
    #> 
    #> $`stress 2`
    #> 
    #> 
    #> |            |      Y|     X1|     X2|     X3|     X4|     X5|
    #> |:-----------|------:|------:|------:|------:|------:|------:|
    #> |mean        | 524.20| 105.87| 103.44| 103.82| 104.75| 106.33|
    #> |sd          | 249.61|  73.14|  46.81|  52.01|  62.57|  78.46|
    #> |skewness    |   0.09|   0.04|   0.04|   0.05|   0.04|   0.06|
    #> |ex kurtosis |  -0.19|  -0.09|  -0.06|  -0.06|  -0.08|  -0.07|
    #> |1st Qu.     | 352.14|  55.99|  71.60|  68.34|  62.00|  52.93|
    #> |Median      | 516.03| 104.72| 102.95| 103.26| 104.12| 104.94|
    #> |3rd Qu.     | 687.58| 155.33| 134.95| 138.82| 146.79| 159.24|

Visual display of the change of empirical distribution functions of the
portfolio loss Y from the baseline to the two stressed models.

    library(spatstat)
    plot_cdf(object = rev.stress, xCol = 1, base = TRUE)

<img src="man/figures/README-plot-cdf-1.png" width="100%" />

### Sensitivity and importance rank of portfolio components

Sensitivity measures allow to assess the importance of the input
components. Implemented sensitivity measures are the Kolmogorov
distance, the Wasserstein distance and *Gamma*. *Gamma*, the *Reverse
Sensitivity Measure*, defined for model component Xi, i = 1, …, 5, and
scenario weights w by

*Gamma* = ( E(Xi \* w) - E(Xi) ) / c,

where c is a normalisation constant such that |*Gamma*| &lt;= 1, see
<https://doi.org/10.1016/j.ejor.2018.10.003>. Loosely speaking, the
Reverse Sensitivity Measure is the normalised difference between the
first moment of the stressed and the baseline distributions of Xi.

    knitr::kable(sensitivity(rev.stress, type = "all"), digits = 2)
    #> Warning in sensitivity(rev.stress, type = "all"): No s passed in. Using Gamma sensitivity instead.

<table>
<thead>
<tr class="header">
<th style="text-align: left;">stress</th>
<th style="text-align: left;">type</th>
<th style="text-align: right;">Y</th>
<th style="text-align: right;">X1</th>
<th style="text-align: right;">X2</th>
<th style="text-align: right;">X3</th>
<th style="text-align: right;">X4</th>
<th style="text-align: right;">X5</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">stress 1</td>
<td style="text-align: left;">Gamma</td>
<td style="text-align: right;">1.00</td>
<td style="text-align: right;">0.79</td>
<td style="text-align: right;">0.74</td>
<td style="text-align: right;">0.75</td>
<td style="text-align: right;">0.77</td>
<td style="text-align: right;">0.81</td>
</tr>
<tr class="even">
<td style="text-align: left;">stress 2</td>
<td style="text-align: left;">Gamma</td>
<td style="text-align: right;">1.00</td>
<td style="text-align: right;">0.79</td>
<td style="text-align: right;">0.74</td>
<td style="text-align: right;">0.75</td>
<td style="text-align: right;">0.77</td>
<td style="text-align: right;">0.80</td>
</tr>
<tr class="odd">
<td style="text-align: left;">stress 1</td>
<td style="text-align: left;">Kolmogorov</td>
<td style="text-align: right;">0.08</td>
<td style="text-align: right;">0.05</td>
<td style="text-align: right;">0.05</td>
<td style="text-align: right;">0.05</td>
<td style="text-align: right;">0.05</td>
<td style="text-align: right;">0.05</td>
</tr>
<tr class="even">
<td style="text-align: left;">stress 2</td>
<td style="text-align: left;">Kolmogorov</td>
<td style="text-align: right;">0.05</td>
<td style="text-align: right;">0.03</td>
<td style="text-align: right;">0.03</td>
<td style="text-align: right;">0.03</td>
<td style="text-align: right;">0.03</td>
<td style="text-align: right;">0.03</td>
</tr>
<tr class="odd">
<td style="text-align: left;">stress 1</td>
<td style="text-align: left;">Wasserstein p = 1</td>
<td style="text-align: right;">33.84</td>
<td style="text-align: right;">8.07</td>
<td style="text-align: right;">4.84</td>
<td style="text-align: right;">5.45</td>
<td style="text-align: right;">6.72</td>
<td style="text-align: right;">8.77</td>
</tr>
<tr class="even">
<td style="text-align: left;">stress 2</td>
<td style="text-align: left;">Wasserstein p = 1</td>
<td style="text-align: right;">24.02</td>
<td style="text-align: right;">5.73</td>
<td style="text-align: right;">3.43</td>
<td style="text-align: right;">3.88</td>
<td style="text-align: right;">4.77</td>
<td style="text-align: right;">6.21</td>
</tr>
<tr class="odd">
<td style="text-align: left;">stress 1</td>
<td style="text-align: left;">Reverse</td>
<td style="text-align: right;">1.00</td>
<td style="text-align: right;">0.79</td>
<td style="text-align: right;">0.74</td>
<td style="text-align: right;">0.75</td>
<td style="text-align: right;">0.77</td>
<td style="text-align: right;">0.81</td>
</tr>
<tr class="even">
<td style="text-align: left;">stress 2</td>
<td style="text-align: left;">Reverse</td>
<td style="text-align: right;">1.00</td>
<td style="text-align: right;">0.79</td>
<td style="text-align: right;">0.74</td>
<td style="text-align: right;">0.75</td>
<td style="text-align: right;">0.77</td>
<td style="text-align: right;">0.80</td>
</tr>
</tbody>
</table>

    plot_sensitivity(rev.stress, xCol = 2:6, type = "Gamma") 

<img src="man/figures/README-sensitivity-1.png" width="100%" />

Sensitivity to all sub-portfolios, (Xi + Xj), i,j = 1, …, 6:

     # sub-portfolios
    f <- rep(list(function(x)x[1] + x[2]), 10)
    k <- list(c(2, 3), c(2, 4), c(2, 5), c(2, 6), c(3, 4), c(3, 5), c(3, 6), c(4, 5), c(4, 6), c(5, 6))
    importance_rank(rev.stress, xCol = NULL, wCol = 1, type = "Gamma", f = f, k = k)
    #>     stress  type f1 f2 f3 f4 f5 f6 f7 f8 f9 f10
    #> 1 stress 1 Gamma  7  6  3  1 10  9  5  8  4   2

Ranking the input components according to the chosen sensitivity
measure, in this example using *Gamma*.

    importance_rank(rev.stress, xCol = 2:6, type = "Gamma")
    #>     stress  type X1 X2 X3 X4 X5
    #> 1 stress 1 Gamma  2  5  4  3  1
    #> 2 stress 2 Gamma  2  5  4  3  1

Visual display of the change of empirical distribution functions and
density from the baseline to the two stressed models of X5, the
portfolio component with the largest sensitivity. Stressing the
portfolio loss Y, results in a distribution function of X5 that has a
heavier tail.

    library(spatstat)
    plot_cdf(object = rev.stress, xCol = 5, base = TRUE)

<img src="man/figures/README-plot-cdf-input-1.png" width="100%" />

    plot_hist(object = rev.stress, xCol = 5, base = TRUE)

<img src="man/figures/README-plot-cdf-input-2.png" width="100%" />
