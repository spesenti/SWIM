
<!-- README.md is generated from README.Rmd. Please edit that file -->

# SWIM - An R Package for Sensitivity Analysis

The SWIM package provides weights on simulated scenarios from a
stochastic model, such that stressed model components (random variables)
fulfil given probabilistic constraints (e.g. specified values for risk
measures), under the new scenario weights. Scenario weights are selected
by constrained minimisation of the relative entropy to the baseline
model.

## Installation

You can install the SWIM package from
[GitHub](https://github.com/spesenti/SWIM) with:

``` r
install.packages("spesenti/SWIM")
```

## Scope of the SWIM package

The SWIM package provides sensitivity analysis tools for stressing model
components (random variables). Implemented stresses are:

| R functions          | Stress                        |
| -------------------- | ----------------------------- |
| stress\_VaR          | VaR risk measure, a quantile  |
| stress\_VaR\_ES      | VaR and ES risk measures      |
| stress\_mean         | means                         |
| stress\_mean\_sd     | means and standard deviations |
| stress\_mean\_moment | moments, functions of moments |
| stress\_prob         | probabilities of intervals    |
| stress\_user         | user defined scenario weights |

Implemented functions allow to graphically display the change in the
probability distributions under different stresses and the baseline
model as well as calculating sensitivity measures.

## Example - Stressing the VaR of a portfolio

Consider a portfolio Y = X1 + X2 + X3 + X4 + X5, where (X1, X2, X3, X4,
X5) are correlated normally distributed with equal mean and different
standard deviations. We stress the VaR (quantile) of the portfolio loss
Y at levels 0.75 and 0.9 with an increase of 10%.

``` r
set.seed(0)
SD <- round(runif(5, 30, 80))
Corr <- matrix(rep(0.5, 5^2), nrow = 5) + diag(rep(1 - 0.5, 5))
if (!requireNamespace("mvtnorm", quietly = TRUE))
   stop("Package \"mvtnorm\" needed for this function 
   to work. Please install it.")
x <- mvtnorm::rmvnorm(10^5, 
   mean =  rep(100, 5), 
   sigma = (SD %*% t(SD)) * Corr)
data <- data.frame(rowSums(x), x)
names(data) <- c("Y", "X1", "X2", "X3", "X4", "X5")
rev.stress <- stress(type = "VaR", x = data, 
   alpha = c(0.75, 0.9), q_ratio = 1.1, k = 1)
```

Summary statistics of the baseline and the stressed model can be
obtained via the summary method.

``` r
summary(rev.stress, base = TRUE)
#> $base
#>                  Y     X1     X2     X3     X4     X5
#> mean        499.57  99.72 100.10 100.00  99.76  99.99
#> sd          234.79  75.09  43.21  48.98  59.05  75.26
#> skewness      0.01   0.01   0.01   0.01   0.00   0.01
#> ex kurtosis   0.02   0.00   0.00   0.02   0.03  -0.01
#> 1st Qu.     341.50  49.05  71.07  67.10  60.15  49.34
#> Median      499.49  99.89 100.11 100.09  99.75  99.84
#> 3rd Qu.     657.33 149.92 129.16 132.79 139.36 151.04
#> 
#> $`stress 1`
#>                  Y     X1     X2     X3     X4     X5
#> mean        533.37 108.37 104.68 105.27 106.31 108.73
#> sd          248.19  77.88  44.58  50.57  61.09  78.09
#> skewness     -0.05  -0.02  -0.01  -0.02  -0.02  -0.03
#> ex kurtosis  -0.22  -0.11  -0.08  -0.07  -0.07  -0.12
#> 1st Qu.     360.40  55.14  74.41  70.97  64.81  55.33
#> Median      531.58 108.68 104.91 105.42 106.36 108.83
#> 3rd Qu.     723.06 161.82 134.92 139.70 148.14 162.95
#> 
#> $`stress 2`
#>                  Y     X1     X2     X3     X4     X5
#> mean        523.55 105.88 103.36 103.75 104.39 106.16
#> sd          252.45  78.80  45.03  51.09  61.71  78.95
#> skewness      0.11   0.06   0.05   0.05   0.05   0.06
#> ex kurtosis  -0.12  -0.07  -0.04  -0.03  -0.02  -0.07
#> 1st Qu.     350.88  52.02  72.80  69.05  62.50  52.29
#> Median      514.80 104.84 102.89 103.18 103.45 104.69
#> 3rd Qu.     688.26 158.37 133.37 137.87 145.77 159.32
```

Visual display of the change of empirical distribution functions of the
portfolio loss Y from the baseline to the two stressed models.

``` r
plot_cdf(object = rev.stress, xCol = , base = TRUE)
```

<img src="man/figures/README-plot-cdf-1.png" width="100%" />

### Sensitivity and importance rank of portfolio components

Sensitivity measures allow to assess the importance of the input
components. Implemented sensitivity measures are the Kolmogorov
distance, the Wasserstein distance and *Gamma*. *Gamma*, the *Reverse
Sensitivity Measure*, defined for model component Xi, i = 1, …, 5, and
scenario weights w by

*Gamma* = ( E(Xi \* w) - E(Xi) ) / c,

where c is a normalisation constant such that |*Gamma*| \<= 1, see
<https://doi.org/10.1016/j.ejor.2018.10.003>. Loosely speaking, the
Reverse Sensitivity Measure is the normalised difference between the
first moment of the stressed and the baseline distributions of Xi.

``` r
sensitivity(rev.stress, type = "all")
#>     stress        type            Y           X1           X2           X3
#> 1 stress 1       Gamma 1.0000000000 0.7996194290 0.7361314472 0.7468647948
#> 2 stress 2       Gamma 1.0000000000 0.8030965565 0.7369764204 0.7503054191
#> 3 stress 1  Kolmogorov 0.0003846520 0.0003846520 0.0003846520 0.0003846520
#> 4 stress 2  Kolmogorov 0.0004879139 0.0004879139 0.0004879139 0.0004879139
#> 5 stress 1 Wasserstein 0.1225813694 0.0386157515 0.0227766091 0.0251783327
#> 6 stress 2 Wasserstein 0.2249939549 0.0715281097 0.0415352694 0.0467140148
#>             X4           X5
#> 1 0.7706528836 0.8076592401
#> 2 0.7681391298 0.8042831834
#> 3 0.0003846520 0.0003846520
#> 4 0.0004879139 0.0004879139
#> 5 0.0306371941 0.0390024561
#> 6 0.0566780366 0.0719661249
plot_sensitivity(rev.stress, xCol = 2:6, type = "Gamma") 
```

<img src="man/figures/README-sensitivity-1.png" width="100%" />

Sensitivity to all sub-portfolios, (X1 + X2), (X1 + X3), … (X4,
X5):

``` r
importance_rank(rev.stress, xCol = NULL, wCol = 1, type = "Gamma", f = rep(list(function(x)x[1] + x[2]), 10), 
   k = list(c(2,3), c(2,4), c(2,5), c(2,6), c(3,4), c(3,5), c(3,6), c(4,5), c(4,6), c(5,6)))
#>     stress  type f1 f2 f3 f4 f5 f6 f7 f8 f9 f10
#> 1 stress 1 Gamma  7  5  3  1 10  9  6  8  4   2
```

Ranking the input components according to the chosen sensitivity
measure, in this example using *Gamma*.

``` r
importance_rank(rev.stress, xCol = 2:6, type = "Gamma")
#>     stress  type X1 X2 X3 X4 X5
#> 1 stress 1 Gamma  2  5  4  3  1
#> 2 stress 2 Gamma  2  5  4  3  1
```

Visual display of the change of empirical distribution functions and
density from the baseline to the two stressed models of X5, the
portfolio component with the largest sensitivity. Stressing the
portfolio loss Y, results in a distribution function of X5 that has a
heavier tail.

``` r
plot_cdf(object = rev.stress, xCol = 5, base = TRUE)
```

<img src="man/figures/README-plot-cdf-input-1.png" width="100%" />

``` r
plot_hist(object = rev.stress, xCol = 5, base = TRUE)
```

<img src="man/figures/README-plot-cdf-input-2.png" width="100%" />
