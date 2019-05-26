
<!-- README.md is generated from README.Rmd. Please edit that file -->

# SWIM - A Package for Sensitivity Analysis

The SWIM package provides weights on simulated scenarios from a
stochastic model, such that a stressed model components (random
variables) fulfil given probabilistic constraints (e.g. specified values
for risk measures), under the new scenario weights. Scenario weights are
selected by constrained minimisation of the relative entropy to the
baseline model.

## Installation

You can install the SWIM package from
[GitHub](https://github.com/spesenti/SWIM) with:

``` r
install.packages("spesenti/SWIM")
```

## Scope of the SWIM package

Implemented stresses are:

| R functions          | Stress                        |
| -------------------- | ----------------------------- |
| stress\_VaR          | VaR risk measure, a quantile  |
| stress\_VaR\_ES      | VaR and ES risk measures      |
| stress\_mean         | means                         |
| stress\_mean\_sd     | means and standard deviations |
| stress\_mean\_moment | moments, functions of moments |
| stress\_prob         | probabilities of intervals    |
| stress\_user         | user defined scenario weights |

## Example - Stressing the VaR of a portfolio

Consider a portfolio \(Y = X1 + X2 + X3 + X4 + X5\), where
\((X1, X2, X3, X4, X5)\) are correlated normally istributed with equal
mean and different standard deviations.

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
obtained via

``` r
summary(rev.stress)
#> $`stress 1`
#>                        Y           X1           X2           X3
#> mean        533.36520478 108.36772309 104.68278212 105.26961033
#> sd          248.19497187  77.87664779  44.57888172  50.56529934
#> skewness     -0.04558895  -0.02348498  -0.01219461  -0.01705958
#> ex kurtosis  -0.22167094  -0.10825975  -0.07713538  -0.06537690
#> 1st Qu.     360.39775266  55.14130134  74.41277663  70.96625000
#> Median      531.57771604 108.68355052 104.91061361 105.41896036
#> 3rd Qu.     723.05569558 161.82081068 134.92261543 139.70171951
#>                       X4           X5
#> mean        106.31083139 108.73425786
#> sd           61.09042461  78.08941029
#> skewness     -0.02195459  -0.02524098
#> ex kurtosis  -0.06549873  -0.12108323
#> 1st Qu.      64.80504721  55.33221323
#> Median      106.36340400 108.82501880
#> 3rd Qu.     148.14389905 162.94570719
#> 
#> $`stress 2`
#>                       Y           X1           X2           X3
#> mean        523.5505116 105.88479223 103.36187107 103.75487216
#> sd          252.4533551  78.80033237  45.02809980  51.08702874
#> skewness      0.1060748   0.06017829   0.05422709   0.05154481
#> ex kurtosis  -0.1155349  -0.06556281  -0.04465029  -0.03223813
#> 1st Qu.     350.8794256  52.01502589  72.80444977  69.04535434
#> Median      514.8024011 104.84378999 102.89127592 103.18199039
#> 3rd Qu.     688.2595214 158.36987599 133.37177634 137.87116212
#>                       X4           X5
#> mean        104.39352013 106.15545601
#> sd           61.71412419  78.94687239
#> skewness      0.05174033   0.05919795
#> ex kurtosis  -0.02483972  -0.07222519
#> 1st Qu.      62.50419349  52.29202549
#> Median      103.45451855 104.69115882
#> 3rd Qu.     145.76720810 159.31728093
```

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r
summary(cars)
#>      speed           dist       
#>  Min.   : 4.0   Min.   :  2.00  
#>  1st Qu.:12.0   1st Qu.: 26.00  
#>  Median :15.0   Median : 36.00  
#>  Mean   :15.4   Mean   : 42.98  
#>  3rd Qu.:19.0   3rd Qu.: 56.00  
#>  Max.   :25.0   Max.   :120.00
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date.

You can also embed plots, for example:

<img src="man/figures/README-pressure-1.png" width="100%" />

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub\!
