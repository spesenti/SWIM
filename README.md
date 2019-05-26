
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
summary(rev.stress, base = TRUE)
#> $base
#>                        Y           X1           X2           X3
#> mean        4.995671e+02 9.971960e+01 1.001025e+02 9.999800e+01
#> sd          2.347882e+02 7.509175e+01 4.321323e+01 4.897528e+01
#> skewness    9.413087e-03 5.651204e-03 1.012673e-02 6.676749e-03
#> ex kurtosis 2.291973e-02 7.286639e-04 2.291537e-04 2.120437e-02
#> 1st Qu.     3.414990e+02 4.905333e+01 7.107213e+01 6.709921e+01
#> Median      4.994938e+02 9.989109e+01 1.001097e+02 1.000864e+02
#> 3rd Qu.     6.573294e+02 1.499158e+02 1.291574e+02 1.327933e+02
#>                       X4            X5
#> mean        9.976048e+01  99.986505887
#> sd          5.905022e+01  75.260750718
#> skewness    2.754239e-03   0.006504508
#> ex kurtosis 3.095137e-02  -0.009494181
#> 1st Qu.     6.014504e+01  49.341442478
#> Median      9.975391e+01  99.843463264
#> 3rd Qu.     1.393594e+02 151.044259284
#> 
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

Sensitivity measures allow to assess the importance of the input
components.

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

The function importance\_rank returns the ranks of the input components
according to the chosen sensitivity measure, here Gamma.

``` r
importance_rank(rev.stress, xCol = 2:6, type = "Gamma")
#>     stress  type X1 X2 X3 X4 X5
#> 1 stress 1 Gamma  2  5  4  3  1
#> 2 stress 2 Gamma  2  5  4  3  1
```
