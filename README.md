
<!-- README.md is generated from README.Rmd. Please edit that file -->

# SWIM - A Package for Sensitivity Analysis

The SWIM package provides weights on simulated scenarios from a
stochastic model, such that stressed random variables fulfil given
probabilistic constraints (e.g. specified values for risk measures),
under the new scenario weights. Scenario weights are selected by
constrained minimisation of the relative entropy to the baseline model.

## Installation

You can install the SWIM package from
[CRAN](https://github.com/spesenti/SWIM) with:

``` r
install.packages("spesenti/SWIM")
```

## Scope of the SWIM package

Implemented stresses are:

| R function           | Stress                        |
| -------------------- | ----------------------------- |
| stress\_VaR          | VaR risk measure, a quantile  |
| stress\_VaR\_ES      | VaR and ES risk measures      |
| stress\_mean         | means                         |
| stress\_mean\_sd     | means and standard deviations |
| stress\_mean\_moment | moments, functions of moments |
| stress\_prob         | probabilities of intervals    |
| stress\_user         | user defined scenario weights |

## 

## Example

This is a basic example which shows you how to solve a common problem:

``` r
## basic example code
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
