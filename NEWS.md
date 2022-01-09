
### SWIM 1.0.0 - current develop version on GitHub

## Major changes: Additional functions and features

 - Wasserstein distance
 
     - `stress_wass()`:
        - A wrapper for the stress functions using the 2-Wasserstein distance
     - `stress_RM_w()`:
        - a stressed model component (random variable) fulfills a 
        constraint on its risk measure defined by a gamma function.
     - `stress_RM_mean_sd_w()`:
        - a stressed model component (random variable) fulfills a 
        constraint on its mean, standard deviation, and risk measure 
        defined by a gamma function.
     - `stress_HARA_RM_w()`:
        - a stressed model component (random variable) fulfills a
        constraint on its HARA utility defined by a, b and eta parameter
        and risk measure defined by a gamma function.
    - `stress_mean_sd_w()`:
        - a stressed model component (random variable) fulfills a
        constraint on its mean and standard deviation.
    - `stress_mean_w()`: 
        - a stressed model component (random variable) fulfills a
        constraint on its mean.
      

 - Functions
   
     - `mean_stressed()`:
        - sample mean of chosen stressed model components, subject to the calculated scenario weights.
     - `sd_stressed()`:
        - sample standard deviation of chosen stressed model components, subject to the calculated scenario weights.
     - `var_stressed()`:
        - sample variance of chosen stressed model components, subject to the calculated scenario weights.
     - `cor_stressed()`:
        - sample correlation coefficient of chosen stressed model components, subject to the calculated scenario weights.
    - `cdf_stressed()`:
        - the empirical distribution function of a stressed model component (random variable) under the scenario weights. 
    - `rename_SWIM()`: 
        - Get a new SWIM object with desired names.
    
    
    
- Features

    - `stress()`:
        - A parameter "names" to all stress functions, which allows to name a stress differently than just "stress 1", "stress 2", etc.
        - A parameter "log" that allows users to inspect weights' statistics, including minimum, maximum, standard deviation, Gini coefficient, and entropy.
    - `sensitivity()`:
        - A parameter "p"  can be specified for the degree of Wasserstein distance.

## Minor changes

 - fix minor bug in `summary()`.
 - add `base` argument for `quantile_stressed()` and an error message if the input has `wCol` has dimension larger than 1.


# SWIM 0.2.2 - current version on CRAN

## Major changes: Additional functions and features

 - `plot_quantile()`:
    * the function plots the empirical quantile of model components, subject to 
      scenario weights.

 - `plot_weights()`:
    * the function plots the scenario weights of a stressed model against 
      model components.

 - `stress_moment()`:
    * add parameter "normalise" that allows to linearly normalise the values
    called by `nleqslv`.
    * the function prints a table with the required and achieved moments and the absolute and relative error.

 - `stress_VaR_ES()`:
    * add parameter "normalise" that allows to linearly normalise the values 
    before `uniroot` is applied.


## Minor changes

 - fix bug in merging different stress objects.


# SWIM 0.2.1 

## Minor changes

- add vignette
- fix bug in `merge()`. 
- fix bug in `sensitivity()`. 


# SWIM 0.2.0

## Major changes

### Additional functions and data sets

 - `VaR_stressed()`:
    * the function calculates the VaR of model components, subject to 
      scenario weights.

 - `ES_stressed()`:
    * the function calculates the ES of model components, subject to 
      scenario weights.
    
 - `credit_data`:
    * a data set containing aggregate losses from a credit portfolio,
      generated through a binomial credit model.

### Amendments to functions

 - `stress_VaR()`:
    * amendment to the calculation of scenario weights when the specified VaR cannot be achieved.
    * returns a message if the achieved VaR is not equal to the stressed VaR specified.
    * specs of the `SWIM` object contains the achieved VaR 
    * allowing for stressing VaR downwards
    
 - `stress_VaR_ES()`:
    * amendment analogous to the `stress_VaR()`.
    * returns a message if the achieved VaR is not equal to the stressed VaR specified.
    * specs of the `SWIM` object contains the achieved VaR 
    * allowing for stressing VaR and ES downwards

## Minor changes

 - `stress()`:   
    * parameter `x` can have missing column names.

 - `stress_moment()`:
    * additional parameter `show`; if `TRUE` (default is `FALSE`), the result of `nleqslv()` is printed.
