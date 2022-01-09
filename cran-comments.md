## Re-submission: Version 1.0.0


## Test environments
* ubuntu (on travis-ci)    
* local win (R4.1.2; R Under development)

## R CMD check results
There were no ERRORS, no WARNINGS, and no NOTES.    


## CRAN status: OK: 13

## Downstream dependencies
There are currently no downstream dependencies for this package.



## Changes (copied from NEWS.md file)

### Major changes: Additional functions and features

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

### Minor changes

 - fix minor bug in `summary()`.
 - add `base` argument for `quantile_stressed()` and an error message if the input has `wCol` has dimension larger than 1.
