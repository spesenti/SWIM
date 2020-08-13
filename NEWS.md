
# SWIM 0.2.1.9000 - develop version

## Major changes: Additional functions and features

 - `plot_quantile()`:
    * the function plots the empirical quantile of model components, subject to 
      scenario weights.

 - `plot_weights()`:
    * the function plots the scenario weights of a stressed model agains 
      model components.

 - `stress_moment()`:
    * add parameter "normalise" that allows to linearly normalise the values
    called by `nleqslv`.


## Minor changes

 - fix bug in merging different stress objects.


# SWIM 0.2.1 - current version on CRAN

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
