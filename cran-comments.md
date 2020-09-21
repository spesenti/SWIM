## Re-submission: Version 0.2.2



## Test environments
* ubuntu (on travis-ci)    
* local win (R4.0.2; R Under development)

## R CMD check results
There were no ERRORs, no WARNINGs, and no NoOTES.

## Downstream dependencies
There are currently no downstream dependencies for this package.



## Changes (copied from NEWS.md file)

### Major changes: Additional functions and features

 - `plot_quantile()`:
    * the function plots the empirical quantile of model components, subject to 
      scenario weights.

 - `plot_weights()`:
    * the function plots the scenario weights of a stressed model agains 
      model components.

 - `stress_moment()`:
    * add parameter "normalise" that allows to linearly normalise the values
    called by `nleqslv`.
    * the function prints a table with the required and achieved moments and the absolute and relative error.

 - `stress_VaR_ES()`:
    * add parameter "normalise" that allows to linearly normalise the values 
    before `uniroot` is applied.


### Minor changes

 - fix bug in merging different stress objects.
