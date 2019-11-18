# SWIM package

The `master` branch corresponds to the [CRAN](https://CRAN.R-project.org/package=SWIM) version. 

## Amendments to the `develop` branch: 

 - allowing that the input `x` of `stress()` can have missing column names.
 - changes to the `stress_VaR()` function:
    * amendment to the calculation of scenario weights for $Y = q$, where $q$ is the stressed VaR. 
    * returns a message if the achieved VaR is not equal to the stressed VaR specified.
    * specs of the `SWIM` object contains the achieved VaR 
    * updated the help function of `stress_VaR()`
    
 - changes to the `stress_VaR_ES()` function:
    * amendment analogous to the `stress_VaR()`.
    * returns a message if the achieved VaR is not equal to the stressed VaR specified.
    * specs of the `SWIM` object contains the achieved VaR 
    * updated the help function of `stress_VaR_ES()`

 - added the function `VaR_stressed()`:
    * The function calculates the VaR of model components, subject to 
      scenario weights.

 - added the function `ES_stressed()`:
    * The function calculates the ES of model components, subject to 
      scenario weights.
      
- added a data set containing aggregate losses from a portfolio,
      generated through a binomial credit model.