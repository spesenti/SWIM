# SWIM package

The `master` branch corresponds to the [CRAN](https://CRAN.R-project.org/package=SWIM) version. 

## Amendments to the `develop` branch: 

 - allowing that the input `x` of `stress()` can have missing column names.
 - changes to the `stress_VaR` function:
    * amendment to the calculation of scenario weights for $Y = q$, where $q$ is the stressed VaR. 
    * retuns a message if the achieved VaR is not equal to the stressed VaR specified.
    * specs of the `SWIM` object contains the achived VaR 
