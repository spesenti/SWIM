## Re-submission: Version 0.2.2



## Test environments
* ubuntu (on travis-ci)    
* local win (R4.0.2; R Under development)

## R CMD check results
There were no ERRORs, no WARNINGS and no NOTES.    


## CRAN status: WARN: 1, OK: 11
The Warning on the current CRAN version pertains to the bibtex package as follows: 

There is 1 Warning "Requires (indirectly) orphaned package: 'bibtex'". The bibtex 
is an Import for the Rdpack package which I use for citing BibTex entries in the 
help files. The bibtex package has been ORPHAND on CRAN on 19. Sept. 2020 and I don't
know whether this is permanent. 

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
