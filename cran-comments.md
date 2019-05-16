## Note: Initial CRAN release :-)


## Test environments
- local OS X install, R version 3.5.3
  - todo xx "" R-devel?
- devtools::build_win()
xx


## Vignette build requirements
Per your policies [1], this package's vignettes and README are enabled only on the maintainer’s machine because 1) they
take a long time (more than a few seconds) to run, and 2) they execute destructive database operations and so operate on
a development server (rather than modify our production database). However, the unit tests do exercise all the features
of the package.

[1] https://cran.r-project.org/web/packages/policies.html

    If the package needs special treatment (for example if vignettes can only be run or re-built on the maintainer’s
    machine or take a very long time), say so on the submission form.

    Long-running tests and vignette code can be made optional for checking, but do ensure that the checks that are left
    do exercise all the features of the package.


## R CMD check results
0 errors | 0 warnings | 0 notes

