## Note: Initial CRAN release :-)


## Test environments
- local OS X install, R 3.5.3
- todo xx devtools win-builder, R 3.4.0 beta (r72499)
- todo xx travis.ci Linux, x64, R 3.5.0 unstable (r72992)


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
- There were no ERRORs nor WARNINGs.
- Local build generated no NOTEs.
- Win-builder generated 1 NOTE:
  - "Possibly mis-spelled words in DESCRIPTION", but the spelling of "Zoltar" is correct.
  

0 errors | 0 warnings | 0 notes


## Downstream dependencies
Reverse dependencies checked with devtools::revdep_check(). No issues found.

