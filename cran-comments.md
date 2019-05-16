## Note: Initial CRAN release :-)


## Test environments
- local OS X install, R 3.5.3
- R-hub windows-x86_64-devel (r-devel)
- R-hub ubuntu-gcc-release (r-release)
- R-hub fedora-clang-devel (r-devel)


## R CMD check results

    0 errors | 0 warnings | 1 note

- There were no ERRORs nor WARNINGs.
- There was 1 NOTE on windows-x86_64-devel (r-devel), ubuntu-gcc-release (r-release), fedora-clang-devel (r-devel):
  - "Possibly mis-spelled words in DESCRIPTION", but the spelling of "Zoltar" is correct.


## Vignette build requirements
Per your policies [1], this package's vignettes and README are enabled only on the maintainer’s machine because: 1) they
take a long time (more than a few seconds) to run, and 2) they execute destructive database operations and so operate on
a development server (rather than modify our production database). However, the unit tests do exercise all the features
of the package.

[1] https://cran.r-project.org/web/packages/policies.html

    If the package needs special treatment (for example if vignettes can only be run or re-built on the maintainer’s
    machine or take a very long time), say so on the submission form.

    Long-running tests and vignette code can be made optional for checking, but do ensure that the checks that are left
    do exercise all the features of the package.

