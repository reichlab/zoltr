## Test environments
- local OS X install, R 3.6.0
- rhub::check_for_cran()
- rhub::check_on_fedora()
- devtools::check_win_release()
- devtools::check_win_devel()
- devtools::check_win_oldrelease()


## R CMD check results
0 ERRORs | 0 WARNINGs | 1 NOTES

- `rhub::check_for_cran()` had one note under "checking CRAN incoming feasibility" about "Days since last update". We are submitting again to bring compatibility up to our server's new 3.1.0 release. We apologize for the short time between submissions.


    N  checking CRAN incoming feasibility
       Maintainer: 'Matthew Cornell <cornell@umass.edu>'
       Days since last update: 4


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

