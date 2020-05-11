## Test environments
- local OS X install, R 3.6.0
- rhub::check_for_cran()
- rhub::check_on_fedora()
- devtools::check_win_release()
- devtools::check_win_devel()
- devtools::check_win_oldrelease()


## R CMD check results
0 ERRORs | 0 WARNINGs | 0 NOTES

- `rhub::check_for_cran()` had one error about "Package suggested but not available", but it looks to us like a configuration issue on their end and not a zoltr package issue:


    E  checking package dependencies
       Package suggested but not available: 'data.table'
       
       The suggested packages are required for a complete check.
       Checking can be attempted without them by setting the environment
       variable _R_CHECK_FORCE_SUGGESTS_ to a false value.
       
       See section 'The DESCRIPTION file' in the 'Writing R Extensions'
       manual.


- `devtools::submit_cran()` had one warning about "CRAN Package Check Results for Package zoltr" , but it looks to us like an OS configuration issue in `r-patched-osx-x86_64` and not a zoltr package issue:

    
    Error(s) in re-building vignettes:
    --- re-building ‘getting-started.Rmd’ using rmarkdown
    dyld: lazy symbol binding failed: Symbol not found: ____chkstk_darwin
    Referenced from: /usr/local/bin/pandoc (which was built for Mac OS X 10.15)
    Expected in: /usr/lib/libSystem.B.dylib


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

