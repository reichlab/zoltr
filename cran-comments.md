## Test environments
- local OS X install, R 3.6.0
- rhub::check_for_cran()
  - _windows-x86_64-devel_ (r-devel)
  - _ubuntu-gcc-release_ (r-release)
  - _fedora-clang-devel_ (r-devel)
- rhub::check_on_fedora()
  - _fedora-gcc-devel_ (r-devel)  
- devtools::build_win()


## R CMD check results
0 ERRORs | 0 WARNINGs | 0 NOTES


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

