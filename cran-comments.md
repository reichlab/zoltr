## Note: Initial CRAN release :-)


## Resubmission
This is a resubmission. In this version we have:
- Omitted the redundant "R" in title and description.
- Changed package names, software names and API names to be in single quotes in the title and description fields.
- Lengthened and elaborated the description field to be a (one paragraph) summary of what the package does and why
  it may be useful.
- Provided small executable examples in all exported functions' Rd files. Examples have been wrapped in \dontrun{} for
  the reasons explained in "Vignette build requirements" below.


## Test environments
- local OS X install, R 3.5.3
- R-hub windows-x86_64-devel (r-devel)
- R-hub ubuntu-gcc-release (r-release)
- R-hub fedora-clang-devel (r-devel)


## R CMD check results
- There were no ERRORs nor WARNINGs.
- There was 1 NOTE on windows-x86_64-devel (r-devel), ubuntu-gcc-release (r-release), fedora-clang-devel (r-devel):
    checking CRAN incoming feasibility ... NOTE
    Maintainer: 'Matthew Cornell <cornell@umass.edu>'
    New submission


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

