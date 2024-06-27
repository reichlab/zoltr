# Test environments

## Local development (Mac)

Versions: macOS Sonoma Version 14.5 (23F79), R version 4.4.1 (2024-06-14)

Result: 0 errors ✔ | 0 warnings ✔ | 0 notes ✔

## R-hub v2

Ran `rhub::rhub_check()` on these platforms:

- 1 🖥 linux R-* (any version)    ubuntu-latest on GitHub
- 4 🖥 windows R-* (any version)  windows-latest on GitHub

Result: no errors, warnings, or notes in the repo's [GitHub Action](https://github.com/reichlab/zoltr/actions) artifacts

# Vignette build requirements

Per your policies [1], this package's vignettes and README are enabled only on the maintainer’s machine because: 1) they take a long time (more than a few seconds) to run, and 2) they execute destructive database operations and so operate on a development server (rather than modify our production database). However, the unit tests do exercise all the features of the package.

[1] https://cran.r-project.org/web/packages/policies.html

    If the package needs special treatment (for example if vignettes can only be run or re-built on the maintainer’s
    machine or take a very long time), say so on the submission form.

    Long-running tests and vignette code can be made optional for checking, but do ensure that the checks that are left
    do exercise all the features of the package.

