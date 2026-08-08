## R CMD check results

0 errors \| 0 warnings \| 1 note

## Submission

This is a maintenance release (v1.3.1) that:

-   Makes `mcmodule_converg()` CRAN tests reproducible by setting a random seed.

-   Fixes issues in `mc_plot()`, `eval_module()`, `optim_ndvar()`, and `mcmodule_converg()`

-   Deprecates `agg_totals()` in favour of `agg_variates()`. The old function remains available for backwards compatibility.

-   Makes small changes to the sensitivity analysis vignette and README.

See NEWS.md for detailed changes.

## Downstream dependencies

There are currently no downstream dependencies for this package.
