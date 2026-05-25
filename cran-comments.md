## R CMD check results

0 errors | 0 warnings | 1 note

## Submission

This is a minor release (v1.3.0) that:

* New sensitivity analysis workflows using sampling designs (Morris, Sobol), 
  with new helpers `mctable_bounds()`, `mctable_sobol_matrices()`, and `set_sampling_design()`.
  
* Most functions have been updated to support nodes created from sampling designs (`eval_module()`, 
  `mc_keys()`, `mc_match()`, `mc_match_data()`, `trial_totals()`).

* Added tornado plot for correlation analysis (`mcmodule_tornado()`), an experimental 
  optimiser for uncertainty convergence (`optim_ndvar()`), and `mcnode_null_rm()` for 
  handling absent nodes.

* Fixes multiple bugs

* Extends documentation, adding a sensitivity analysis vignette.

See NEWS.md for detailed changes.

## Downstream dependencies

There are currently no downstream dependencies for this package.
