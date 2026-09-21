# mcmodule (development version)

## New features

* `agg_variates()` now accepts custom aggregation functions through `agg_func`. It also warns when automatic probability aggregation is applied to values greater than one (#98).

* `eval_module()` now accepts a list of previous `mcmodule` objects, allowing an expression to use nodes from multiple modules. Conflicting node names must be resolved beforehand, for example with `add_prefix()`.

* `create_mcnodes()` now supports namespace-qualified distribution functions and functions with required arguments. Bare `rpert` now resolves explicitly to `mc2d::rpert()`, with a warning when it is masked by the freedom package (#45).

* `mc_network()`, `visNetwork_nodes()`, and `get_node_table()` gain a `percentages` argument. Count and time nodes, and stochastic nodes with values exceeding one, are displayed numerically, percentage formatting can also be disabled entirely (#41).

* Added `data-raw/water_example.R`, which generates the water-supply quantitative microbial risk assessment example data, node specifications, data keys, and pathway expressions.

## Bug fixes and improvements

* `at_least_one()` now combines nodes directly when every input comes from `sample_design`, avoiding unnecessary key matching (#96).

* `create_mcnodes()` now constructs `mcdata` and `mcstoc` nodes directly instead of generating and parsing R code. Configured transformation expressions continue to be evaluated as R code.

* `eval_module()` now reports clearer errors for unnamed or duplicated expression blocks and unsupported expression statements.

* Input nodes that match columns in multiple datasets now produce an informative error instead of being assigned to a dataset implicitly.

* `mc_filter()` and `mc_compare()` now use `name` exactly as supplied. `suffix` is appended only when a name is generated automatically from `mc_name` (#93).

* `mc_network()` now provides clearer legend labels and node tooltips and omits duplicated parameter and dependency information (#41).

* `mc_summary()` now applies `digits` and `sep_keys` consistently when returning stored summaries.

* `mc_summary()` now returns summaries with sequential row names (#94).

* `trial_totals()` now creates non-sampled input nodes from `mctable` or module data when `sample_design` is supplied (#95).

* Added `data-raw/imports_example.R` to document and standardise generation of the existing imports example datasets and related package objects.

# mcmodule 1.3.1

## Lifecycle

* `agg_totals()` is now deprecated in favour of `agg_variates()`.
  `agg_totals()` remains available for backwards compatibility, but now
  issues a lifecycle warning.

## Bug fixes

* `mc_plot()` now draws boxplots above scatter points (#86).

* `get_node_list()` now uses `node_name` instead of `param_names` when filtering `in_nodes` (#84).

* `optim_ndvar()` no longer produces warnings related to empty mctable handling
  (#87).

* `mcmodule_converg()` tests are now reproducible on CRAN. Tests for
  non-convergence now set a random seed, because false negatives are expected
  when using a very limited number of simulations (#87).

## Documentation

* Updated the sensitivity analysis vignette and added references.

* Updated README installation instructions to recommend `pak::pak()` instead
  of `devtools::install_github()`.

# mcmodule 1.3.0

## New features

* Added support for sensitivity-analysis workflows based on sampling designs 
  (Morris and Sobol), including new helpers `mctable_bounds()` and `mctable_sobol_matrices()` (#49).

* `eval_module()` now supports creating modules using a sampling design (#49).

* Added `set_sampling_design()` to register a sampling design for use across 
  the workflow (#49).

* Other package functions (`mc_keys()`, `mc_match()`, `mc_match_data()`, 
  `trial_totals()`) adapted to handle nodes created from sampling designs (#49).

* Added a tornado plot for correlation analysis via `mcmodule_tornado()` (#49).

* Added `mcnode_null_rm()` to replace absent nodes by an specific value (similar to 
  `mcnode_na_rm()`) (#66).

* Added a experimental function to optimize the number of iterations needed for uncertainty 
  convergence `optim_ndvar()` (#82).

* Improved convergence diagnostics in `mcmodule_converg()`, including clearer 
  summaries and reporting of non-converged nodes (#49).

* Improved  `mcmodule_info()`, now returns the number of nodes by type and handles 
  more complex module–expression combinations (#70).

## Bug fixes

* Fixed a bug in `mc_compare()` when comparing a total mcnode with multiple 
  `data_names` (#71).

* Fixed an `add_prefix()` bug where totals node names were not correctly 
  prefixed (#68).

* Fixed a bug in `eval_module()` when `package::function()` nomenclature was used 
  within expressions (#32).

* Renamed `filter_suffix` to `suffix` (#77).

## Documentation

* Expanded the main package vignette, including sensitivity analysis examples (#49, #70).

* Added  sensitivity analysis vignette (#49, #70).

* Expanded `mc_compare()` documentation with `align_uncertainty` explanation (#65).

* Updated the package citation and README details (#57, #69).

# mcmodule 1.2.0

## Breaking changes

* `create_mcnodes()` no longer automatically calculates `rpert()` mode when not
  provided. This was not widely used and lacked transparency.

## New features

* New `mc_filter()` filters mcnodes and metadata within mcmodules or using
  mcnode and data (#53).

* New `mc_plot()` visualises Monte Carlo results with support for filters and
  color mapping. This function is experimental and may change in future
  versions (#4).

* New `mcmodule_info()` provides comprehensive information about mcmodule
  structure (#43).

* New `mcmodule_corr()` calculates correlation matrices for mcmodule variates
  (#48).

* New `mcmodule_converg()` assesses convergence of Monte Carlo simulations
  (#50).

* New `mcmodule_to_matrices()` converts mcmodules to matrix format (#3).

* New `mcmodule_to_mc()` adapts mcmodules to mc2d mc objects with optional
  aggregation of variates (#3).

* `eval_module()` now supports `mcstoc()` and `mcdata()` calls directly within
  expressions, with proper `nvariates` handling (#32, #42).

* New `which_mcnode()` function, with specific wrapers for NA (`which_mcnode_na()`) 
  and Inf ()`which_mcnode_inf())` detection (#35).

## Minor improvements and bug fixes

* `add_prefix()` no longer has issues when mcmodule uses pipes (#51).

* `add_prefix()` and `combine_modules()` now handle absent module metadata in
  nodes.

* `create_mcnodes()` now handles argument ordering and issues warnings for 
  multiple column matches (#34).

* `eval_module()` improves handling of mcnode creation from data when nodes are
  not in mctable or prev_mcmodule (#32).

* `get_node_list()` now uses custom AST traversal parser and improves mcnode 
  ordering for better network visualization(#39, #40).

* `mc_network()` fixes bug related to unprefixed inputs in trial_totals output nodes.

* `trial_totals()` now combines keys from all inputs, not just the main
  mc_name.

* `eval_module()`and `set_mctable()`now can use `sensi_baseline` and `sensi_variation` 
  to perfrom One-At-a-Time sensitivity analysis. This will be further developed in future
  releases (#49). 

* Not exported functions `mc_summary_keys()` and `node_list_summary()` have been
removed.

* The distinction between "exp" and "module" terminology has been clarified throughout the package.

* Function documentation has been extended and harmonised.

* Documentation updated to clarify use of `mcstoc()` and `mcdata()` within
  expressions in `eval_module()` (#32, #42).

* Vignettes updated to document new analysis functions and features (#3, #4,
  #32, #48, #49, #50).

* Website links updated to <https://nataliaciria.com/mcmodule/>.

# mcmodule 1.1.1

* `eval_module()` gains `keys` and `overwrite_keys` arguments to add keys
  that aren't in `data_keys` or replace existing keys (#23).

* `keys_match()` now returns early when keys already match, improving
  performance and fixing occasional bugs (#28).

* Core functions (`eval_module()`, `trial_totals()`, `dim_match()`,
  `at_least_one()`, `mc_match()`, `create_mcnodes()`, `get_node_list()`)
  now support mcnodes with multiple data names, with clear messages
  indicating defaults (#19).

* `create_mcnodes()` and `eval_module()` provide clearer error messages
  for invalid or missing data (#18).

* `mc_match()` and `mc_match_data()` include improved scenario baseline
  checks and error messages.

# mcmodule 1.1.0

* Re-submission to CRAN. Removed unexported function examples.

* `eval_module()` gains `match_keys` parameter for flexible data-mcnode
  matching.

* `eval_module()` now supports multiple `prev_mcmodule` data names.

* `eval_module()` improves R function handling within mcmodule expressions.

* `trial_totals()` now supports `agg_suffix` skipping.

* `at_least_one()` improves agg_keys combination.

* Fixed missing `agg_suffix` handling.

* Fixed dimension matching for aggregated nodes.

* Fixed prefix issues in combined probability nodes.

* NA removal added in key columns.

* Added tests for totals custom names and various dimension matching scenarios.

# mcmodule 1.0.1

* Re-submission to CRAN. Removed unexported function examples, replaced
  `dontrun` with `donttest`, added vignette link to DESCRIPTION, and included
  citation file.

# mcmodule 1.0.0

* Initial CRAN submission.
