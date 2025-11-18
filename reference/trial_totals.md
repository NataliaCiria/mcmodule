# Trial Probability and Expected Counts

Calculates probabilities and expected counts across hierarchical levels
(trial, subset, set) in a structured population. Uses trial
probabilities and handles nested sampling with conditional
probabilities.

## Usage

``` r
trial_totals(
  mcmodule,
  mc_names,
  trials_n,
  subsets_n = NULL,
  subsets_p = NULL,
  name = NULL,
  prefix = NULL,
  combine_prob = TRUE,
  all_suffix = NULL,
  level_suffix = c(trial = "trial", subset = "subset", set = "set"),
  mctable = set_mctable(),
  agg_keys = NULL,
  agg_suffix = NULL,
  keep_variates = FALSE,
  summary = TRUE,
  data_name = NULL
)
```

## Arguments

- mcmodule:

  mcmodule object containing input data and node structure

- mc_names:

  Vector of node names to process

- trials_n:

  Trial count column name

- subsets_n:

  Subset count column name (optional)

- subsets_p:

  Subset prevalence column name (optional)

- name:

  Custom name for output nodes (optional)

- prefix:

  Prefix for output node names (optional)

- combine_prob:

  Combine probability of all nodes assuming independence (default: TRUE)

- all_suffix:

  Suffix for combined node name (default: "all")

- level_suffix:

  A list of suffixes for each hierarchical level (default:
  c(trial="trial",subset="subset",set="set"))

- mctable:

  Data frame containing Monte Carlo nodes definitions (default:
  set_mctable())

- agg_keys:

  Column names for aggregation (optional)

- agg_suffix:

  Suffix for aggregated node names (default: "hag")

- keep_variates:

  whether to preserve individual values (default: FALSE)

- summary:

  Include summary statistics if TRUE (default: TRUE)

- data_name:

  Data name used to create trials_n, subsets_n and subsets_p nodes if
  they don't exist in mcmodule (optional)

## Value

Updated mcmodule object containing:

- Combined node probabilities

- Probabilities and counts at trial level

- Probabilities and counts at subset level

- Probabilities and counts at set level

## Examples

``` r
imports_mcmodule <- trial_totals(
  mcmodule = imports_mcmodule,
  mc_names = "no_detect_a",
  trials_n = "animals_n",
  subsets_n = "farms_n",
  subsets_p = "h_prev",
  mctable = imports_mctable
)
print(imports_mcmodule$node_list$no_detect_a_set$summary)
#>           mc_name pathogen origin      mean          sd       Min      2.5%
#> 1 no_detect_a_set        a   nord 0.3764715 0.020323487 0.3411145 0.3427513
#> 2 no_detect_a_set        a  south 0.2982527 0.062479652 0.1830303 0.1913597
#> 3 no_detect_a_set        a   east 0.6035354 0.046265356 0.5219547 0.5262205
#> 4 no_detect_a_set        b   nord 0.9875522 0.008374908 0.9687340 0.9701970
#> 5 no_detect_a_set        b  south 0.9587261 0.008070145 0.9437208 0.9445872
#> 6 no_detect_a_set        b   east 0.9664391 0.020728570 0.9181797 0.9221154
#>         25%       50%       75%     97.5%       Max  nsv Na's
#> 1 0.3583955 0.3767396 0.3945887 0.4078370 0.4095042 1001    0
#> 2 0.2463023 0.2981711 0.3519906 0.3966782 0.4012021 1001    0
#> 3 0.5640380 0.6049379 0.6449311 0.6758179 0.6794182 1001    0
#> 4 0.9815032 0.9899148 0.9947579 0.9973527 0.9975547 1001    0
#> 5 0.9519542 0.9589960 0.9658775 0.9711185 0.9717524 1001    0
#> 6 0.9504834 0.9716804 0.9840396 0.9916738 0.9920768 1001    0
```
