# Evaluate a Monte Carlo Model Expression and create an mcmodule

Takes a set of Monte Carlo model expressions and evaluates them and
creates an mcmodule containing results and metadata.

## Usage

``` r
eval_module(
  exp,
  data,
  param_names = NULL,
  prev_mcmodule = NULL,
  summary = FALSE,
  mctable = set_mctable(),
  data_keys = set_data_keys(),
  match_keys = NULL,
  keys = NULL,
  overwrite_keys = NULL
)
```

## Arguments

- exp:

  Model expression or list of expressions to evaluate

- data:

  Input data frame containing model parameters

- param_names:

  Named vector for parameter renaming (optional)

- prev_mcmodule:

  Previous module(s) for dependent calculations

- summary:

  Logical; whether to calculate summary statistics

- mctable:

  Reference table for mcnodes, defaults to set_mctable()

- data_keys:

  Data structure and keys, defaults to set_data_keys()

- match_keys:

  Keys to match prev_mcmodule mcnodes and data by

- keys:

  Optional explicit keys for the input data (character vector)

- overwrite_keys:

  Logical or NULL. If NULL (default) it becomes TRUE when data_keys is
  NULL or an empty list; otherwise FALSE.

## Value

An mcmodule object containing data, expressions, and nodes

## Examples

``` r
# Basic usage with single expression
eval_module(
  exp = imports_exp,
  data = imports_data,
  mctable = imports_mctable,
  data_keys = imports_data_keys
)
#> imports evaluated
#> mcmodule created (expressions: )
#> $data
#> $data$imports_data
#>   pathogen origin h_prev_min h_prev_max w_prev_min w_prev_max farms_n
#> 1        a   nord       0.08       0.10       0.15        0.2       5
#> 2        a  south       0.02       0.05       0.15        0.2      10
#> 3        a   east       0.10       0.15       0.15        0.2       7
#> 4        b   nord       0.50       0.70       0.45        0.6       5
#> 5        b  south       0.25       0.30       0.37        0.4      10
#> 6        b   east       0.30       0.50       0.45        0.6       7
#>   animals_n_mean animals_n_sd test_origin test_sensi_min test_sensi_mode
#> 1            100            6   sometimes           0.89            0.90
#> 2            130           10   sometimes           0.89            0.90
#> 3            140           12       never           0.89            0.90
#> 4            100            2      always           0.80            0.85
#> 5            130            4   sometimes           0.80            0.85
#> 6            140            3     unknown           0.80            0.85
#>   test_sensi_max
#> 1           0.91
#> 2           0.91
#> 3           0.91
#> 4           0.90
#> 5           0.90
#> 6           0.90
#> 
#> 
#> $exp
#> {
#>     inf_a <- w_prev
#>     false_neg_a <- inf_a * test_origin * (1 - test_sensi)
#>     no_test_a <- inf_a * (1 - test_origin)
#>     no_detect_a <- false_neg_a + no_test_a
#> }
#> 
#> $node_list
#> $node_list$w_prev
#> $node_list$w_prev$type
#> [1] "in_node"
#> 
#> $node_list$w_prev$mc_func
#> [1] "runif"
#> 
#> $node_list$w_prev$description
#> [1] "Within herd prevalence"
#> 
#> $node_list$w_prev$inputs_col
#> [1] "w_prev_min" "w_prev_max"
#> 
#> $node_list$w_prev$input_dataset
#> [1] "prevalence_region"
#> 
#> $node_list$w_prev$keys
#> [1] "pathogen" "origin"  
#> 
#> $node_list$w_prev$module
#> [1] "imports"
#> 
#> $node_list$w_prev$mc_name
#> [1] "w_prev"
#> 
#> $node_list$w_prev$mcnode
#>   node    mode  nsv nsu nva variate  min  mean median max Nas type outm
#> 1    x numeric 1001   1   6       1 0.15 0.176  0.176 0.2   0    V each
#> 2    x numeric 1001   1   6       2 0.15 0.175  0.175 0.2   0    V each
#> 3    x numeric 1001   1   6       3 0.15 0.176  0.176 0.2   0    V each
#> 4    x numeric 1001   1   6       4 0.45 0.525  0.524 0.6   0    V each
#> 5    x numeric 1001   1   6       5 0.37 0.385  0.385 0.4   0    V each
#> 6    x numeric 1001   1   6       6 0.45 0.525  0.523 0.6   0    V each
#> 
#> $node_list$w_prev$data_name
#> [1] "imports_data"
#> 
#> 
#> $node_list$test_origin
#> $node_list$test_origin$type
#> [1] "in_node"
#> 
#> $node_list$test_origin$description
#> [1] "Probability of the animals being tested in origin"
#> 
#> $node_list$test_origin$inputs_col
#> [1] "test_origin"
#> 
#> $node_list$test_origin$input_dataset
#> [1] "prevalence_region"
#> 
#> $node_list$test_origin$keys
#> [1] "pathogen" "origin"  
#> 
#> $node_list$test_origin$module
#> [1] "imports"
#> 
#> $node_list$test_origin$mc_name
#> [1] "test_origin"
#> 
#> $node_list$test_origin$mcnode
#>   node    mode nsv nsu nva variate min mean median max Nas type outm
#> 1    x numeric   1   1   6       1 0.5  0.5    0.5 0.5   0    0 each
#> 2    x numeric   1   1   6       2 0.5  0.5    0.5 0.5   0    0 each
#> 3    x numeric   1   1   6       3 0.0  0.0    0.0 0.0   0    0 each
#> 4    x numeric   1   1   6       4 1.0  1.0    1.0 1.0   0    0 each
#> 5    x numeric   1   1   6       5 0.5  0.5    0.5 0.5   0    0 each
#> 6    x numeric   1   1   6       6 0.0  0.0    0.0 0.0   0    0 each
#> 
#> $node_list$test_origin$data_name
#> [1] "imports_data"
#> 
#> 
#> $node_list$test_sensi
#> $node_list$test_sensi$type
#> [1] "in_node"
#> 
#> $node_list$test_sensi$mc_func
#> [1] "rpert"
#> 
#> $node_list$test_sensi$description
#> [1] "Test sensitivity"
#> 
#> $node_list$test_sensi$inputs_col
#> [1] "test_sensi_min"  "test_sensi_mode" "test_sensi_max" 
#> 
#> $node_list$test_sensi$input_dataset
#> [1] "test_sensitivity"
#> 
#> $node_list$test_sensi$keys
#> [1] "pathogen"
#> 
#> $node_list$test_sensi$module
#> [1] "imports"
#> 
#> $node_list$test_sensi$mc_name
#> [1] "test_sensi"
#> 
#> $node_list$test_sensi$mcnode
#>   node    mode  nsv nsu nva variate   min  mean median   max Nas type outm
#> 1    x numeric 1001   1   6       1 0.890 0.900  0.900 0.908   0    V each
#> 2    x numeric 1001   1   6       2 0.891 0.900  0.900 0.909   0    V each
#> 3    x numeric 1001   1   6       3 0.891 0.900  0.900 0.910   0    V each
#> 4    x numeric 1001   1   6       4 0.805 0.850  0.850 0.897   0    V each
#> 5    x numeric 1001   1   6       5 0.801 0.850  0.850 0.893   0    V each
#> 6    x numeric 1001   1   6       6 0.802 0.851  0.851 0.896   0    V each
#> 
#> $node_list$test_sensi$data_name
#> [1] "imports_data"
#> 
#> 
#> $node_list$inf_a
#> $node_list$inf_a$node_exp
#> [1] "w_prev"
#> 
#> $node_list$inf_a$type
#> [1] "out_node"
#> 
#> $node_list$inf_a$inputs
#> [1] "w_prev"
#> 
#> $node_list$inf_a$module
#> [1] "imports"
#> 
#> $node_list$inf_a$mc_name
#> [1] "inf_a"
#> 
#> $node_list$inf_a$keys
#> [1] "pathogen" "origin"  
#> 
#> $node_list$inf_a$param
#> [1] "w_prev"
#> 
#> $node_list$inf_a$mcnode
#>   node    mode  nsv nsu nva variate  min  mean median max Nas type outm
#> 1    x numeric 1001   1   6       1 0.15 0.176  0.176 0.2   0    V each
#> 2    x numeric 1001   1   6       2 0.15 0.175  0.175 0.2   0    V each
#> 3    x numeric 1001   1   6       3 0.15 0.176  0.176 0.2   0    V each
#> 4    x numeric 1001   1   6       4 0.45 0.525  0.524 0.6   0    V each
#> 5    x numeric 1001   1   6       5 0.37 0.385  0.385 0.4   0    V each
#> 6    x numeric 1001   1   6       6 0.45 0.525  0.523 0.6   0    V each
#> 
#> $node_list$inf_a$data_name
#> [1] "imports_data"
#> 
#> 
#> $node_list$false_neg_a
#> $node_list$false_neg_a$node_exp
#> [1] "inf_a * test_origin * (1 - test_sensi)"
#> 
#> $node_list$false_neg_a$type
#> [1] "out_node"
#> 
#> $node_list$false_neg_a$inputs
#> [1] "inf_a"       "test_origin" "test_sensi" 
#> 
#> $node_list$false_neg_a$module
#> [1] "imports"
#> 
#> $node_list$false_neg_a$mc_name
#> [1] "false_neg_a"
#> 
#> $node_list$false_neg_a$keys
#> [1] "pathogen" "origin"  
#> 
#> $node_list$false_neg_a$param
#> [1] "inf_a"       "test_origin" "test_sensi" 
#> 
#> $node_list$false_neg_a$mcnode
#>   node    mode  nsv nsu nva variate     min    mean  median    max Nas type
#> 1    x numeric 1001   1   6       1 0.00701 0.00878 0.00878 0.0108   0    V
#> 2    x numeric 1001   1   6       2 0.00689 0.00874 0.00872 0.0107   0    V
#> 3    x numeric 1001   1   6       3 0.00000 0.00000 0.00000 0.0000   0    V
#> 4    x numeric 1001   1   6       4 0.04822 0.07862 0.07796 0.1134   0    V
#> 5    x numeric 1001   1   6       5 0.01989 0.02887 0.02896 0.0387   0    V
#> 6    x numeric 1001   1   6       6 0.00000 0.00000 0.00000 0.0000   0    V
#>   outm
#> 1 each
#> 2 each
#> 3 each
#> 4 each
#> 5 each
#> 6 each
#> 
#> $node_list$false_neg_a$data_name
#> [1] "imports_data"
#> 
#> 
#> $node_list$no_test_a
#> $node_list$no_test_a$node_exp
#> [1] "inf_a * (1 - test_origin)"
#> 
#> $node_list$no_test_a$type
#> [1] "out_node"
#> 
#> $node_list$no_test_a$inputs
#> [1] "inf_a"       "test_origin"
#> 
#> $node_list$no_test_a$module
#> [1] "imports"
#> 
#> $node_list$no_test_a$mc_name
#> [1] "no_test_a"
#> 
#> $node_list$no_test_a$keys
#> [1] "pathogen" "origin"  
#> 
#> $node_list$no_test_a$param
#> [1] "inf_a"       "test_origin"
#> 
#> $node_list$no_test_a$mcnode
#>   node    mode  nsv nsu nva variate   min   mean median max Nas type outm
#> 1    x numeric 1001   1   6       1 0.075 0.0878 0.0879 0.1   0    V each
#> 2    x numeric 1001   1   6       2 0.075 0.0876 0.0873 0.1   0    V each
#> 3    x numeric 1001   1   6       3 0.150 0.1755 0.1764 0.2   0    V each
#> 4    x numeric 1001   1   6       4 0.000 0.0000 0.0000 0.0   0    V each
#> 5    x numeric 1001   1   6       5 0.185 0.1924 0.1925 0.2   0    V each
#> 6    x numeric 1001   1   6       6 0.450 0.5250 0.5232 0.6   0    V each
#> 
#> $node_list$no_test_a$data_name
#> [1] "imports_data"
#> 
#> 
#> $node_list$no_detect_a
#> $node_list$no_detect_a$node_exp
#> [1] "false_neg_a + no_test_a"
#> 
#> $node_list$no_detect_a$type
#> [1] "out_node"
#> 
#> $node_list$no_detect_a$inputs
#> [1] "false_neg_a" "no_test_a"  
#> 
#> $node_list$no_detect_a$module
#> [1] "imports"
#> 
#> $node_list$no_detect_a$mc_name
#> [1] "no_detect_a"
#> 
#> $node_list$no_detect_a$keys
#> [1] "pathogen" "origin"  
#> 
#> $node_list$no_detect_a$param
#> [1] "false_neg_a" "no_test_a"  
#> 
#> $node_list$no_detect_a$mcnode
#>   node    mode  nsv nsu nva variate    min   mean median   max Nas type outm
#> 1    x numeric 1001   1   6       1 0.0822 0.0966 0.0968 0.111   0    V each
#> 2    x numeric 1001   1   6       2 0.0822 0.0963 0.0960 0.111   0    V each
#> 3    x numeric 1001   1   6       3 0.1501 0.1755 0.1764 0.200   0    V each
#> 4    x numeric 1001   1   6       4 0.0482 0.0786 0.0780 0.113   0    V each
#> 5    x numeric 1001   1   6       5 0.2063 0.2213 0.2213 0.237   0    V each
#> 6    x numeric 1001   1   6       6 0.4501 0.5250 0.5232 0.600   0    V each
#> 
#> $node_list$no_detect_a$data_name
#> [1] "imports_data"
#> 
#> 
#> 
#> $modules
#> [1] "imports"
#> 
#> attr(,"class")
#> [1] "mcmodule"
```
