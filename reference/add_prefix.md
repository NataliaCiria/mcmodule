# Add Prefix to Node Names

Adds a prefix to node_list names and all input nodes. Preserves previous
prefixes unless rewrite_module argument is specified.

## Usage

``` r
add_prefix(mcmodule, prefix = NULL, rewrite_module = NULL)
```

## Arguments

- mcmodule:

  An mcmodule or a node_list object

- prefix:

  String to add as prefix of the new mcmodule mcnodes, defaults to
  mcmodule name

- rewrite_module:

  Name of a module to rewrite prefixes

## Value

A mcmodule with new prefixes in node_list names

## Examples

``` r
print(names(imports_mcmodule$node_list))
#> [1] "w_prev"      "test_origin" "test_sensi"  "inf_a"       "false_neg_a"
#> [6] "no_test_a"   "no_detect_a"
imports_mcmodule_prefix<-purchase <- add_prefix(imports_mcmodule)
print(names(imports_mcmodule_prefix$node_list))
#> [1] "imports_mcmodule_w_prev"      "imports_mcmodule_test_origin"
#> [3] "imports_mcmodule_test_sensi"  "imports_mcmodule_inf_a"      
#> [5] "imports_mcmodule_false_neg_a" "imports_mcmodule_no_test_a"  
#> [7] "imports_mcmodule_no_detect_a"
```
