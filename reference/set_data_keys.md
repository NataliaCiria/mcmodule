# Set or Get Global Data Keys

Manages a global data model by either setting new data keys or
retrieving the current ones. The data model consists of named lists
containing column names and their associated key columns.

## Usage

``` r
set_data_keys(data_keys = NULL)
```

## Arguments

- data_keys:

  Optional list of lists. Each inner list must contain:

  - cols: A vector containing the column names for the data

  - keys: A vector specifying the key columns

  If NULL, returns the current data model.

## Value

- If data_keys = NULL: Returns the current global data model

- If data_keys provided: Sets the new data model and returns invisibly

## Examples

``` r
print(imports_data_keys)
#> $test_sensitivity
#> $test_sensitivity$cols
#> [1] "pathogen"        "test_sensi_min"  "test_sensi_mode" "test_sensi_max" 
#> 
#> $test_sensitivity$keys
#> [1] "pathogen"
#> 
#> 
#> $animal_imports
#> $animal_imports$cols
#> [1] "origin"         "farms_n"        "animals_n_mean" "animals_n_sd"  
#> 
#> $animal_imports$keys
#> [1] "origin"
#> 
#> 
#> $prevalence_region
#> $prevalence_region$cols
#> [1] "pathogen"    "origin"      "h_prev_min"  "h_prev_max"  "w_prev_min" 
#> [6] "w_prev_max"  "test_origin"
#> 
#> $prevalence_region$keys
#> [1] "pathogen" "origin"  
#> 
#> 
set_data_keys(imports_data_keys)
#> data_keys set to imports_data_keys
```
